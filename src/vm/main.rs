use std::collections::{HashMap, HashSet};

// these need to be added
use crate::cpu::memory::{MemoryAdapter, RAM};
use crate::languages::base::{CompilationError, create_error, Span};
use crate::languages::vm::{
    CallInstruction, FunctionInstruction, Segment, StackInstruction, VmInstruction,
};
use crate::os::os::OS;
use crate::vm::builtins::VM_BUILTINS;
use crate::vm::memory::VmMemory;

// Values for frame region
#[derive(Clone, Debug)]
pub struct VmFrameValues {
    pub base: i32,
    pub count: usize,
    pub values: Vec<i32>,
}

#[derive(Clone, Debug)]
pub struct VmFrame {
    pub fn_def: Option<VmFunction>,
    pub locals: VmFrameValues,
    pub args: VmFrameValues,
    pub stack: VmFrameValues,
    pub this_: VmFrameValues,
    pub that: VmFrameValues,
    pub frame: FramePointers,
    pub used_segments: Option<HashSet<Segment>>,
}

#[derive(Clone, Debug)]
pub struct FramePointers {
    pub ret: i32,
    pub arg: i32,
    pub lcl: i32,
    pub this: i32,
    pub that: i32,
}

pub type VmFunctions = HashMap<String, VmFunction>;

#[derive(Clone, Debug)]
pub struct VmFunction {
    pub name: String,
    pub n_vars: usize,
    pub labels: HashMap<String, usize>,
    pub operations: Vec<VmInstruction>,
    pub op_base: usize,
}

#[derive(Clone, Debug)]
pub struct VmFunctionInvocation {
    pub function: String,
    // current operation offset in the function
    pub op_ptr: usize,
    // base address of frame in memory
    pub frame_base: i32,
    // number of args  function was called with
    pub n_args: usize,
    pub this_initialized: bool,
    pub that_initialized: bool,
    // size of the memory block pointed to by this (if is)
    pub this_n: Option<usize>,
}

pub const IMPLICIT: &str = "__implicit";

pub fn sys_init_fn() -> VmFunction {
    // construct the Sys.init analogue
    VmFunction {
        name: "Sys.init".to_string(),
        labels: HashMap::new(),
        n_vars: 0,
        op_base: 0,
        operations: vec![
            VmInstruction::Function {
                name: "Sys.init".to_string(),
                n_vars: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "Math.init".to_string(),
                n_args: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "String.init".to_string(),
                n_args: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "Array.init".to_string(),
                n_args: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "Output.init".to_string(),
                n_args: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "Screen.init".to_string(),
                n_args: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "Keyboard.init".to_string(),
                n_args: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "Memory.init".to_string(),
                n_args: 0,
                span: None,
            },
            VmInstruction::Call {
                name: "Main.main".to_string(),
                n_args: 0,
                span: None,
            },
        ],
    }
}

pub struct ParsedVmFile {
    pub name: String,
    pub instructions: Vec<VmInstruction>,
}

#[derive(Clone, Debug)]
struct SegmentStatus {
    initialized: bool,
    n: usize,
}

pub struct Vm {
    pub memory: VmMemory,
    os: OS,
    pub function_map: HashMap<String, VmFunction>,
    pub execution_stack: Vec<VmFunctionInvocation>,

    pub entry: String,

    segment_initializations: HashMap<&'static str, SegmentStatus>,

    pub functions: Vec<VmFunction>,
    pub program: Vec<VmInstruction>,
    pub added_sys_init: bool,

    static_count: usize,
    pub statics: HashMap<String, Vec<i32>>,

    return_line: Option<usize>,
}

impl Vm {
    pub fn new() -> Self {
        let memory = VmMemory::new();
        let os = OS::new(&memory);
        Vm {
            memory,
            os,
            function_map: HashMap::new(),
            execution_stack: Vec::new(),
            entry: String::new(),
            segment_initializations: {
                let mut m = HashMap::new();
                m.insert(
                    "local",
                    SegmentStatus {
                        initialized: false,
                        n: 0,
                    },
                );
                m.insert(
                    "argument",
                    SegmentStatus {
                        initialized: false,
                        n: 0,
                    },
                );
                m
            },
            functions: Vec::new(),
            program: Vec::new(),
            added_sys_init: false,
            static_count: 0,
            statics: HashMap::new(),
            return_line: None,
        }
    }

    pub fn get_static_count(&self) -> usize {
        self.static_count
    }

    fn register_static(&mut self, fn_name: &str, offset: usize) -> i32 {
        // file basename (before dot or path)
        let file_name = Self::file_basename_no_extension(fn_name);
        let statics = self.statics.entry(file_name.clone()).or_insert_with(Vec::new);
        if offset >= statics.len() {
            // make sure has capacity
            statics.resize(offset + 1, -1);
        }
        if statics[offset] != -1 {
            statics[offset]
        } else {
            let id = self.static_count as i32;
            self.static_count += 1;
            statics[offset] = id;
            id
        }
    }

    fn register_statics(&mut self) {
        for fn_def in self.function_map.values_mut() {
            for op in &mut fn_def.operations {
                match op {
                    VmInstruction::Push {
                        segment,
                        offset,
                        ..
                    }
                    | VmInstruction::Pop {
                        segment,
                        offset,
                        ..
                    } => {
                        if *segment == Segment::Static {
                            // register static and replace offset
                            let new_offset = self.register_static(&fn_def.name, *offset as usize);
                            *offset = new_offset as usize;
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn file_basename_no_extension(s: &str) -> String {
        // remove path
        let mut file = s.to_string();
        if file.contains('/') {
            if let Some(pos) = file.rfind('/') {
                file = file[pos + 1..].to_string();
            }
        }
        if let Some(pos) = file.find('.') {
            file = file[..pos].to_string();
        }
        file
    }

    fn validate_file(file: &ParsedVmFile) -> Result<(), CompilationError> {
        for inst in &file.instructions {
            if let VmInstruction::Function { name, span, .. } = inst {
                let parts: Vec<&str> = name.split('.').collect();
                if parts.len() != 2 {
                    return Err(create_error(
                        &format!(
                            "Illegal subroutine name {} (Expected <className>.<SubroutineName>)",
                            name
                        ),
                        span.clone(),
                    ));
                }
                if parts[0] != Self::file_basename_no_extension(&file.name) {
                    return Err(create_error(
                        &format!(
                            "File name {} doesn't match class name {} (at {})",
                            file.name, parts[0], name
                        ),
                        span.clone(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn validate_files(files: &[ParsedVmFile]) -> Result<(), CompilationError> {
        let mut names = HashSet::new();
        for file in files {
            if names.contains(&file.name) {
                return Err(create_error(&format!("File {} already exists", file.name), None));
            }
            Self::validate_file(file)?;
            names.insert(file.name.clone());
        }
        Ok(())
    }

    fn validate_stack_instructions(&self) -> Result<(), CompilationError> {
        for fn_def in self.function_map.values() {
            for inst in &fn_def.operations {
                match inst {
                    VmInstruction::Pop { segment, offset, span, .. }
                    | VmInstruction::Push { segment, offset, span, .. } => {
                        match self.memory.base_segment(*segment, *offset as i32) {
                            Ok(_) => {}
                            Err(msg) => {
                                return Err(create_error(&msg, span.clone()));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn validate_functions(
        instructions: &[VmInstruction],
    ) -> Result<(), CompilationError> {
        let mut functions = HashSet::new();
        let mut calls: Vec<CallInstruction> = Vec::new();

        for inst in instructions {
            match inst {
                VmInstruction::Function { name, n_vars, span } => {
                    if *n_vars as i32 < 0 || *n_vars > 32767 {
                        return Err(create_error(
                            &format!("Illegal number of local variables {} (Expected 0-32767)", n_vars),
                            span.clone(),
                        ));
                    }
                    functions.insert(name.clone());
                }
                VmInstruction::Call { name, n_args, span } => {
                    if *n_args as i32 < 0 || *n_args > 32767 {
                        return Err(create_error(
                            &format!("Illegal number of arguments {} (Expected 0-32767)", n_args),
                            span.clone(),
                        ));
                    }
                    // convert to CallInstruction (should have ame shape)
                    calls.push(CallInstruction {
                        name: name.clone(),
                        n_args: *n_args,
                        span: span.clone(),
                    });
                }
                _ => {}
            }
        }

        for call in calls {
            if !functions.contains(&call.name) {
                if let Some(builtin) = VM_BUILTINS.get(&call.name) {
                    let expected_n_args = if builtin.kind == "method" {
                        builtin.args.len() + 1
                    } else {
                        builtin.args.len()
                    };
                    if expected_n_args != call.n_args {
                        return Err(create_error(
                            &format!("OS function {} expects {} arguments, not {}", call.name, expected_n_args, call.n_args),
                            call.span.clone(),
                        ));
                    }
                } else {
                    return Err(create_error(&format!("Undefined function {}", call.name), call.span.clone()));
                }
            }
        }

        Ok(())
    }

    pub fn build_from_files(files: Vec<ParsedVmFile>) -> Result<Self, CompilationError> {
        Self::validate_files(&files)?;
        let mut instructions: Vec<VmInstruction> = Vec::new();
        for f in files {
            instructions.extend(f.instructions.into_iter());
        }
        Self::validate_functions(&instructions)?;
        let mut vm = Vm::new();
        vm.load(instructions, false)?;
        vm.bootstrap()?;
        Ok(vm)
    }

    pub fn build(instructions: Vec<VmInstruction>) -> Result<Self, CompilationError> {
        Self::validate_functions(&instructions)?;
        let mut vm = Vm::new();
        vm.load(instructions, false)?;
        vm.bootstrap()?;
        Ok(vm)
    }

    fn build_function(
        instructions: &[VmInstruction],
        mut i: usize,
    ) -> Result<(VmFunction, usize), CompilationError> {
        // must start at a Function
        match &instructions[i] {
            VmInstruction::Function { name, n_vars, span } => {
                let mut fn_def = VmFunction {
                    name: name.clone(),
                    n_vars: *n_vars as usize,
                    labels: HashMap::new(),
                    operations: vec![instructions[i].clone()],
                    op_base: 0,
                };

                let mut declared_labels = HashSet::new();
                let mut used_labels: HashMap<String, Option<Span>> = HashMap::new();

                i += 1;
                while i < instructions.len() {
                    match &instructions[i] {
                        VmInstruction::Function { .. } => break,
                        VmInstruction::Add { span } => {
                            fn_def.operations.push(VmInstruction::Add { span: span.clone() });
                        }
                        VmInstruction::Sub { span } => {
                            fn_def.operations.push(VmInstruction::Sub { span: span.clone() });
                        }
                        VmInstruction::Neg { span } => {
                            fn_def.operations.push(VmInstruction::Neg { span: span.clone() });
                        }
                        VmInstruction::And { span } => {
                            fn_def.operations.push(VmInstruction::And { span: span.clone() });
                        }
                        VmInstruction::Or { span } => {
                            fn_def.operations.push(VmInstruction::Or { span: span.clone() });
                        }
                        VmInstruction::Not { span } => {
                            fn_def.operations.push(VmInstruction::Not { span: span.clone() });
                        }
                        VmInstruction::Gt { span } => {
                            fn_def.operations.push(VmInstruction::Gt { span: span.clone() });
                        }
                        VmInstruction::Lt { span } => {
                            fn_def.operations.push(VmInstruction::Lt { span: span.clone() });
                        }
                        VmInstruction::Eq { span } => {
                            fn_def.operations.push(VmInstruction::Eq { span: span.clone() });
                        }
                        VmInstruction::Push { segment, offset, span } => {
                            fn_def.operations.push(VmInstruction::Push {
                                segment: *segment,
                                offset: *offset,
                                span: span.clone(),
                            });
                        }
                        VmInstruction::Pop { segment, offset, span } => {
                            fn_def.operations.push(VmInstruction::Pop {
                                segment: *segment,
                                offset: *offset,
                                span: span.clone(),
                            });
                        }
                        VmInstruction::Call { name, n_args, span } => {
                            fn_def.operations.push(VmInstruction::Call {
                                name: name.clone(),
                                n_args: *n_args,
                                span: span.clone(),
                            });
                        }
                        VmInstruction::Goto { label, span } => {
                            used_labels.insert(label.clone(), span.clone());
                            fn_def.operations.push(VmInstruction::Goto {
                                label: label.clone(),
                                span: span.clone(),
                            });
                        }
                        VmInstruction::IfGoto { label, span } => {
                            used_labels.insert(label.clone(), span.clone());
                            fn_def.operations.push(VmInstruction::IfGoto {
                                label: label.clone(),
                                span: span.clone(),
                            });
                        }
                        VmInstruction::Label { label, span } => {
                            let label_name = label.clone();
                            if fn_def.labels.contains_key(&label_name) {
                                return Err(create_error(
                                    &format!(
                                        "Cannot redeclare label {} in function {} (previously at line {})",
                                        label_name,
                                        fn_def.name,
                                        fn_def.labels[&label_name] + 1
                                    ),
                                    span.clone(),
                                ));
                            }
                            fn_def.labels.insert(label_name.clone(), fn_def.operations.len());
                            declared_labels.insert(label_name.clone());
                            fn_def.operations.push(VmInstruction::Label {
                                label: label_name,
                                span: span.clone(),
                            });
                        }
                        VmInstruction::Return { span } => {
                            fn_def.operations.push(VmInstruction::Return { span: span.clone() });
                        }
                        other => {
                            // unknown instruction
                            fn_def.operations.push(other.clone());
                        }
                    }
                    i += 1;
                }

                for label in used_labels.keys() {
                    if !declared_labels.contains(label) {
                        return Err(create_error(&format!("Undeclared label {}", label), None));
                    }
                }

                Ok((fn_def, i))
            }
            _ => panic!("Only call build_function at the initial Function instruction"),
        }
    }

    pub fn ram(&self) -> &RAM {
        &self.memory
    }

    pub fn keyboard(&self) -> &MemoryAdapter {
        &self.memory.keyboard
    }

    pub fn screen(&self) -> &MemoryAdapter {
        &self.memory.screen
    }

    pub fn invocation(&self) -> VmFunctionInvocation {
        if let Some(inv) = self.execution_stack.last() {
            inv.clone()
        } else {
            VmFunctionInvocation {
                function: IMPLICIT.to_string(),
                op_ptr: 0,
                frame_base: 256,
                n_args: 0,
                this_initialized: false,
                that_initialized: false,
                this_n: None,
            }
        }
    }

    pub fn current_function(&self) -> Option<&VmFunction> {
        self.function_map.get(&self.invocation().function)
    }

    pub fn current_operation(&self) -> Option<VmInstruction> {
        if let Some(fn_def) = self.current_function() {
            let inv = self.execution_stack.last().unwrap_or(&VmFunctionInvocation {
                function: IMPLICIT.to_string(),
                op_ptr: 0,
                frame_base: 256,
                n_args: 0,
                this_initialized: false,
                that_initialized: false,
                this_n: None,
            });
            if inv.op_ptr > fn_def.operations.len() {
                panic!("Current operation step beyond end of function operations ({} > {})", inv.op_ptr, fn_def.operations.len());
            }
            fn_def.operations.get(inv.op_ptr).cloned()
        } else {
            None
        }
    }

    pub fn load(
        &mut self,
        mut instructions: Vec<VmInstruction>,
        reset: bool,
    ) -> Result<(), CompilationError> {
        if reset {
            self.function_map.clear();
            self.statics.clear();
            self.static_count = 0;
        }

        // make sure implicit function at front
        if instructions.get(0).map(|i| match i {
            VmInstruction::Function { .. } => true,
            _ => false,
        }) != Some(true)
        {
            instructions.insert(0, VmInstruction::Function {
                name: IMPLICIT.to_string(),
                n_vars: 0,
                span: None,
            });
        }

        let mut i = 0usize;
        while i < instructions.len() {
            let (fn_def, next_i) = Self::build_function(&instructions, i)?;
            if self.function_map.contains_key(&fn_def.name) && self.memory.strict && fn_def.name != IMPLICIT && fn_def.name != sys_init_fn().name {
                return Err(create_error(&format!("VM Already has a function named {}", fn_def.name), instructions[0].span()));
            }
            self.function_map.insert(fn_def.name.clone(), fn_def);
            i = next_i;
        }

        self.validate_stack_instructions()?;
        self.register_statics();

        if reset {
            self.bootstrap()?;
        }

        Ok(())
    }

    pub fn bootstrap(&mut self) -> Result<(), CompilationError> {
        if !self.function_map.contains_key(&sys_init_fn().name) && self.function_map.contains_key("Main.main") {
            self.function_map.insert(sys_init_fn().name.clone(), sys_init_fn());
            self.added_sys_init = true;
        }

        if self.function_map.contains_key(&sys_init_fn().name) {
            self.entry = sys_init_fn().name.clone();
        } else if self.function_map.contains_key(IMPLICIT) {
            self.entry = IMPLICIT.to_string();
        } else {
            let fn_names: Vec<String> = self.function_map.keys().cloned().collect();
            if fn_names.len() == 1 {
                self.entry = fn_names.into_iter().next().unwrap_or_default();
            }
        }

        if self.function_map.contains_key(IMPLICIT) && self.function_map.contains_key(&sys_init_fn().name) {
            return Err(create_error("Cannot use both bootstrap and an implicit function", None));
        }

        if self.entry.is_empty() {
            return Err(create_error("Could not determine an entry point for VM", None));
        }

        self.functions = self.function_map.values().cloned().collect();
        // place entry first
        self.functions.sort_by(|a, b| {
            if a.name == self.entry {
                std::cmp::Ordering::Less
            } else if b.name == self.entry {
                std::cmp::Ordering::Greater
            } else {
                std::cmp::Ordering::Equal
            }
        });

        let mut offset = 0usize;
        let mut program: Vec<VmInstruction> = Vec::new();
        for fn_def in &mut self.functions {
            if fn_def.name != sys_init_fn().name {
                fn_def.op_base = offset;
            }
            offset += fn_def.operations.len();
            program.extend(fn_def.operations.clone());
        }
        self.program = program;

        self.reset();

        Ok(())
    }

    pub fn reset(&mut self) {
        self.execution_stack = vec![VmFunctionInvocation {
            function: self.entry.clone(),
            op_ptr: 1,
            frame_base: 256,
            n_args: 0,
            this_initialized: false,
            that_initialized: false,
            this_n: None,
        }];
        self.memory.reset();
        self.memory.SP = 256;
        self.segment_initializations.insert("local", SegmentStatus { initialized: false, n: 0 });
        self.segment_initializations.insert("argument", SegmentStatus { initialized: false, n: 0 });

        self.os.dispose();
        self.os = OS::new(&self.memory);
    }

    fn validate_stack_op(&self, op: &StackInstruction) {
        let current_fn_name = self.invocation().function.clone();
        if let Some(current_fn) = self.function_map.get(&current_fn_name) {
            if current_fn.name == self.entry {
                for &segment in &["local", "argument"] {
                    if op.segment.as_str() == segment {
                        if self.segment_initializations[segment].initialized {
                            let n = op.offset as usize + 1;
                            let status = &self.segment_initializations[segment];
                            let new_n = std::cmp::max(n, status.n);
                            return;
                        } else {
                            // CHange this ot &mut self and return Result
                            panic!("The {} segment cannot be accessed since it was not initialized", segment);
                        }
                    }
                }
                if op.segment == Segment::This && self.execution_stack.last().map(|i| i.this_initialized).unwrap_or(false) {
                    // would update invocation.thisN
                    return;
                }
            }
        }

        // other checks
        if op.segment == Segment::Argument {
            if op.offset as usize >= self.execution_stack.last().map(|i| i.n_args).unwrap_or(0) {
                panic!("Argument offset out of bounds");
            }
        }
        if op.segment == Segment::Local {
            let fname = self.execution_stack.last().map(|i| i.function.clone()).unwrap_or_default();
            let nvars = self.function_map.get(&fname).map(|f| f.n_vars).unwrap_or(0);
            if op.offset as usize >= nvars {
                panic!("Local offset out of bounds");
            }
        }
        if op.segment == Segment::This && !self.execution_stack.last().map(|i| i.this_initialized).unwrap_or(false) {
            panic!("The this segment cannot be accessed since it was not initialized");
        }
        if op.segment == Segment::That && !self.execution_stack.last().map(|i| i.that_initialized).unwrap_or(false) {
            panic!("The that segment cannot be accessed since it was not initialized");
        }
    }

    pub fn set_paused(&mut self, paused: bool) {
        self.os.paused = paused;
    }

    // execute a single step, returns Some(exit_code) if halted with exit code,
    // none for normal continuation, or Err for compilation/runtime error
    pub fn step(&mut self) -> Option<i32> {
        if self.os.sys.halted {
            return Some(self.os.sys.exit_code);
        }
        if self.os.sys.blocked {
            return None;
        }

        if self.os.sys.released {
            if let Some(VmInstruction::Call { n_args, .. }) = self.current_operation() {
                // os released and previous op was call read return
                let ret = self.os.sys.read_return_value();
                let sp = self.memory.SP - n_args;
                self.memory.set(sp as i32, ret);
                self.memory.SP = sp + 1;
                if let Some(last) = self.execution_stack.last_mut() {
                    last.op_ptr += 1;
                }
                return None;
            }
        }

        if self.current_operation().is_none() {
            self.os.sys.halt();
            return self.step();
        }

        let operation = self.current_operation().unwrap();
        if let VmInstruction::Label { .. } = operation {
            if let Some(last) = self.execution_stack.last_mut() {
                last.op_ptr += 1;
            }
            return self.step();
        }

        match operation {
            VmInstruction::Push { segment, offset, .. } => {
                // validate and push
                let stack_inst = StackInstruction { segment, offset, span: None };
                self.validate_stack_op(&stack_inst);
                let value = self.memory.get_segment(segment, offset as i32);
                self.memory.push(value);
            }
            VmInstruction::Pop { segment, offset, .. } => {
                let stack_inst = StackInstruction { segment, offset, span: None };
                self.validate_stack_op(&stack_inst);
                let value = self.memory.pop();
                self.memory.set_segment(segment, offset as i32, value);

                // update this/that status
                if segment == Segment::Pointer {
                    if offset == 0 {
                        if let Some(last) = self.execution_stack.last_mut() {
                            last.this_initialized = true;
                            // thisN becomes memory.get(this - 1)
                            last.this_n = Some(self.memory.get(self.memory.this - 1) as usize);
                        }
                    } else if offset == 1 {
                        if let Some(last) = self.execution_stack.last_mut() {
                            last.that_initialized = true;
                        }
                    }
                }
            }
            VmInstruction::Add { .. } => {
                self.memory.bin_op(|a, b| a + b);
            }
            VmInstruction::Sub { .. } => {
                self.memory.bin_op(|a, b| a - b);
            }
            VmInstruction::Neg { .. } => {
                self.memory.un_op(|a| -a);
            }
            VmInstruction::And { .. } => {
                self.memory.bin_op(|a, b| a & b);
            }
            VmInstruction::Or { .. } => {
                self.memory.bin_op(|a, b| a | b);
            }
            VmInstruction::Not { .. } => {
                self.memory.un_op(|a| !a);
            }
            VmInstruction::Eq { .. } => {
                self.memory.comp(|a, b| a == b);
            }
            VmInstruction::Lt { .. } => {
                self.memory.comp(|a, b| a < b);
            }
            VmInstruction::Gt { .. } => {
                self.memory.comp(|a, b| a > b);
            }
            VmInstruction::Goto { label, .. } => {
                self.goto(&label);
            }
            VmInstruction::IfGoto { label, .. } => {
                let check = self.memory.pop();
                if check != 0 {
                    self.goto(&label);
                }
            }
            VmInstruction::Call { name, n_args, .. } => {
                let fn_name = name.clone();
                if self.function_map.contains_key(&fn_name) {
                    let base = self.memory.push_frame(
                        self.invocation().op_ptr as i32,
                        n_args as i32,
                        self.function_map[&fn_name].n_vars as i32,
                    );
                    self.execution_stack.push(VmFunctionInvocation {
                        function: fn_name,
                        op_ptr: 0,
                        n_args: n_args as usize,
                        frame_base: base,
                        this_initialized: false,
                        that_initialized: false,
                        this_n: None,
                    });
                } else if let Some(builtin) = VM_BUILTINS.get(&fn_name) {
                    let ret = (builtin.func)(&mut self.memory, &mut self.os);
                    if self.os.sys.blocked {
                        // handle on release
                        return None; 
                    }
                    let sp = self.memory.SP - n_args as i32;
                    self.memory.set(sp, ret);
                    self.memory.SP = sp + 1;
                }
            }
            VmInstruction::Return { .. } => {
                let line = self.derived_line();
                self.execution_stack.pop();
                let ret = self.memory.pop_frame();
                // ret is the saved opPtr
                if let Some(mut inv) = self.execution_stack.last_mut() {
                    inv.op_ptr = ret as usize;
                } else {
                    self.return_line = Some(line);
                    return Some(0);
                }
            }
            _ => {}
        }

        // advance opPtr
        if let Some(last) = self.execution_stack.last_mut() {
            last.op_ptr += 1;
        }
        None
    }

    fn goto(&mut self, label: &str) {
        if let Some(current_fn) = self.current_function() {
            if let Some(&idx) = current_fn.labels.get(label) {
                if let Some(last) = self.execution_stack.last_mut() {
                    last.op_ptr = idx;
                }
            } else {
                panic!("Attempting GOTO to unknown label {} in {}", label, current_fn.name);
            }
        }
    }

    pub fn write(&mut self, addresses: &[(i32, i32)]) {
        for (addr, val) in addresses {
            self.memory.set(*addr, *val);
        }
    }

    pub fn read(&self, addresses: &[i32]) -> Vec<i32> {
        addresses.iter().map(|a| self.memory.get(*a)).collect()
    }

    pub fn vm_stack(&self) -> Vec<VmFrame> {
        let mut frames = Vec::new();
        for (i, invocation) in self.execution_stack.iter().enumerate() {
            let next = self.execution_stack.get(i + 1);
            let end = if let Some(next_inv) = next {
                next_inv.frame_base - next_inv.n_args as i32
            } else {
                self.memory.get(0)
            };
            frames.push(self.make_frame(invocation.clone(), end));
        }
        frames
    }

    fn get_used_segments(&self, invocation: &VmFunctionInvocation) -> HashSet<Segment> {
        let mut used_segments = HashSet::new();
        if let Some(fn_def) = self.function_map.get(&invocation.function) {
            for inst in &fn_def.operations {
                match inst {
                    VmInstruction::Push { segment, .. } | VmInstruction::Pop { segment, .. } => {
                        used_segments.insert(*segment);
                    }
                    _ => {}
                }
            }
        }
        used_segments
    }

    pub fn make_frame(&self, invocation: VmFunctionInvocation, next_frame: i32) -> VmFrame {
        let fn_def = self.function_map.get(&invocation.function).cloned();
        if let Some(fn_def) = fn_def {
            if fn_def.name == self.entry {
                let stack_base = 256 + fn_def.n_vars as i32;
                let next_frame = self.execution_stack.get(1).cloned();
                let frame_end = if let Some(next) = next_frame {
                    next.frame_base - next.n_args as i32
                } else {
                    self.memory.get(0)
                };
                let arg = self.memory.arg;
                let lcl = self.memory.lcl;
                let that = self.memory.that;
                let this = self.memory.this;
                let n_arg = self.segment_initializations["argument"].n;
                let n_local = self.segment_initializations["local"].n;
                let n_this = self.execution_stack.last().and_then(|i| i.this_n).unwrap_or(0);

                VmFrame {
                    fn_def: Some(fn_def.clone()),
                    args: VmFrameValues {
                        base: arg,
                        count: n_arg,
                        values: self.memory.map(arg, arg + n_arg as i32),
                    },
                    locals: VmFrameValues {
                        base: lcl,
                        count: n_local,
                        values: self.memory.map(lcl, lcl + n_local as i32),
                    },
                    stack: VmFrameValues {
                        base: 256,
                        count: (frame_end - stack_base) as usize,
                        values: self.memory.map(stack_base, frame_end),
                    },
                    this_: VmFrameValues {
                        base: this,
                        count: n_this,
                        values: self.memory.map(this, this + n_this as i32),
                    },
                    that: VmFrameValues {
                        base: that,
                        count: 1,
                        values: vec![self.memory.that],
                    },
                    frame: FramePointers {
                        arg,
                        lcl,
                        ret: 0xffff,
                        that,
                        this,
                    },
                    used_segments: Some(self.get_used_segments(&invocation)),
                }
            } else {
                // give to memory.get_frame
                let frame = self.memory.get_frame(
                    invocation.frame_base,
                    invocation.n_args as i32,
                    fn_def.n_vars as i32,
                    self.execution_stack.last().and_then(|i| i.this_n).unwrap_or(0) as i32,
                    1,
                    Some(next_frame),
                );
                let mut frame = frame;
                frame.fn_def = Some(fn_def.clone());
                frame.used_segments = Some(self.get_used_segments(&invocation));
                frame
            }
        } else {
            // unknown
            vm_frame_unknown()
        }
    }

    pub fn derived_line(&self) -> usize {
        self.current_operation()
            .and_then(|op| op.span())
            .and_then(|s| s.line)
            .or(self.return_line)
            .unwrap_or(0)
    }

    pub fn write_debug(&self) -> String {
        let line = self.derived_line();
        let from = if line >= 5 { line - 5 } else { 0 };
        let to = std::cmp::min(line + 3, self.program.len());
        let lines = &self.program[from..to];
        let mut prog_lines = Vec::new();
        for (i, op) in lines.iter().enumerate() {
            prog_lines.push(format!(
                "{} {}",
                if i == (line - from) { "->" } else { "  " },
                write_op(op)
            ));
        }
        let prog = prog_lines.join("\n");
        if let Some(frame) = self.vm_stack().last() {
            format!("{}\n\n{}", prog, write_frame(frame))
        } else {
            prog
        }
    }
}

// make frame if unknown
fn vm_frame_unknown() -> VmFrame {
    VmFrame {
        fn_def: None,
        locals: VmFrameValues { base: 0, count: 0, values: vec![] },
        args: VmFrameValues { base: 0, count: 0, values: vec![] },
        stack: VmFrameValues { base: 0, count: 0, values: vec![] },
        this_: VmFrameValues { base: 0, count: 0, values: vec![] },
        that: VmFrameValues { base: 0, count: 0, values: vec![] },
        frame: FramePointers { arg: 0, lcl: 0, ret: 0, that: 0, this: 0 },
        used_segments: None,
    }
}

// helpers to print frames and ops
pub fn write_frame(frame: &VmFrame) -> String {
    let name = frame.fn_def.as_ref().map(|f| f.name.clone()).unwrap_or("Unknown Fn".to_string());
    format!(
        "Frame: {} arg:{} lcl:{}\nArgs: {}\nLcls: {}\nStck: {}",
        name,
        frame.frame.arg,
        frame.frame.lcl,
        write_frame_values(&frame.args),
        write_frame_values(&frame.locals),
        write_frame_values(&frame.stack),
    )
}

fn write_frame_values(fv: &VmFrameValues) -> String {
    format!("[{};{}][{}]", fv.base, fv.count, fv.values.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", "))
}

fn write_op(op: &VmInstruction) -> String {
    match op {
        VmInstruction::Add { .. } => "  add".to_string(),
        VmInstruction::And { .. } => "  and".to_string(),
        VmInstruction::Sub { .. } => "  sub".to_string(),
        VmInstruction::Eq { .. } => "  eq".to_string(),
        VmInstruction::Gt { .. } => "  gt".to_string(),
        VmInstruction::Lt { .. } => "  lt".to_string(),
        VmInstruction::Neg { .. } => "  neg".to_string(),
        VmInstruction::Not { .. } => "  not".to_string(),
        VmInstruction::Or { .. } => "  or".to_string(),
        VmInstruction::Return { .. } => "  return".to_string(),
        VmInstruction::Goto { label, .. } => format!("  goto    {}", label),
        VmInstruction::IfGoto { label, .. } => format!("  if-goto {}", label),
        VmInstruction::Label { label, .. } => format!("label     {}", label),
        VmInstruction::Call { name, n_args, .. } => format!("  call    {} {}", name, n_args),
        VmInstruction::Function { name, n_vars, .. } => format!("function  {} {}", name, n_vars),
        VmInstruction::Pop { segment, offset, .. } => format!("  pop     {:?} {}", segment, offset),
        VmInstruction::Push { segment, offset, .. } => format!("  push    {:?} {}", segment, offset),
        _ => "unknown".to_string(),
    }
}
