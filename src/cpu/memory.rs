#[derive(Debug, Clone, Copy)]
pub enum Format {
    Bin,
    Dec,
    Hex,
}

impl Format {
    pub fn radix(self) -> u32 {
        match self {
            Self::Bin   =>  2,
            Self::Dec   =>  10,
            Self::Hex   =>  16,
        }
    }
}

pub const SCREEN_OFFSET: u16 = 0x4000;
pub const SCREEN_ROWS: u16 = 256;
pub const SCREEN_COLS: u16 = 32;
pub const SCREEN_SIZE: u16 = SCREEN_ROWS * SCREEN_COLS;
pub const KEYBOARD_OFFSET: u16 = 0x6000;

pub trait MemInterface {
    fn get          (&self, index: i32) -> u16;
    fn set          (&mut self, index: i32, value: u16);
    fn reset        (&mut self);
    fn update       (&mut self, index: i32, value: &str, format: Format);
    fn load_bytes   (&mut self, bytes: Vec<u16>, offset: Option<i32>);
    fn range        (&self, start: Option<i32>, end: Option<i32>) -> Vec<u16>;
}

pub trait KeyboardInterface {
    fn get_key       (&self) -> u16;
    fn set_key       (&mut self, key: u16);
    fn clear_key     (&mut self);
}

pub struct Memory {
    pub size:   i32,
    memory  :   Vec<u16>,
}

impl Memory {
    pub fn new(size: i32) -> Memory {
        Memory {size, memory: vec![0; size as usize]}
    }

    pub fn with_vec(mem: Vec<u16>) -> Memory {
        Memory { size: mem.len() as i32, memory: mem }
    }

    pub fn reset_range(&mut self, start: Option<i32>, end: Option<i32>) {
        let a = start.unwrap_or(0) as usize;
        let b = end.unwrap_or(self.size) as usize;

        for i in a..b {
            self.memory[i] = 0;
        }
    }
}

impl MemInterface for Memory {
    fn get(&self, index: i32) -> u16 {
        if index < 0 || index >= self.size { return 0xffff }
        self.memory[index as usize]
    }

    fn set(&mut self, index: i32, value: u16) {
        assert!(index >= 0 && index < self.size);
        self.memory[index as usize] = value;
    }
    
    fn reset(&mut self) {
        self.memory = vec![0; self.size as usize];
    }

    // making the executive decision that the format will always be binary or hex
    fn update(&mut self, index: i32, value: &str, format: Format) {
        if index <= self.size && index > 0 {
            let trimmed = value.strip_prefix("0x").unwrap_or(value);

            match u16::from_str_radix(trimmed, format.radix()) {
                Ok(number)  => self.set(index, number),
                Err(_)      => self.set(index, 0xffff),
            }
        }
    }

    fn load_bytes(&mut self, bytes: Vec<u16>, offset: Option<i32>) {
        let start = offset.unwrap_or(0);
        
        if start <= (self.size + bytes.len() as i32) && start > 0 {
            self.reset();
            
            for i in bytes.iter().enumerate() {
                self.set(start + i.0 as i32, *i.1);
            }
        }
    }

    fn range(&self, start: Option<i32>, end: Option<i32>) -> Vec<u16> {
        let a = start.unwrap_or(0) as usize;
        let b = end.unwrap_or(self.size) as usize;

        self.memory[a..b].to_vec()
    }
}

pub struct SubMemory<'a> {
    parent: &'a mut Memory,
    pub size: i32, 
    offset: i32,
}

impl<'a> SubMemory<'a> {
    pub fn new(parent: &'a mut Memory, size: i32, offset: Option<i32>) -> SubMemory<'a> {
        let index = offset.unwrap_or(0);
        assert!(index + size <= parent.size);

        SubMemory { parent, size, offset: index }
    }
}

impl<'a> MemInterface for SubMemory<'a> {
    fn get(&self, index: i32) -> u16 {
        if index < 0 || index >= self.size { return 0xffff; }
        self.parent.get(self.offset + index)
    }

    fn set(&mut self, index: i32, value: u16) {
        if index >= 0 && index < self.size {
            self.parent.set(self.offset + index, value);
        }
    }

    fn reset(&mut self) {
        self.parent.reset_range(Some(self.offset), Some(self.offset + self.size));
    }

    fn update(&mut self, index: i32, value: &str, format: Format) {
        if index <= self.size && index > 0 {
            self.parent.update(self.offset + index, value, format);
        }
    }

    fn load_bytes(&mut self, bytes: Vec<u16>, offset: Option<i32>) {
        let start = offset.unwrap_or(0);

        if start <= self.size && start > 0 {
            self.parent.load_bytes(bytes, Some(self.offset + start));
        }
    }

    fn range(&self, start: Option<i32>, end: Option<i32>) -> Vec<u16> {
        let a = start.unwrap_or(0);
        let b = end.unwrap_or(self.size);
        self.parent.range(Some(self.offset + a), Some(self.offset + b))
    }
}

pub struct MemoryKeyboard<'a> {
    sub: SubMemory<'a>,
}

impl<'a> MemoryKeyboard<'a> {
    pub fn new(mem: &'a mut Memory) -> Self {
        Self {
            sub: SubMemory::new(mem, 1, Some(KEYBOARD_OFFSET.into())),
        }
    }
}

impl KeyboardInterface for MemoryKeyboard<'_> {
    fn get_key(&self) -> u16 {
        self.sub.get(0)
    }

    fn set_key(&mut self, key: u16) {
        self.sub.set(0, key & 0xffff);
    }

    fn clear_key (&mut self) {
        self.sub.set(0, 0);
    }
}

pub struct ROM {
    mem: Memory,
}

impl ROM {
    pub const SIZE: i32 = 0x8000;

    /// Create an empty ROM.
    pub fn new_empty() -> Self {
        Self {
            mem: Memory::new(Self::SIZE),
        }
    }

    /// Create a ROM initialized with `program` words. Remaining words are zeros.
    pub fn from_program(program: &[u16]) -> Self {
        let mut vec = vec![0u16; Self::SIZE.try_into().unwrap()];
        let copy_len = program.len().min(Self::SIZE.try_into().unwrap());
        vec[..copy_len].copy_from_slice(&program[..copy_len]);
        Self {
            mem: Memory::with_vec(vec),
        }
    }

    pub fn get(&self, index: i32) -> u16 {
        self.mem.get(index)
    }
}

pub struct RAM {
    pub mem: Memory,
}

impl RAM {
    pub const SIZE: i32 = 0x4000 + 0x2000 + 1;

    pub fn new() -> Self {
        Self {
            mem: Memory::new(Self::SIZE),
        }
    }

    pub fn screen(&mut self) -> SubMemory<'_> {
        SubMemory::new(
            &mut self.mem,
            SCREEN_SIZE as i32,
            Some(SCREEN_OFFSET.into()),
        )
    }

    pub fn keyboard(&mut self) -> SubMemory<'_> {
        SubMemory::new(&mut self.mem, 1, Some(KEYBOARD_OFFSET.into()))
    }
}
