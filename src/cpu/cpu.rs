use crate::cpu::alu::{alu, Flags};
use crate::cpu::memory::{
    KeyboardInterface, MemInterface, Memory, MemoryKeyboard, RAM, ROM, SCREEN_OFFSET, SCREEN_SIZE, SubMemory,
};

pub struct CPUInput {
    inM:            u16,
    instruction:    u16,
    reset:          bool,
}

pub struct CPUOutput {
    outM:           u16,
    writeM:         bool,
    addrM:          u16,
}

pub struct CPUState {
    A: u16,
    D: u16,
    PC: u16,
    ALU: u16,
    flags: Flags,
}

#[derive(Debug, Clone, Copy)]
pub struct Bits {
    pub c: bool,
    pub x1: bool,
    pub x2: bool,
    pub am: bool,
    pub op: u16,
    pub d1: bool,
    pub d2: bool,
    pub d3: bool,
    pub j1: bool,
    pub j2: bool,
    pub j3: bool,
}

pub mod masks {
    pub const C: u16 = 0x8000;  // 0b1000_0000_0000_0000
    pub const X1: u16 = 0x9000; // 0b1001_0000_0000_0000
    pub const X2: u16 = 0x9000; // 0b1001_0000_0000_0000
    pub const AM: u16 = 0x9000; // 0b1001_0000_0000_0000
    pub const OP: u16 = 0x0FC0; // 0b0000_1111_1100_0000
    pub const D1: u16 = 0x8020; // 0b1000_0000_0010_0000
    pub const D2: u16 = 0x8010; // 0b1000_0000_0001_0000
    pub const D3: u16 = 0x8008; // 0b1000_0000_0000_1000
    pub const J1: u16 = 0x8001; // 0b1000_0000_0000_0001
    pub const J2: u16 = 0x8002; // 0b1000_0000_0000_0010
    pub const J3: u16 = 0x8004; // 0b1000_0000_0000_0100
}

/// Decode a 16-bit instruction into its constituent control bits.
///
/// The `op` field is returned as a 10-bit (shifted) value in a u16,
/// mirroring the JS implementation's `>> 6` extraction.
pub fn decode(instruction: u16) -> Bits {
    let bit = |mask: u16| (instruction & mask) == mask;

    Bits {
        c: bit(masks::C),
        x1: bit(masks::X1),
        x2: bit(masks::X2),
        am: bit(masks::AM),
        op: ((instruction & masks::OP) >> 6) as u16,
        d1: bit(masks::D1),
        d2: bit(masks::D2),
        d3: bit(masks::D3),
        j1: bit(masks::J1),
        j2: bit(masks::J2),
        j3: bit(masks::J3),
    }
}

pub fn cpuTick( inp: CPUInput, state: CPUState ) -> (CPUState, bool, u16) {
    let bits = decode(inp.instruction);
    let a = if bits.am { inp.inM } else { state.A };
    let (ALU, flags) = alu(state.D, a, bits.op);

    let mut D = state.D;
    if bits.d2 {
        D = ALU;
    }

    (
        CPUState   { A: state.A, D, PC: state.PC + 1, ALU, flags },
        bits.d3,
        ALU,
    )
}

pub fn cpuTock(inp: CPUInput, state: &mut CPUState) -> (CPUOutput, CPUState) {
    let bits: Bits = decode(inp.instruction);

    let j1 = bits.j1 && !state.flags.zr && !state.flags.ng;
    let j2 = bits.j2 && state.flags.zr;
    let j3= bits.j3 && !state.flags.zr && state.flags.ng;
    let jump = j1 || j2 || j3;

    let PC = match inp.reset {
        true => 0,
        false => if jump { state.A } else {state.PC}
    };

    let mut A: u16 = state.A;
    if !bits.c {
        A = inp.instruction & 0x7fff;
    } else if bits.d1 {
        A = state.ALU;
    } 

    let a = if bits.am { inp.inM } else { A };
    let aluOut = alu(state.D, a, bits.op);

    let ALU = aluOut.0;
    let flags = aluOut.1;

    (
        CPUOutput   { addrM: A, writeM: bits.d3, outM: ALU },
        CPUState    {   
                        A,
                        D: state.D,
                        PC,
                        ALU,
                        flags,
                    }
    )
}