pub struct Flags {
    pub zr: bool,
    pub ng: bool,
}

pub fn alu(d: u16, a: u16, op: u16) -> (u16, Flags) {
    let o: u16;

    match op {
        0x2a => o = 0,
        0x3f => o = 1,
        0x3a => o = 0xffff,
        0x0c => o = d,
        0x30 => o = a,
        0xd0 => o = !d,
        0x31 => o = !a,
        0x0f => o = 0u16.wrapping_sub(d),
        0x33 => o = 0u16.wrapping_sub(a),
        0x1f => o = d.wrapping_add(1),
        0x37 => o = a.wrapping_add(1),
        0x0e => o = d.wrapping_sub(1),
        0x32 => o = a.wrapping_sub(1),
        0x02 => o = a.wrapping_add(d),
        0x13 => o = d.wrapping_sub(a),
        0x07 => o = a.wrapping_sub(d),
        0x00 => o = d & a,
        0x15 => o = d | a,
        _ => { panic!() } // panicking because how did you get here
    }

    let msb_mask = 1u16 << (u16::BITS - 1);
    let flags = Flags { zr: o == 0, ng: (o & msb_mask) != 0 };

    (o, flags)
}