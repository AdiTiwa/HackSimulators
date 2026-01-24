#[derive(Debug, Clone, Copy)]
pub enum Format {
    Bin,
    Dec,
    Hex,
}

impl Format {
    pub fn radix(self) -> i16 {
        match self {
            Self::Bin   =>  2,
            Self::Dec   =>  10,
            Self::Hex   =>  16,
        }
    }
}

pub const SCREEN_OFFSET: i16 = 0x4000;
pub const SCREEN_ROWS: i16 = 256;
pub const SCREEN_COLS: i16 = 32;
pub const SCREEN_SIZE: i16 = SCREEN_ROWS * SCREEN_COLS;
pub const KEYBOARD_OFFSET: i16 = 0x6000;

pub trait MemInterface {
    fn get          (&self, index: i32) -> i16;
    fn set          (&mut self, index: i32, value: i16);
    fn reset        (&mut self);
    fn update       (&mut self, index: i32, value: &str, format: Format);
    fn load_bytes   (&mut self, bytes: Vec<i16>, offset: Option<i32>);
    fn range        (&self, start: Option<i32>, end: Option<i32>) -> Vec<i16>;
}

pub struct Memory {
    pub size:   i32,
    memory  :   Vec<i16>,
}

impl Memory {
    pub fn new(size: i32) -> Memory {
        Memory {size, memory: vec![0; size as usize]}
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
    fn get(&self, index: i32) -> i16 {
        if index < 0 || index >= self.size { return -1 }
        self.memory[index as usize]
    }

    fn set(&mut self, index: i32, value: i16) {
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

            match i16::from_str_radix(trimmed, format.radix() as u32) {
                Ok(number)  => self.set(index, number),
                Err(_)      => self.set(index, -1),
            }
        }
    }

    fn load_bytes(&mut self, bytes: Vec<i16>, offset: Option<i32>) {
        let start = offset.unwrap_or(0);
        
        if start <= (self.size + bytes.len() as i32) && start > 0 {
            self.reset();
            
            for i in 0..bytes.len() {
                self.set(start + i as i32, bytes[i]);
            }
        }
    }

    fn range(&self, start: Option<i32>, end: Option<i32>) -> Vec<i16> {
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
    fn get(&self, index: i32) -> i16 {
        if index < 0 || index >= self.size { return -1; }
        self.parent.get(self.offset + index)
    }

    fn set(&mut self, index: i32, value: i16) {
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

    fn load_bytes(&mut self, bytes: Vec<i16>, offset: Option<i32>) {
        let start = offset.unwrap_or(0);

        if start <= self.size && start > 0 {
            self.parent.load_bytes(bytes, Some(self.offset + start));
        }
    }

    fn range(&self, start: Option<i32>, end: Option<i32>) -> Vec<i16> {
        let a = start.unwrap_or(0);
        let b = end.unwrap_or(self.size);
        self.parent.range(Some(self.offset + a), Some(self.offset + b))
    }
}   