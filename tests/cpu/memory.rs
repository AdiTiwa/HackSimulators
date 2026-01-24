// Integration tests for memory module
// Note: This tests the cpu binary's memory implementation

use hacksimulators::cpu::memory::{Memory, SubMemory, MemInterface};

// Basic Memory tests
#[test]
fn test_memory_creation() {
    let memory = Memory::new(256);
    assert_eq!(memory.size, 256);
}

#[test]
fn test_memory_get_set() {
    let mut memory = Memory::new(256);
    memory.set(0, 42);
    assert_eq!(memory.get(0), 42);
}

#[test]
fn test_memory_reset() {
    let mut memory = Memory::new(256);
    memory.set(0, 100);
    memory.reset();
    assert_eq!(memory.get(0), 0);
}

#[test]
fn test_memory_get_out_of_bounds() {
    let memory = Memory::new(256);
    assert_eq!(memory.get(300), -1);
    assert_eq!(memory.get(-1), -1);
}

#[test]
fn test_memory_set_multiple() {
    let mut memory = Memory::new(256);
    memory.set(0, 10);
    memory.set(1, 20);
    memory.set(2, 30);
    assert_eq!(memory.get(0), 10);
    assert_eq!(memory.get(1), 20);
    assert_eq!(memory.get(2), 30);
}

#[test]
fn test_memory_reset_range() {
    let mut memory = Memory::new(256);
    memory.set(0, 100);
    memory.set(1, 100);
    memory.set(2, 100);
    memory.set(3, 100);
    
    memory.reset_range(Some(1), Some(3));
    
    assert_eq!(memory.get(0), 100);
    assert_eq!(memory.get(1), 0);
    assert_eq!(memory.get(2), 0);
    assert_eq!(memory.get(3), 100);
}

#[test]
fn test_memory_range() {
    let mut memory = Memory::new(256);
    memory.set(0, 10);
    memory.set(1, 20);
    memory.set(2, 30);
    memory.set(3, 40);
    
    let range = memory.range(Some(1), Some(4));
    assert_eq!(range, vec![20, 30, 40]);
}

#[test]
fn test_memory_load_bytes() {
    let mut memory = Memory::new(256);
    let bytes = vec![100, 101, 102, 103];
    memory.load_bytes(bytes.clone(), Some(10));
    
    assert_eq!(memory.get(10), 100);
    assert_eq!(memory.get(11), 101);
    assert_eq!(memory.get(12), 102);
    assert_eq!(memory.get(13), 103);
}

// SubMemory tests
#[test]
fn test_submemory_creation() {
    let mut memory = Memory::new(256);
    let submem = SubMemory::new(&mut memory, 64, Some(128));
    assert_eq!(submem.size, 64);
}

#[test]
fn test_submemory_get_set() {
    let mut memory = Memory::new(256);
    memory.set(130, 999);
    
    let submem = SubMemory::new(&mut memory, 64, Some(128));
    assert_eq!(submem.get(2), 999);
}

#[test]
fn test_submemory_set_through_parent() {
    let mut memory = Memory::new(256);
    {
        let mut submem = SubMemory::new(&mut memory, 64, Some(100));
        submem.set(10, 555);
    }
    
    assert_eq!(memory.get(110), 555);
}

#[test]
fn test_submemory_reset() {
    let mut memory = Memory::new(256);
    memory.set(50, 100);
    memory.set(51, 100);
    memory.set(52, 100);
    
    {
        let mut submem = SubMemory::new(&mut memory, 64, Some(50));
        submem.reset();
    }
    
    assert_eq!(memory.get(50), 0);
    assert_eq!(memory.get(51), 0);
    assert_eq!(memory.get(52), 0);
}

#[test]
fn test_submemory_range() {
    let mut memory = Memory::new(256);
    memory.set(100, 10);
    memory.set(101, 20);
    memory.set(102, 30);
    memory.set(103, 40);
    
    let submem = SubMemory::new(&mut memory, 64, Some(100));
    let range = submem.range(Some(0), Some(3));
    assert_eq!(range, vec![10, 20, 30]);
}

#[test]
fn test_submemory_out_of_bounds() {
    let mut memory = Memory::new(256);
    let submem = SubMemory::new(&mut memory, 64, Some(100));
    assert_eq!(submem.get(100), -1);
    assert_eq!(submem.get(-1), -1);
}
