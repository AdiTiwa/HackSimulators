use std::{
    collections::HashMap,
    fs::{self, File},
    io::Read,
};

// Enum for the structuring of each line
#[derive(Debug, Clone)]
enum Line {
    AVal(u16), // A instruction referencing direct value
    ASym(String),  // A instruction referencing jump location or arbitrary variable
    C { // C instruction with the comp, dest (optional), and jump (optional)
        dest: Option<String>,
        comp: String,
        jump: Option<String>,
    },
}

// Remove comments and whitespace from the line
fn clean_line(s: &str) -> String {
    let line_wout_com = if let Some(idx) = s.find("//") { &s[..idx] } else { s };
    line_wout_com.trim().replace(char::is_whitespace, "")
}

// Process label into string name
fn parse_label(line: &str) -> Option<String> {
    if line.starts_with('(') && line.ends_with(')') && line.len() >= 3 {
        let name = &line[1..line.len() - 1];
        if name.is_empty() { panic!("Label is empty")};
        Some(name.to_string())
    } else {
        None
    }
}
// Convert a raw string instruction into the formatted Line
fn parse_instruction(line: &str) -> Line {
    // Parse A instruction
    if line.starts_with('@') {
        let sym = &line[1..];
        // Parsing a raw address
        if is_number(sym) {
            // Make sure address in range
            let addr: u16 = sym.parse::<u16>().expect("A-instruction number out of range");
            Line::AVal(addr)
        } else {
        // Parsing a labeled address or variable
            Line::ASym(sym.to_string())
        }
    } 
    // Parse C instruction
    else {
        // Split by destination assigner
        let (destination_seg, rest) = if let Some(eq) = line.find('=') {
            (Some(&line[..eq]), &line[eq + 1..])
        } else {
            (None, line)
        };
        // Split by Jump operator
        let (comp_part, jump_part) = if let Some(sc) = rest.find(';') {
            (&rest[..sc], Some(&rest[sc + 1..]))
        } else {
            (rest, None)
        };

        Line::C {
            dest: destination_seg.map(|s| s.to_string()),
            comp: comp_part.to_string(),
            jump: jump_part.map(|s| s.to_string()),
        }
    }
}

// check if string is a number
fn is_number(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
}



fn defd_symbols() -> HashMap<String, u16> {
    let mut special_symbols: HashMap<String, u16> = HashMap::new();
    // Spceial pointers as defined in the book
    special_symbols.insert("SP".into(), 0);
    special_symbols.insert("LCL".into(), 1);
    special_symbols.insert("ARG".into(), 2);
    special_symbols.insert("THIS".into(), 3);
    special_symbols.insert("THAT".into(), 4);
    // Predefined register values
    for r in 0..=15 {
        special_symbols.insert(format!("R{r}"), r as u16);
    }
    // Screen and keyboard values
    special_symbols.insert("SCREEN".into(), 16384);
    special_symbols.insert("KBD".into(), 24576);
    return special_symbols;
}

// Returns (a_bit, c1 -> c6)
fn comp_bits(comp: &str) -> (u8, u8) {
    // a=0 values
    match comp {
        "0"   => return (0, 0b101010),
        "1"   => return (0, 0b111111),
        "-1"  => return (0, 0b111010),
        "D"   => return (0, 0b001100),
        "A"   => return (0, 0b110000),
        "!D"  => return (0, 0b001101),
        "!A"  => return (0, 0b110001),
        "-D"  => return (0, 0b001111),
        "-A"  => return (0, 0b110011),
        "D+1" => return (0, 0b011111),
        "A+1" => return (0, 0b110111),
        "D-1" => return (0, 0b001110),
        "A-1" => return (0, 0b110010),
        "D+A" => return (0, 0b000010),
        "D-A" => return (0, 0b010011),
        "A-D" => return (0, 0b000111),
        "D&A" => return (0, 0b000000),
        "D|A" => return (0, 0b010101),
        _ => {}
    }
    // a=1 values
    match comp {
        "M"   => return (1, 0b110000),
        "!M"  => return (1, 0b110001),
        "-M"  => return (1, 0b110011),
        "M+1" => return (1, 0b110111),
        "M-1" => return (1, 0b110010),
        "D+M" => return (1, 0b000010),
        "D-M" => return (1, 0b010011),
        "M-D" => return (1, 0b000111),
        "D&M" => return (1, 0b000000),
        "D|M" => return (1, 0b010101),
        _ => {}
    }
    panic!("Unknown comp: {comp}");
}

// Destination bits correspond as (d1 -> A, d2 -> D, d3 -> M)
// Pretty self explanatory
fn dest_bits(dest: Option<&str>) -> u8 {
    if dest.is_none() {
        return 0;
    }
    let destination_str: &str = dest.unwrap();
    let mut a_bit: i32 = 0;
    let mut d_bit: i32 = 0;
    let mut m_bit: i32 = 0;
    for char in destination_str.chars() {
        match char {
            'A' => a_bit = 1,
            'D' => d_bit = 1,
            'M' => m_bit = 1,
            _ => panic!("Wrong destination character {char} in destination {destination_str}"),
        }
    }
    
    return ((a_bit << 2) | (d_bit << 1) | m_bit).try_into().unwrap()
}

// Match the JUMP code to the bit pattern
fn jump_bits(jump: Option<&str>) -> u8 {
    match jump {
        None | Some("") => 0b000,
        Some("JGT") => 0b001,
        Some("JEQ") => 0b010,
        Some("JGE") => 0b011,
        Some("JLT") => 0b100,
        Some("JNE") => 0b101,
        Some("JLE") => 0b110,
        Some("JMP") => 0b111,
        Some(other) => panic!("Wrong JUMP code: {other}"),
    }
}


fn main() -> std::io::Result<()> {
    // Read the whole file to a string
    let mut file = File::open("Prog.asm")?;
   
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("Loaded file Prog.asm with {:?} lines", contents.split('\n').collect::<Vec<&str>>().len());

    // Predefined symbol hashmap
    let mut symbols: HashMap<String, u16> = defd_symbols();

    // Resulting lines  
    let mut program: Vec<Line> = Vec::new();
    let mut rom_addr: u16 = 0;

    for unparsed_line in contents.lines() {
        let line = clean_line(unparsed_line);
        if line.is_empty() {
            // If it's a raw comment line or whitespace, we skip
            continue;
        }

        if let Some(name) = parse_label(&line) {
            // If we get a label, we insert the symbol and the address into the hashmap
            symbols.insert(name, rom_addr);
            continue;
        }

        // Instruction line
        let parsed = parse_instruction(&line);
        program.push(parsed);
        // Increase the counter
        rom_addr = rom_addr.checked_add(1).expect("Exceeded 16 bit rom limit");
    }
    // Address value for the variable to be assigned
    let mut next_var: u16 = 16;
    for inst in &program {
        // If it's a symbol A instruction
        if let Line::ASym(sym) = inst {
            // If it's a symbol we recognize
            if !is_number(sym) && !symbols.contains_key(sym) {
                // Insert into our symbols, increment variable count
                symbols.insert(sym.clone(), next_var);
                next_var = next_var.checked_add(1).expect("No more variable addresses");
            }
        }
    }

    // Actually encode
    let mut finalstring: String = String::new();
    let mut counte: u32 = 0;
    for inst in program {
        match inst {
            // Write the line for an Address with direct value reference
            Line::AVal(address) => {
                // anding against 7FFFF means it ends up in this form:
                // 0 [addr;15]
                let towrite = format!("{:016b}\n", address & 0x7FFF);
                finalstring.push_str(&towrite);
                
                
            }
            // Write the line for an Address with a symbol reference
            Line::ASym(symbol_name) => {
                // We fetch and define the symbol at writetime
                let addr = *symbols.get(&symbol_name)
                    .unwrap_or_else(|| panic!("Unresolved symbol: {symbol_name}"));

                let towrite = format!("{:016b}\n", addr & 0x7FFF);
                finalstring.push_str(&towrite);
            }
            // Write the line for a C instruction
            Line::C { dest, comp, jump } => {
                
                let (a_bit, comp_bits) = comp_bits(&comp);
                let dest_bits = dest_bits(dest.as_deref());
                let jump_bits = jump_bits(jump.as_deref());
                // 111 (a) (c1 c2 c3 c4 c5 c6) (d1 d2 d3) (j1 j2 j3)
                
                let word: u16 =
                    0b1110_0000_0000_0000 |
                    ((a_bit as u16) << 12) |
                    ((comp_bits as u16) << 6) |
                    ((dest_bits as u16) << 3) |
                    (jump_bits as u16);

                let towrite = format!("{:016b}\n", word);
                finalstring.push_str(&towrite);
            }
        }
        counte +=1;
    }
    // Remove the newline at the end
    finalstring.remove(finalstring.len()-1);
    // Write to Prog.hack
    fs::write("Prog.hack", finalstring)?;
    println!("Successfully written {} lines of machine code to Prog.hack", {counte});

    Ok(())
}
