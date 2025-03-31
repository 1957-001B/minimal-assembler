use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
};

#[derive(Debug, PartialEq)]
enum Instruction {
    // Minimal
    MOVK {
        rd: Operand,
        imm: Operand,
        shift: Operand,
    },
    MOVZ {
        rd: Operand,
        imm: Operand,
        shift: Operand,
    },
    ORR {
        dest: Operand,
        src1: Operand,
        src2: Operand,
    },
    ADD {
        dest: Operand,
        src1: Operand,
        src2: Operand,
    },
    B {
        addr: Operand,
    },
    LDR {
        dest: Operand,
        addr: Operand,
    },
    STUR {
        src: Operand,
        addr: Operand,
    },
    SVC {
        syscall: Operand,
    },
}

#[derive(Debug, PartialEq)]
enum Operand {
    Reg(Reg),
    Imm(ImmType), // TODO implement Uimm (unisgned) and Simm (signed) for use with ADD and B relative adressing
}

#[derive(Debug, PartialEq)]
enum ImmType {
    Unsigned(u64),
    Unsigned16(u16),
    Address(u64),
    UnresolvedSymbol(String),
}

#[derive(Debug)]
struct State {
    labels: HashMap<String, u64>,
    current_addr: u64,
    unresolved_refs: Vec<(String, usize)>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
#[repr(u32)]
enum Reg {
    // keeping only 64 bit registers for simplicity easy to implement 32 bit
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    X8 = 8,
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    X13 = 13,
    X14 = 14,
    X15 = 15,
    X16 = 16,
    X17 = 17,
    X18 = 18,
    X19 = 19,
    X20 = 20,
    X21 = 21,
    X22 = 22,
    X23 = 23,
    X24 = 24,
    X25 = 25,
    X26 = 26,
    X27 = 27,
    X28 = 28,
    X29 = 29,
    X30 = 30,
    XZR = 31,
}

fn parse_register(reg: &str) -> Reg {
    match reg.to_uppercase().as_str() {
        "X0" => Reg::X0,
        "X1" => Reg::X1,
        "X2" => Reg::X2,
        "X3" => Reg::X3,
        "X4" => Reg::X4,
        "X5" => Reg::X5,
        "X6" => Reg::X6,
        "X7" => Reg::X7,
        "X8" => Reg::X8,
        "X9" => Reg::X9,
        "X10" => Reg::X10,
        "X11" => Reg::X11,
        "X12" => Reg::X12,
        "X13" => Reg::X13,
        "X14" => Reg::X14,
        "X15" => Reg::X15,
        "X16" => Reg::X16,
        "X17" => Reg::X17,
        "X18" => Reg::X18,
        "X19" => Reg::X19,
        "X20" => Reg::X20,
        "X21" => Reg::X21,
        "X22" => Reg::X22,
        "X23" => Reg::X23,
        "X24" => Reg::X24,
        "X25" => Reg::X25,
        "X26" => Reg::X26,
        "X27" => Reg::X27,
        "X28" => Reg::X28,
        "X29" => Reg::X29,
        "X30" => Reg::X30,
        "XZR" => Reg::XZR,

        _ => panic!("Not a valid register"),
    }
}

fn parse_line(line: &str, state: &mut State) -> Option<Instruction> {
    // Determine if a label
    if line.ends_with(':') {
        state.labels.insert(
            line.strip_suffix(':').unwrap().to_string(),
            state.current_addr,
        );
        return None;
    }

    let mut parts: Vec<String> = line
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();

    if let Some(first) = parts.first_mut() {
        *first = first.to_uppercase().to_string();
    }

    let parts: Vec<&str> = parts.iter().map(|s| s.as_str()).collect();
    println!("{:#?}", parts);
    let inst = match parts.as_slice() {
        // without shift
        ["MOVK", rd, imm] => Some(Instruction::MOVK {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: Operand::Imm(ImmType::Unsigned16(0)),
        }),
        //shifted
        ["MOVK", rd, imm, "LSL", shift] => Some(Instruction::MOVK {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: {
                let shift_value = shift.strip_prefix('#').unwrap();
                Operand::Imm(ImmType::Unsigned16(shift_value.parse::<u16>().unwrap()))
            },
        }),

        ["MOVZ", rd, imm] => Some(Instruction::MOVZ {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: Operand::Imm(ImmType::Unsigned16(0)),
        }),

        //shifted
        ["MOVZ", rd, imm, "LSL", shift] => Some(Instruction::MOVZ {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: {
                let shift_value = shift.strip_prefix('#').unwrap();
                Operand::Imm(ImmType::Unsigned16(shift_value.parse::<u16>().unwrap()))
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/ORR--immediate---Bitwise-OR--immediate--?lang=en
        ["ORR", dest, src1, src2] => Some(Instruction::ORR {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            src1: if src1.starts_with('X') {
                Operand::Reg(parse_register(src1.trim_end_matches(',')))
            } else {
                Operand::Imm(ImmType::Unsigned(
                    src1.trim_end_matches(',').parse().unwrap(),
                ))
            },

            src2: if src2.starts_with('X') {
                Operand::Reg(parse_register(src2.trim_end_matches(',')))
            } else {
                Operand::Imm(ImmType::Unsigned(
                    src2.trim_end_matches(',').parse().unwrap(),
                ))
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/ADD--immediate---Add-immediate-value-?lang=en or register???
        ["ADD", dest, src1, src2] => Some(Instruction::ADD {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            src1: if src1.starts_with('X') {
                Operand::Reg(parse_register(src1.trim_end_matches(',')))
            } else {
                Operand::Imm(ImmType::Unsigned(
                    src1.trim_end_matches(',').parse().unwrap(),
                ))
            },

            src2: if src2.starts_with('X') {
                Operand::Reg(parse_register(src2.trim_end_matches(',')))
            } else {
                Operand::Imm(ImmType::Unsigned(
                    src2.trim_end_matches(',').parse().unwrap(),
                ))
            },
        }),
        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/LDR--immediate---Load-register--immediate--?lang=en#XnSP_option
        ["LDR", dest, addr] => Some(Instruction::LDR {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            addr: if addr.starts_with('=') {
                if addr.starts_with("0x") {
                    Operand::Imm(ImmType::Address(
                        u64::from_str_radix(addr.strip_prefix("0x").unwrap(), 16).unwrap_or(0),
                    ))
                } else {
                    let symbol_name = addr.strip_prefix('=').unwrap().to_string();
                    let unref = Operand::Imm(ImmType::UnresolvedSymbol(symbol_name.clone()));
                    state
                        .unresolved_refs
                        .push((symbol_name, state.current_addr.try_into().unwrap()));
                    unref
                }
            } else {
                panic!("Literal Addr Only")
            },
        }),
        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/STR--immediate---Store-register--immediate--?lang=en
        ["STUR", dest, addr] => Some(Instruction::STUR {
            src: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            addr: if addr.starts_with('=') {
                Operand::Imm(ImmType::Address(
                    u64::from_str_radix(addr.trim_start_matches("=0x"), 16).unwrap(),
                ))
            } else {
                panic!("Only literal addressing supported");
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/B--Branch-?lang=en
        ["B", addr] => Some(Instruction::B {
            addr: if addr.starts_with("0x") {
                Operand::Imm(ImmType::Address(
                    u64::from_str_radix(addr.trim_start_matches("0x"), 16).unwrap(),
                ))
            } else if addr.starts_with('.') {
                Operand::Imm(ImmType::Unsigned(0))
            } else {
                panic!("Invalid Address")
            },
        }),
        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/SVC--Supervisor-call-?lang=en
        ["SVC", syscall] => Some(Instruction::SVC {
            syscall: if syscall.starts_with('#') {
                Operand::Imm(ImmType::Unsigned(
                    u64::from_str_radix(
                        syscall.trim_start_matches('#').trim_start_matches("0x"),
                        16,
                    )
                    .unwrap(),
                ))
            } else {
                panic!("Syntax error # for imm");
            },
        }),

        _ => None,
    };
    if !line.trim_start().starts_with('.') {
        println!("{:#?}", state);
        state.current_addr += 4;
    }
    inst
}
fn encode_movk(rd: &Operand, imm: &Operand, shift: &Operand) -> u32 {
    // Extract register number
    let rd_val = match rd {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };
    
    // Extract immediate value
    let imm_val = match imm {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for imm"),
    };
    
    // Extract shift amount
    let shift_val = match shift {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for shift"),
    };
    
    // Calculate hw field based on shift
    let hw = match shift_val {
        0 => 0b00,
        16 => 0b01,
        32 => 0b10,
        48 => 0b11,
        _ => panic!("Invalid shift value for MOVK"),
    };
    
    // Set sf bit (1 for 64-bit, 0 for 32-bit)
    let sf = 1; // 64-bit variant
    
    // Construct the instruction using the encoding format:
    // sf(1) opc(7) hw(2) imm16(16) Rd(5) op(1)
    let encoded = (sf << 31) |            // sf bit
                 (0b11100101 << 23) |     // opc field
                 (hw << 21) |             // hw field
                 (imm_val << 5) |         // imm16 field
                 rd_val;                  // Rd field
    
    encoded
}
fn encode_movz(rd: &Operand, imm: &Operand, shift: &Operand) -> u32 {
    // Extract register number
    let rd_val = match rd {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };
    
    // Extract immediate value
    let imm_val = match imm {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for imm"),
    };
    dbg!(&imm_val);
    
    // Extract shift amount
    let shift_val = match shift {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for shift"),
    };
    
    // Calculate hw field based on shift
    let hw = match shift_val {
        0 => 0b00,
        16 => 0b01,
        32 => 0b10,
        48 => 0b11,
        _ => panic!("Invalid shift value for MOVK"),
    };
    
    // Set sf bit (1 for 64-bit, 0 for 32-bit)
    let sf = 1; // 64-bit variant
    
    // Construct the instruction using the encoding format:
    // sf(1) opc(7) hw(2) imm16(16) Rd(5) op(1)
    let encoded = (sf << 31) |            // sf bit
                 (0b10100101<< 23) |     // opc field
                 (hw << 21) |             // hw field
                 (imm_val << 5) |         // imm16 field
                 rd_val;                  // Rd field
    
    encoded
}

fn encode_line(op: Instruction, _state: &mut State) -> u32 {
    match op {
        Instruction::MOVK { rd, imm, shift } =>  encode_movk(&rd, &imm, &shift),
        Instruction::MOVZ { rd, imm, shift } => encode_movz(&rd, &imm, &shift),
        _ => 0
    }
}
fn assemble(path: String) -> io::Result<()> {
    let mut file = File::open(path)?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    let contents: Vec<&str> = contents
        .split('\n')
        .map(|l| l.trim())
        .filter(|l| !l.starts_with("//") && !l.trim().is_empty())
        .collect();

    dbg!(&contents);

    let mut state = State {
        labels: HashMap::new(),
        current_addr: 0,
        unresolved_refs: Vec::new(),
    };

    let mut parsed = Vec::new();
    // first pass
    for l in &contents {
        if let Some(instruction) = parse_line(l, &mut state) {
            parsed.push(instruction);
        }
    }

    let mut encoded: Vec<u32> =  Vec::new();

    //second pass
    for l in parsed{
        let encoded_line = encode_line(l, &mut state);
        println!("{:0b}",&encoded_line);
        encoded.push(encoded_line);
        
    } 


    return Ok(());
}
fn main() {
    let _asm = assemble("goal.s".to_string());
}

// tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        let result = assemble("goal.s".to_string());
        assert!(matches!(result, Ok(())))
    }
}
