use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    path,
};

// const TEST_LINE: &str = "ORR X0, XZR, 1";
// const TEST_LINE: &str = "LDR X0, =0x1234567890ABCDEF";
// const TEST_LINE: &str = "main:";
//const TEST_LINE: &str = "MOVK X0, #0x1234";
const TEST_LINE: &str = "MOVK X0, #0xABCD, LSL #16";
#[derive(Debug, PartialEq)]
enum Instruction {
    // Minimal
    MOVK {
        rd: Operand,
        imm: Operand,
        shift: Operand,
    },
    MOVZ {
        dest: Operand,
        src1: Operand,
        src2: Operand,
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
    Signed(i64),
    Logical(u64), // For bitmask immediates
    Address(u64),
}

struct State {
    labels: HashMap<String, u64>,
    current_addr: u64,
}

#[derive(Debug, PartialEq)]
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

    let mut parts: Vec<&str> = line
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
        .collect();
    
    let opcode_upper = parts[0].to_uppercase();
    
    parts[0] = &opcode_upper;


    let inst = match parts.as_slice() {
        // without shift
        ["MOVK", rd, imm] => dbg!(Some(Instruction::MOVK {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap()
            )),
            shift: Operand::Imm(ImmType::Unsigned16(0))
        })),
        //shifted
        ["MOVK", rd, imm, "LSL", shift] => dbg!(Some(Instruction::MOVK {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap()
            )),
            shift: {
                let shift_value = shift.strip_prefix('#').unwrap();
                Operand::Imm(ImmType::Unsigned16(shift_value.parse::<u16>().unwrap()))
            }
        })),

        //   ["MOVZ", dest, src1, src2] => dbg!(Some(Instruction::MOVZ{

        //     })),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/ORR--immediate---Bitwise-OR--immediate--?lang=en
        ["ORR", dest, src1, src2] => dbg!(Some(Instruction::ORR {
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
        })),
        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/ADD--immediate---Add-immediate-value-?lang=en or register???
        ["ADD", dest, src1, src2] => dbg!(Some(Instruction::ADD {
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
        })),
        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/LDR--immediate---Load-register--immediate--?lang=en#XnSP_option
        ["LDR", dest, addr] => dbg!(Some(Instruction::LDR {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            addr: if addr.starts_with('=') {
                Operand::Imm(ImmType::Address(
                    u64::from_str_radix(addr.trim_start_matches("=0x"), 16).unwrap(),
                ))
            } else {
                panic!("Only literal addressing supported");
            },
        })),
        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/STR--immediate---Store-register--immediate--?lang=en
        ["STUR", dest, addr] => dbg!(Some(Instruction::STUR {
            src: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            addr: if addr.starts_with('=') {
                Operand::Imm(ImmType::Address(
                    u64::from_str_radix(addr.trim_start_matches("=0x"), 16).unwrap(),
                ))
            } else {
                panic!("Only literal addressing supported");
            },
        })),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/B--Branch-?lang=en
        ["B", addr] => dbg!(Some(Instruction::B {
            addr: if addr.starts_with("0x") {
                Operand::Imm(ImmType::Address(
                    u64::from_str_radix(addr.trim_start_matches("0x"), 16).unwrap(),
                ))
            } else if addr.starts_with('.') {
                Operand::Imm(ImmType::Unsigned(0))
            } else {
                panic!("Invalid Address")
            }
        })),
        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/SVC--Supervisor-call-?lang=en
        ["SVC", syscall] => dbg!(Some(Instruction::SVC {
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
            }
        })),

        _ => None,
    };
    state.current_addr += 4;
    inst
}

// fn encode(op: Instruction) -> u32 {
//     match op {
//         Instruction::ORR { dest, src1, src2 } => {
//             match (src1,src2) {
//                 // Imm ==> Reg
//                 (Operand::Reg(rn), Operand::Imm(rm)) => {
//                     let sf = 1;
//                     let opcode = 0x64;
//                     let N = 0;
//                     let immr =

//                     let encoding: u32 = 0;
//                     encoding
//                 }
//             }
//         }
//         Instruction::ADD { dest, src1, src2 } => encode_add_imm(dest,src1,src2)
//     }
// }
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
    };

    let mut parsed = Vec::new();

    for l in &contents {
        if let Some(instruction) = parse_line(l, &mut state) {
            parsed.push(instruction);
        }
    }

    println!("{:#?}", parsed);

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
