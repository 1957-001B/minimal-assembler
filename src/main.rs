use std::{fmt::Binary, str::Bytes};

/*
12:00 Parsing of ORR/MOV (X) 23:39 works as such
| "ORR X0, XZR, 1"; -> ORR {
|                       dest: Reg(
|                         X0,
|                       ),
|                       src1: Reg(
|                         XZR,
|                       ),
|                       src2: Imm(
|                         1,
|                       ),
|                    },
12:30 Parsing of SVC (x) 00:14, ADD (x) 00:20, B (x) 00:49 with labels parsing ~(hardcoded/imm values only)~
01:00 Tests for opsc
*/
const TEST_LINE: &str = "ORR X0, XZR, 1";

#[derive(Debug, PartialEq)]
enum Instruction {
    // Minimal
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
    SVC {
        syscall: Operand,
    },
}
#[derive(Debug, PartialEq)]
enum Operand {
    // Hardcoded only to start with
    Reg(Reg),
    Imm(u32), // TODO implement Uimm (unisgned) and Simm (signed) for use with ADD and B relative adressing
    Addr(u64),
    Label(String),
}

#[derive(Debug, PartialEq)]
#[repr(u32)]
enum Reg {
    // keeping only 64 bit registers for simplicity easy to implement 32 bit
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    // .... \\
    XZR = 31,
}

fn parse_register(reg: &str) -> Reg {
    match reg.to_uppercase().as_str() {
        "X0" => Reg::X0,
        "X1" => Reg::X1,
        "X2" => Reg::X2,
        "X3" => Reg::X3,
        // .... \\
        "XZR" => Reg::XZR,
        _ => panic!("Not a valid register"),
    }
}

fn encode_logical_immediate64(val: u32) -> Result<(u32,u32,u32), &'static str> {
    /*
    Thanks to Dougall J: Bit-Twiddling: Optimising AArch64 Logical Immediate Encoding (and Decoding)
    https://dougallj.wordpress.com/2021/10/30/bit-twiddling-optimising-aarch64-logical-immediate-encoding-and-decoding/
     */
    if val == 0 || val == !0u32 { // these values cannot be encoded like this
        return Err("Can't encode all zeros or all ones");
    }
    // calculate amount to rotate
    let rotation = (val & (val +1 )).trailing_zeros();

    let normalized = val.rotate_right(rotation);

    let zeros= normalized.leading_zeros();
    let ones= (!normalized).trailing_zeros();

    let size = zeros + ones;

    if val.rotate_right(size) != val {
        return Err("Value doesn't form a repeatable pattern");
    }

    let n = 0; // for sf=1 as we are doing 64bit operations
    let immr    = (rotation &0x3F) as u32;
    let imms    = ((1 << ones)-1) as u32;

    Ok((n, immr,imms))
}

fn encode_orr_imm(op: Instruction) -> u32{
    let mut encoding: u32 = 0b10110010_00000000_00000000_00000000;

    encoding |= 1 << 31; // sf=1 since we are using X registers (64 bit)

    match op{
        Instruction::ORR { dest, src1, src2} => {
            if let Operand::Reg(reg) = dest {
                let xd_value = reg as u32;
                encoding |= xd_value & 0x1F;
            }else {
                panic!("Not a register")
            }

            if let Operand::Reg(reg) = src1 {
                let xn_value = reg as u32;
                encoding |= (xn_value & 0x1F) << 5; // bits 5-9
            }else {
                panic!("Not a register")
            } 

            if let Operand::Imm(val ) = src2 {
                match encode_logical_immediate64(val){
                    Ok((n, immr,imms)) => {
                        encoding |= n << 20;

                        encoding |= immr << 16;

                        encoding |= imms << 10;
                    }

                    Err(msg) => {
                        panic!("Cannot encode {}: {}", val, msg)
                    }

                }
            }
        }
        _ => panic!("Failed to encode")
    }
 encoding
}

fn parse_line(line: &str) -> Option<Instruction> {
    let line: Vec<&str> = line.split_whitespace().collect();

    match line.as_slice() {
        // as_slice don't quite get why
        // Auto matches the arguments pretty well.
        ["ORR", dest, src1, src2] => dbg!(Some(Instruction::ORR {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            src1: if src1.starts_with('X') {
                Operand::Reg(parse_register(src1.trim_end_matches(',')))
            } else {
                Operand::Imm(src1.trim_end_matches(',').parse().unwrap())
            },

            src2: if src2.starts_with('X') {
                Operand::Reg(parse_register(src2.trim_end_matches(',')))
            } else {
                Operand::Imm(src2.trim_end_matches(',').parse().unwrap())
            },
        })),

        ["ADD", dest, src1, src2] => dbg!(Some(Instruction::ADD {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            src1: if src1.starts_with('X') {
                Operand::Reg(parse_register(src1.trim_end_matches(',')))
            } else {
                Operand::Imm(src1.trim_end_matches(',').parse().unwrap())
            },

            src2: if src2.starts_with('X') {
                Operand::Reg(parse_register(src2.trim_end_matches(',')))
            } else {
                Operand::Imm(src2.trim_end_matches(',').parse().unwrap())
            },
        })),

        ["B", addr] => dbg!(Some(Instruction::B {
            addr: if addr.starts_with("0x") {
                Operand::Addr(u64::from_str_radix(addr.trim_start_matches("0x"), 16).unwrap())
            } else if addr.starts_with('.') {
                Operand::Label(addr.to_string())
            }
            // TODO add relative adressing
            else {
                panic!("Invalid Adress")
            }
        })),

        ["SVC", syscall] => dbg!(Some(Instruction::SVC {
            syscall: if syscall.starts_with('#') {
                Operand::Imm(dbg!(
                    u32::from_str_radix(
                        syscall.trim_start_matches('#').trim_start_matches("0x"),
                        16
                    )
                    .unwrap()
                ))
            } else {
                panic!("Syntax error # for imm");
            }
        })),

        _ => None,
    }
}

fn main() {
    println!("0x{:032b}", encode_orr_imm(parse_line(TEST_LINE).unwrap()));
}

// wrote with perplexity idk how I feel about that
#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn test_orr() {
        let input = "ORR X0, XZR, 1";
        let expected = Some(Instruction::ORR {
            dest: Operand::Reg(Reg::X0),
            src1: Operand::Reg(Reg::XZR),
            src2: Operand::Imm(1),
        });
        assert_eq!(parse_line(input), expected);
    }

    #[test]
    fn test_add() {
        let input = "ADD X0, X0, 5";
        let expected = Some(Instruction::ADD {
            dest: Operand::Reg(Reg::X0),
            src1: Operand::Reg(Reg::X0),
            src2: Operand::Imm(5),
        });
        assert_eq!(parse_line(input), expected);
    }

    #[test]
    fn test_svc() {
        let input = "SVC #0x80";
        let expected = Some(Instruction::SVC {
            syscall: Operand::Imm(128), // 0x80 in decimal
        });
        assert_eq!(parse_line(input), expected);
    }

    #[test]
    fn test_b_address() {
        let input = "B 0x4000";
        let expected = Some(Instruction::B {
            addr: Operand::Addr(0x4000),
        });
        assert_eq!(parse_line(input), expected);
    }

    #[test]
    fn test_b_label() {
        let input = "B .loop";
        let expected = Some(Instruction::B {
            addr: Operand::Label(".loop".to_string()),
        });
        assert_eq!(parse_line(input), expected);
    }

    #[test]
    fn test_invalid_instruction() {
        let input = "INVALID X0, X1, X2";
        assert_eq!(parse_line(input), None);
    }
}
