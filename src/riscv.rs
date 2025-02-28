use super::*;

fn get_funct3(inst: i32) -> i32 {
    (inst >> 12) & 0x07
}

fn get_rd(inst: i32) -> usize {
    ((inst >> 7) & 0x1f) as usize
}

fn get_rs1(inst: i32) -> usize {
    ((inst >> 15) & 0x1f) as usize
}

fn get_rs2(inst: i32) -> usize {
    ((inst >> 20) & 0x1f) as usize
}

fn get_imm_i(inst: i32) -> i64 {
    (inst >> 20) as i64
}

fn get_imm_s(inst: i32) -> i64 {
    let mut imm = (inst >> 20) & !0x0000001f;
    imm |= (inst >> 7) & 0x0000001f;
    imm as i64
}

fn get_imm_b(inst: i32) -> i64 {
    let mut imm = (inst >> 20) & !0x00000fff;
    imm |= (inst << 4) & 0x00000800;
    imm |= (inst >> 20) & 0x000007e0;
    imm |= (inst >> 7) & 0x0000001e;
    imm as i64
}

fn get_imm_u(inst: i32) -> i64 {
    (inst & !0x00000fff) as i64
}

fn get_imm_j(inst: i32) -> i64 {
    let mut imm = (inst >> 11) & !0x000fffff;
    imm |= inst & 0x000ff000;
    imm |= (inst >> 9) & 0x00000800;
    imm |= (inst >> 20) & 0x000007e0;
    imm |= (inst >> 20) & 0x0000001e;
    imm as i64
}

fn get_funct7(inst: i32) -> i32 {
    inst >> 25
}

pub const R: [&str; 32] = [
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2",
    "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6",
];

pub const ZERO: usize = 0;
pub const RA: usize = 1;
pub const SP: usize = 2;
pub const GP: usize = 3;
pub const A0: usize = 10;
pub const A1: usize = 11;
pub const A2: usize = 12;

pub const A_REGS: [usize; 8] = [10, 11, 12, 13, 14, 15, 16, 17];
pub const T_REGS: [usize; 7] = [5, 6, 7, 28, 29, 30, 31];
pub const S_REGS: [usize; 12] = [8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27];

pub enum Op {
    // r-type
    Add { rd: usize, rs1: usize, rs2: usize },
    Sub { rd: usize, rs1: usize, rs2: usize },
    Sll { rd: usize, rs1: usize, rs2: usize },
    Slt { rd: usize, rs1: usize, rs2: usize },
    Sltu { rd: usize, rs1: usize, rs2: usize },
    Xor { rd: usize, rs1: usize, rs2: usize },
    Srl { rd: usize, rs1: usize, rs2: usize },
    Sra { rd: usize, rs1: usize, rs2: usize },
    Or { rd: usize, rs1: usize, rs2: usize },
    And { rd: usize, rs1: usize, rs2: usize },

    // rv64-specific r-type
    Addw { rd: usize, rs1: usize, rs2: usize },
    Subw { rd: usize, rs1: usize, rs2: usize },
    Sllw { rd: usize, rs1: usize, rs2: usize },
    Srlw { rd: usize, rs1: usize, rs2: usize },
    Sraw { rd: usize, rs1: usize, rs2: usize },

    // i-type
    Addi { rd: usize, rs1: usize, imm: i64 },
    Slti { rd: usize, rs1: usize, imm: i64 },
    Sltiu { rd: usize, rs1: usize, imm: i64 },
    Xori { rd: usize, rs1: usize, imm: i64 },
    Ori { rd: usize, rs1: usize, imm: i64 },
    Andi { rd: usize, rs1: usize, imm: i64 },
    Slli { rd: usize, rs1: usize, shamt: i64 },
    Srli { rd: usize, rs1: usize, shamt: i64 },
    Srai { rd: usize, rs1: usize, shamt: i64 },

    // rv64-specific i-type
    Addiw { rd: usize, rs1: usize, imm: i64 },
    Slliw { rd: usize, rs1: usize, shamt: i64 },
    Srliw { rd: usize, rs1: usize, shamt: i64 },
    Sraiw { rd: usize, rs1: usize, shamt: i64 },

    // branch
    Beq { rs1: usize, rs2: usize, offset: i64 },
    Bne { rs1: usize, rs2: usize, offset: i64 },
    Blt { rs1: usize, rs2: usize, offset: i64 },
    Bge { rs1: usize, rs2: usize, offset: i64 },
    Bltu { rs1: usize, rs2: usize, offset: i64 },
    Bgeu { rs1: usize, rs2: usize, offset: i64 },

    // jump
    Jal { rd: usize, offset: i64 },
    Jalr { rd: usize, rs1: usize, offset: i64 },

    // load
    Lb { rd: usize, rs1: usize, offset: i64 },
    Lh { rd: usize, rs1: usize, offset: i64 },
    Lw { rd: usize, rs1: usize, offset: i64 },
    Ld { rd: usize, rs1: usize, offset: i64 },
    Lbu { rd: usize, rs1: usize, offset: i64 },
    Lhu { rd: usize, rs1: usize, offset: i64 },
    Lwu { rd: usize, rs1: usize, offset: i64 },

    // store
    Sb { rs1: usize, rs2: usize, offset: i64 },
    Sh { rs1: usize, rs2: usize, offset: i64 },
    Sw { rs1: usize, rs2: usize, offset: i64 },
    Sd { rs1: usize, rs2: usize, offset: i64 },

    // u-type
    Lui { rd: usize, imm: i64 },
    Auipc { rd: usize, imm: i64 },

    // misc
    Fence,
    Ecall,
    Ebreak,

    // m extension
    Mul { rd: usize, rs1: usize, rs2: usize },
    Mulh { rd: usize, rs1: usize, rs2: usize },
    Mulhsu { rd: usize, rs1: usize, rs2: usize },
    Mulhu { rd: usize, rs1: usize, rs2: usize },
    Div { rd: usize, rs1: usize, rs2: usize },
    Divu { rd: usize, rs1: usize, rs2: usize },
    Rem { rd: usize, rs1: usize, rs2: usize },
    Remu { rd: usize, rs1: usize, rs2: usize },

    // m extension rv64-specific
    Mulw { rd: usize, rs1: usize, rs2: usize },
    Divw { rd: usize, rs1: usize, rs2: usize },
    Divuw { rd: usize, rs1: usize, rs2: usize },
    Remw { rd: usize, rs1: usize, rs2: usize },
    Remuw { rd: usize, rs1: usize, rs2: usize },
}

impl Op {
    pub fn new(inst: i32) -> Result<Self, String> {
        // get the opcode
        let opcode = inst & 0x7f;

        match opcode {
            // r-type (including m extension)
            0x33 => Self::decode_r_type(inst),

            // rv64-specific r-type (include m extension)
            0x3b => Self::decode_rv64_r_type(inst),

            // i-type
            0x13 => Self::decode_i_type(inst),

            // rv64-specific i-type
            0x1b => Self::decode_rv64_i_type(inst),

            // branch
            0x63 => Self::decode_branches(inst),

            // jump
            0x6f => Ok(Op::Jal { rd: get_rd(inst), offset: get_imm_j(inst) }),
            0x67 => {
                let funct3 = get_funct3(inst);
                if funct3 == 0 {
                    Ok(Op::Jalr { rd: get_rd(inst), rs1: get_rs1(inst), offset: get_imm_i(inst) })
                } else {
                    Err(format!("jalr with unknown funct3 value of {}", funct3))
                }
            }

            // load
            0x03 => Self::decode_load(inst),

            // store
            0x23 => Self::decode_store(inst),

            // u type
            0x37 => Ok(Op::Lui { rd: get_rd(inst), imm: get_imm_u(inst) }),
            0x17 => Ok(Op::Auipc { rd: get_rd(inst), imm: get_imm_u(inst) }),

            // misc
            0x0f => Ok(Self::Fence),
            0x73 if inst == 0x00000073 => Ok(Self::Ecall),
            0x73 if inst == 0x00100073 => Ok(Self::Ebreak),

            _ => Err(format!("disassembler found unknown instruction {:#x}", inst)),
        }
    }

    fn decode_branches(inst: i32) -> Result<Self, String> {
        let funct3 = get_funct3(inst);
        let rs1 = get_rs1(inst);
        let rs2 = get_rs2(inst);
        let offset = get_imm_b(inst);

        match funct3 {
            0 => Ok(Op::Beq { rs1, rs2, offset }),
            1 => Ok(Op::Bne { rs1, rs2, offset }),
            4 => Ok(Op::Blt { rs1, rs2, offset }),
            5 => Ok(Op::Bge { rs1, rs2, offset }),
            6 => Ok(Op::Bltu { rs1, rs2, offset }),
            7 => Ok(Op::Bgeu { rs1, rs2, offset }),
            _ => Err(format!("branch instruction of unknown type {}", funct3)),
        }
    }

    fn decode_load(inst: i32) -> Result<Self, String> {
        let funct3 = get_funct3(inst);
        let rd = get_rd(inst);
        let rs1 = get_rs1(inst);
        let offset = get_imm_i(inst);

        match funct3 {
            0 => Ok(Op::Lb { rd, rs1, offset }),
            1 => Ok(Op::Lh { rd, rs1, offset }),
            2 => Ok(Op::Lw { rd, rs1, offset }),
            3 => Ok(Op::Ld { rd, rs1, offset }),
            4 => Ok(Op::Lbu { rd, rs1, offset }),
            5 => Ok(Op::Lhu { rd, rs1, offset }),
            6 => Ok(Op::Lwu { rd, rs1, offset }),
            _ => Err(format!("load instruction of unknown type {}", funct3)),
        }
    }

    fn decode_store(inst: i32) -> Result<Self, String> {
        let funct3 = get_funct3(inst);
        let rs1 = get_rs1(inst);
        let rs2 = get_rs2(inst);
        let offset = get_imm_s(inst);

        match funct3 {
            0 => Ok(Op::Sb { rs1, rs2, offset }),
            1 => Ok(Op::Sh { rs1, rs2, offset }),
            2 => Ok(Op::Sw { rs1, rs2, offset }),
            3 => Ok(Op::Sd { rs1, rs2, offset }),
            _ => Err(format!("store of unknown type {}", funct3)),
        }
    }

    fn decode_i_type(inst: i32) -> Result<Self, String> {
        let funct3 = get_funct3(inst);
        let rd = get_rd(inst);
        let rs1 = get_rs1(inst);
        let imm = get_imm_i(inst);
        let shamt = imm & 0x3f;
        let imm_high = imm >> 6;

        match funct3 {
            0 => Ok(Op::Addi { rd, rs1, imm }),
            1 => {
                if imm_high == 0 {
                    Ok(Op::Slli { rd, rs1, shamt })
                } else {
                    Err(format!("immediate mode alu instruction of type {} with unknown subtype {}", funct3, imm_high))
                }
            }
            2 => Ok(Op::Slti { rd, rs1, imm }),
            3 => Ok(Op::Sltiu { rd, rs1, imm }),
            4 => Ok(Op::Xori { rd, rs1, imm }),
            5 => match imm_high {
                0x00 => Ok(Op::Srli { rd, rs1, shamt }),
                0x10 => Ok(Op::Srai { rd, rs1, shamt }),
                _ => {
                    Err(format!("immediate mode alu instruction of type {} with unknown subtype {}", funct3, imm_high))
                }
            },
            6 => Ok(Op::Ori { rd, rs1, imm }),
            7 => Ok(Op::Andi { rd, rs1, imm }),
            _ => Err(format!("alu immediate of unknown type {}", funct3)),
        }
    }

    fn decode_rv64_i_type(inst: i32) -> Result<Self, String> {
        let funct3 = get_funct3(inst);
        let rd = get_rd(inst);
        let rs1 = get_rs1(inst);
        let imm = get_imm_i(inst);
        let shamt = imm & 0x1f;
        let imm_high = imm >> 5;

        match funct3 {
            0 => Ok(Op::Addiw { rd, rs1, imm }),
            1 => Ok(Op::Slliw { rd, rs1, shamt }),
            5 => match imm_high {
                0x00 => Ok(Op::Srliw { rd, rs1, shamt }),
                0x20 => Ok(Op::Sraiw { rd, rs1, shamt }),
                _ => Err(format!(
                    "immediate mode alu w instruction of type {} with unknown subtype {}",
                    funct3, imm_high
                )),
            },
            _ => Err(format!("immediate mode alu w instruction of unknown type {}", funct3)),
        }
    }

    fn decode_r_type(inst: i32) -> Result<Self, String> {
        let funct3 = get_funct3(inst);
        let funct7 = get_funct7(inst);
        let rd = get_rd(inst);
        let rs1 = get_rs1(inst);
        let rs2 = get_rs2(inst);

        match (funct7, funct3) {
            (0x00, 0x00) => Ok(Op::Add { rd, rs1, rs2 }),
            (0x20, 0x00) => Ok(Op::Sub { rd, rs1, rs2 }),
            (0x00, 0x01) => Ok(Op::Sll { rd, rs1, rs2 }),
            (0x00, 0x02) => Ok(Op::Slt { rd, rs1, rs2 }),
            (0x00, 0x03) => Ok(Op::Sltu { rd, rs1, rs2 }),
            (0x00, 0x04) => Ok(Op::Xor { rd, rs1, rs2 }),
            (0x00, 0x05) => Ok(Op::Srl { rd, rs1, rs2 }),
            (0x20, 0x05) => Ok(Op::Sra { rd, rs1, rs2 }),
            (0x00, 0x06) => Ok(Op::Or { rd, rs1, rs2 }),
            (0x00, 0x07) => Ok(Op::And { rd, rs1, rs2 }),

            (0x01, 0x00) => Ok(Op::Mul { rd, rs1, rs2 }),
            (0x01, 0x01) => Ok(Op::Mulh { rd, rs1, rs2 }),
            (0x01, 0x02) => Ok(Op::Mulhsu { rd, rs1, rs2 }),
            (0x01, 0x03) => Ok(Op::Mulhu { rd, rs1, rs2 }),
            (0x01, 0x04) => Ok(Op::Div { rd, rs1, rs2 }),
            (0x01, 0x05) => Ok(Op::Divu { rd, rs1, rs2 }),
            (0x01, 0x06) => Ok(Op::Rem { rd, rs1, rs2 }),
            (0x01, 0x07) => Ok(Op::Remu { rd, rs1, rs2 }),

            _ => Err(format!("alu instruction of unknown type {} subtype {}", funct3, funct7)),
        }
    }

    fn decode_rv64_r_type(inst: i32) -> Result<Self, String> {
        let funct3 = get_funct3(inst);
        let funct7 = get_funct7(inst);
        let rd = get_rd(inst);
        let rs1 = get_rs1(inst);
        let rs2 = get_rs2(inst);

        match (funct7, funct3) {
            (0x00, 0x00) => Ok(Op::Addw { rd, rs1, rs2 }),
            (0x20, 0x00) => Ok(Op::Subw { rd, rs1, rs2 }),
            (0x00, 0x01) => Ok(Op::Sllw { rd, rs1, rs2 }),
            (0x00, 0x05) => Ok(Op::Srlw { rd, rs1, rs2 }),
            (0x20, 0x05) => Ok(Op::Sraw { rd, rs1, rs2 }),

            (0x01, 0x00) => Ok(Op::Mulw { rd, rs1, rs2 }),
            (0x01, 0x04) => Ok(Op::Divw { rd, rs1, rs2 }),
            (0x01, 0x05) => Ok(Op::Divuw { rd, rs1, rs2 }),
            (0x01, 0x06) => Ok(Op::Remw { rd, rs1, rs2 }),
            (0x01, 0x07) => Ok(Op::Remuw { rd, rs1, rs2 }),

            _ => Err(format!("alu w instruction of unknown type {} subtype {}", funct3, funct7)),
        }
    }

    pub fn execute(&self, m: &mut Machine) -> Result<(), String> {
        match self {
            // r-type
            Op::Add { rd, rs1, rs2 } => {
                let val = m.get(*rs1).wrapping_add(m.get(*rs2));
                m.set(*rd, val);
            }
            Op::Sub { rd, rs1, rs2 } => {
                let val = m.get(*rs1).wrapping_sub(m.get(*rs2));
                m.set(*rd, val);
            }
            Op::Sll { rd, rs1, rs2 } => {
                let rs2_val = m.get(*rs2) & 0x3f;
                let val = m.get(*rs1) << rs2_val;
                m.set(*rd, val);
            }
            Op::Slt { rd, rs1, rs2 } => {
                let val = if m.get(*rs1) < m.get(*rs2) { 1 } else { 0 };
                m.set(*rd, val);
            }
            Op::Sltu { rd, rs1, rs2 } => {
                let val = if (m.get(*rs1) as u64) < (m.get(*rs2) as u64) { 1 } else { 0 };
                m.set(*rd, val);
            }
            Op::Xor { rd, rs1, rs2 } => {
                let val = m.get(*rs1) ^ m.get(*rs2);
                m.set(*rd, val);
            }
            Op::Srl { rd, rs1, rs2 } => {
                let rs2_val = m.get(*rs2) & 0x3f;
                let val = ((m.get(*rs1) as u64) >> rs2_val) as i64;
                m.set(*rd, val);
            }
            Op::Sra { rd, rs1, rs2 } => {
                let rs2_val = m.get(*rs2) & 0x3f;
                let val = m.get(*rs1) >> rs2_val;
                m.set(*rd, val);
            }
            Op::Or { rd, rs1, rs2 } => {
                let val = m.get(*rs1) | m.get(*rs2);
                m.set(*rd, val);
            }
            Op::And { rd, rs1, rs2 } => {
                let val = m.get(*rs1) & m.get(*rs2);
                m.set(*rd, val);
            }

            // rv64-specific r-type
            Op::Addw { rd, rs1, rs2 } => {
                let val = m.get32(*rs1).wrapping_add(m.get32(*rs2));
                m.set32(*rd, val);
            }
            Op::Subw { rd, rs1, rs2 } => {
                let val = m.get32(*rs1).wrapping_sub(m.get32(*rs2));
                m.set32(*rd, val);
            }
            Op::Sllw { rd, rs1, rs2 } => {
                let rs2_val = m.get32(*rs2) & 0x1f;
                let val = m.get32(*rs1) << rs2_val;
                m.set32(*rd, val);
            }
            Op::Srlw { rd, rs1, rs2 } => {
                let rs2_val = m.get32(*rs2) & 0x1f;
                let val = ((m.get32(*rs1) as u32) >> rs2_val) as i32;
                m.set32(*rd, val);
            }
            Op::Sraw { rd, rs1, rs2 } => {
                let rs2_val = m.get32(*rs2) & 0x1f;
                let val = m.get32(*rs1) >> rs2_val;
                m.set32(*rd, val);
            }

            // i-type
            Op::Addi { rd, rs1, imm } => {
                let val = m.get(*rs1).wrapping_add(*imm);
                m.set(*rd, val);
            }
            Op::Slti { rd, rs1, imm } => {
                let val = if m.get(*rs1) < *imm { 1 } else { 0 };
                m.set(*rd, val);
            }
            Op::Sltiu { rd, rs1, imm } => {
                let val = if (m.get(*rs1) as u64) < (*imm as u64) { 1 } else { 0 };
                m.set(*rd, val);
            }
            Op::Xori { rd, rs1, imm } => {
                let val = m.get(*rs1) ^ *imm;
                m.set(*rd, val);
            }
            Op::Ori { rd, rs1, imm } => {
                let val = m.get(*rs1) | *imm;
                m.set(*rd, val);
            }
            Op::Andi { rd, rs1, imm } => {
                let val = m.get(*rs1) & *imm;
                m.set(*rd, val);
            }
            Op::Slli { rd, rs1, shamt } => {
                let val = m.get(*rs1) << *shamt;
                m.set(*rd, val);
            }
            Op::Srli { rd, rs1, shamt } => {
                let val = ((m.get(*rs1) as u64) >> *shamt) as i64;
                m.set(*rd, val);
            }
            Op::Srai { rd, rs1, shamt } => {
                let val = m.get(*rs1) >> *shamt;
                m.set(*rd, val);
            }

            // rv64-specific i-type
            Op::Addiw { rd, rs1, imm } => {
                let val = m.get32(*rs1).wrapping_add(*imm as i32);
                m.set32(*rd, val);
            }
            Op::Slliw { rd, rs1, shamt } => {
                let val = m.get32(*rs1) << (*shamt as i32);
                m.set32(*rd, val);
            }
            Op::Srliw { rd, rs1, shamt } => {
                let val = ((m.get32(*rs1) as u32) >> (*shamt as i32)) as i32;
                m.set32(*rd, val);
            }
            Op::Sraiw { rd, rs1, shamt } => {
                let val = m.get32(*rs1) >> (*shamt as i32);
                m.set32(*rd, val);
            }

            // branch
            Op::Beq { rs1, rs2, offset } => {
                if m.get(*rs1) == m.get(*rs2) {
                    m.set_pc(m.pc + *offset)?;
                }
            }
            Op::Bne { rs1, rs2, offset } => {
                if m.get(*rs1) != m.get(*rs2) {
                    m.set_pc(m.pc + *offset)?;
                }
            }
            Op::Blt { rs1, rs2, offset } => {
                if m.get(*rs1) < m.get(*rs2) {
                    m.set_pc(m.pc + *offset)?;
                }
            }
            Op::Bge { rs1, rs2, offset } => {
                if m.get(*rs1) >= m.get(*rs2) {
                    m.set_pc(m.pc + *offset)?;
                }
            }
            Op::Bltu { rs1, rs2, offset } => {
                if (m.get(*rs1) as u64) < (m.get(*rs2) as u64) {
                    m.set_pc(m.pc + *offset)?;
                }
            }
            Op::Bgeu { rs1, rs2, offset } => {
                if (m.get(*rs1) as u64) >= (m.get(*rs2) as u64) {
                    m.set_pc(m.pc + *offset)?;
                }
            }

            // jump
            Op::Jal { rd, offset } => {
                m.set(*rd, m.pc + 4);
                m.set_pc(m.pc + *offset)?;
            }
            Op::Jalr { rd, rs1, offset } => {
                let rs1_val = m.get(*rs1);
                m.set(*rd, m.pc + 4);
                m.set_pc((rs1_val + *offset) & !1)?;
            }

            // load
            Op::Lb { rd, rs1, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let val = m.load_i8(effective_address)?;
                m.set(*rd, val);
            }
            Op::Lh { rd, rs1, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let val = m.load_i16(effective_address)?;
                m.set(*rd, val);
            }
            Op::Lw { rd, rs1, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let val = m.load_i32(effective_address)?;
                m.set(*rd, val);
            }
            Op::Ld { rd, rs1, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let val = m.load_i64(effective_address)?;
                m.set(*rd, val);
            }
            Op::Lbu { rd, rs1, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let val = m.load_u8(effective_address)?;
                m.set(*rd, val);
            }
            Op::Lhu { rd, rs1, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let val = m.load_u16(effective_address)?;
                m.set(*rd, val);
            }
            Op::Lwu { rd, rs1, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let val = m.load_u32(effective_address)?;
                m.set(*rd, val);
            }

            // store
            Op::Sb { rs1, rs2, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let raw = (m.get(*rs2) as u8).to_le_bytes();
                m.store(effective_address, &raw)?;
            }
            Op::Sh { rs1, rs2, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let raw = (m.get(*rs2) as u16).to_le_bytes();
                m.store(effective_address, &raw)?;
            }
            Op::Sw { rs1, rs2, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let raw = (m.get(*rs2) as u32).to_le_bytes();
                m.store(effective_address, &raw)?;
            }
            Op::Sd { rs1, rs2, offset } => {
                let effective_address = m.get(*rs1) + *offset;
                let raw = m.get(*rs2).to_le_bytes();
                m.store(effective_address, &raw)?;
            }

            // u-type
            Op::Lui { rd, imm } => {
                m.set(*rd, *imm);
            }
            Op::Auipc { rd, imm } => {
                m.set(*rd, m.pc + *imm);
            }

            // misc
            Op::Fence => {
                // treat fence as a no-op
            }
            Op::Ecall => {
                match m.get(17) {
                    63 => {
                        // read system call
                        m.effects.as_mut().unwrap().other_message =
                            Some(format!("read({}, 0x{:x}, {})", m.get(10), m.get(11), m.get(12)));
                        let fd = m.get(A0);
                        let buf_addr = m.get(A1);
                        let count = m.get(A2);

                        if fd != 0 {
                            return Err(format!("read syscall: only stdin (fd 0) supported, not {fd}"));
                        }
                        if count < 0 {
                            return Err(format!("read syscall: invalid buffer size: {count}"));
                        }

                        // make a buffer and read from stdin
                        let mut read_buffer = vec![0; count as usize];
                        let mut handle = io::stdin().lock();
                        match handle.read(&mut read_buffer) {
                            Ok(n) => read_buffer.truncate(n),
                            Err(e) => return Err(format!("read syscall error: {}", e)),
                        }

                        m.store(buf_addr, &read_buffer)?;
                        m.set(A0, read_buffer.len() as i64);
                        m.stdin.extend_from_slice(&read_buffer);
                        m.effects.as_mut().unwrap().stdin = Some(read_buffer);
                    }
                    64 => {
                        // write system call
                        m.effects.as_mut().unwrap().other_message =
                            Some(format!("write({}, 0x{:x}, {})", m.get(A0), m.get(A1), m.get(A2)));
                        let fd = m.get(A0);
                        let buf_addr = m.get(11);
                        let count = m.get(12);

                        if fd != 1 {
                            return Err(format!("write syscall: only stdout (fd 1) supported, not {fd}"));
                        }
                        if count < 0 {
                            return Err(format!("write syscall: invalid buffer size: {count}"));
                        }

                        let write_buffer = m.load(buf_addr, count)?;
                        m.set(A0, write_buffer.len() as i64);
                        m.stdout.extend_from_slice(&write_buffer);
                        m.effects.as_mut().unwrap().stdout = Some(write_buffer);
                    }
                    93 => {
                        // exit system call
                        let status = m.get(A0) & 0xff;
                        let effects = m.effects.as_mut().unwrap();
                        effects.error(format!("exit({})", status));
                    }
                    syscall => return Err(format!("unsupported syscall {syscall}")),
                }
            }
            Op::Ebreak => {
                let effects = m.effects.as_mut().unwrap();
                effects.error(String::from("ebreak"));
            }

            // m extension
            Op::Mul { rd, rs1, rs2 } => {
                let val = m.get(*rs1).wrapping_mul(m.get(*rs2));
                m.set(*rd, val);
            }
            Op::Mulh { rd, rs1, rs2 } => {
                let val = ((m.get(*rs1) as i128 * m.get(*rs2) as i128) >> 64) as i64;
                m.set(*rd, val);
            }
            Op::Mulhsu { rd, rs1, rs2 } => {
                let val = ((m.get(*rs1) as i128 * (m.get(*rs2) as u64) as i128) >> 64) as i64;
                m.set(*rd, val);
            }
            Op::Mulhu { rd, rs1, rs2 } => {
                let val = (((m.get(*rs1) as u64) as u128 * (m.get(*rs2) as u64) as u128) >> 64) as i64;
                m.set(*rd, val);
            }
            Op::Div { rd, rs1, rs2 } => {
                let rs2_val = m.get(*rs2);
                let val = if rs2_val == 0 { -1 } else { m.get(*rs1).wrapping_div(rs2_val) };
                m.set(*rd, val);
            }
            Op::Divu { rd, rs1, rs2 } => {
                let rs2_val = m.get(*rs2) as u64;
                let val = if rs2_val == 0 { -1 } else { (m.get(*rs1) as u64).wrapping_div(rs2_val) as i64 };
                m.set(*rd, val);
            }
            Op::Rem { rd, rs1, rs2 } => {
                let rs2_val = m.get(*rs2);
                let val = if rs2_val == 0 { m.get(*rs1) } else { m.get(*rs1).wrapping_rem(rs2_val) };
                m.set(*rd, val);
            }
            Op::Remu { rd, rs1, rs2 } => {
                let rs2_val = m.get(*rs2) as u64;
                let val = if rs2_val == 0 { m.get(*rs1) } else { (m.get(*rs1) as u64).wrapping_rem(rs2_val) as i64 };
                m.set(*rd, val);
            }

            // m extension rv64-specific
            Op::Mulw { rd, rs1, rs2 } => {
                let val = m.get32(*rs1).wrapping_mul(m.get32(*rs2));
                m.set32(*rd, val);
            }
            Op::Divw { rd, rs1, rs2 } => {
                let rs2_val = m.get32(*rs2);
                let val = if rs2_val == 0 { -1 } else { m.get32(*rs1).wrapping_div(rs2_val) };
                m.set32(*rd, val);
            }
            Op::Divuw { rd, rs1, rs2 } => {
                let rs2_val = m.get32(*rs2) as u32;
                let val = if rs2_val == 0 { -1 } else { (m.get32(*rs1) as u32).wrapping_div(rs2_val) as i32 };
                m.set32(*rd, val);
            }
            Op::Remw { rd, rs1, rs2 } => {
                let rs2_val = m.get32(*rs2);
                let val = if rs2_val == 0 { m.get32(*rs1) } else { m.get32(*rs1).wrapping_rem(rs2_val) };
                m.set32(*rd, val);
            }
            Op::Remuw { rd, rs1, rs2 } => {
                let rs2_val = m.get32(*rs2) as u32;
                let val =
                    if rs2_val == 0 { m.get32(*rs1) } else { (m.get32(*rs1) as u32).wrapping_rem(rs2_val) as i32 };
                m.set32(*rd, val);
            }
        }
        Ok(())
    }

    pub fn to_fields(&self) -> Vec<Field> {
        match *self {
            // r-type
            Op::Add { rd, rs1, rs2 } => vec![Field::Opcode("add"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Sub { rd, rs1, rs2 } => vec![Field::Opcode("sub"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Sll { rd, rs1, rs2 } => vec![Field::Opcode("sll"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Slt { rd, rs1, rs2 } => vec![Field::Opcode("slt"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Sltu { rd, rs1, rs2 } => vec![Field::Opcode("sltu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Xor { rd, rs1, rs2 } => vec![Field::Opcode("xor"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Srl { rd, rs1, rs2 } => vec![Field::Opcode("srl"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Sra { rd, rs1, rs2 } => vec![Field::Opcode("sra"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Or { rd, rs1, rs2 } => vec![Field::Opcode("or"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::And { rd, rs1, rs2 } => vec![Field::Opcode("and"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],

            // rv64-specific r-type
            Op::Addw { rd, rs1, rs2 } => vec![Field::Opcode("addw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Subw { rd, rs1, rs2 } => vec![Field::Opcode("subw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Sllw { rd, rs1, rs2 } => vec![Field::Opcode("sllw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Srlw { rd, rs1, rs2 } => vec![Field::Opcode("srlw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Sraw { rd, rs1, rs2 } => vec![Field::Opcode("sraw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],

            // i-type instructions
            Op::Addi { rd, rs1, imm } => vec![Field::Opcode("addi"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)],
            Op::Slti { rd, rs1, imm } => vec![Field::Opcode("slti"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)],
            Op::Sltiu { rd, rs1, imm } => {
                vec![Field::Opcode("sltiu"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)]
            }
            Op::Xori { rd, rs1, imm } => vec![Field::Opcode("xori"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)],
            Op::Ori { rd, rs1, imm } => vec![Field::Opcode("ori"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)],
            Op::Andi { rd, rs1, imm } => vec![Field::Opcode("andi"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)],
            Op::Slli { rd, rs1, shamt } => {
                vec![Field::Opcode("slli"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)]
            }
            Op::Srli { rd, rs1, shamt } => {
                vec![Field::Opcode("srli"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)]
            }
            Op::Srai { rd, rs1, shamt } => {
                vec![Field::Opcode("srai"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)]
            }

            // rv64-specific i-type
            Op::Addiw { rd, rs1, imm } => {
                vec![Field::Opcode("addiw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)]
            }
            Op::Slliw { rd, rs1, shamt } => {
                vec![Field::Opcode("slliw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)]
            }
            Op::Srliw { rd, rs1, shamt } => {
                vec![Field::Opcode("srliw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)]
            }
            Op::Sraiw { rd, rs1, shamt } => {
                vec![Field::Opcode("sraiw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)]
            }

            // branch
            Op::Beq { rs1, rs2, offset } => {
                vec![Field::Opcode("beq"), Field::Reg(rs1), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Bne { rs1, rs2, offset } => {
                vec![Field::Opcode("bne"), Field::Reg(rs1), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Blt { rs1, rs2, offset } => {
                vec![Field::Opcode("blt"), Field::Reg(rs1), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Bge { rs1, rs2, offset } => {
                vec![Field::Opcode("bge"), Field::Reg(rs1), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Bltu { rs1, rs2, offset } => {
                vec![Field::Opcode("bltu"), Field::Reg(rs1), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Bgeu { rs1, rs2, offset } => {
                vec![Field::Opcode("bgeu"), Field::Reg(rs1), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }

            // jump
            Op::Jal { rd, offset } => vec![Field::Opcode("jal"), Field::Reg(rd), Field::PCRelAddr(offset)],
            Op::Jalr { rd, rs1, offset } => vec![Field::Opcode("jalr"), Field::Reg(rd), Field::Indirect(offset, rs1)],

            // load
            Op::Lb { rd, rs1, offset } => vec![Field::Opcode("lb"), Field::Reg(rd), Field::Indirect(offset, rs1)],
            Op::Lh { rd, rs1, offset } => vec![Field::Opcode("lh"), Field::Reg(rd), Field::Indirect(offset, rs1)],
            Op::Lw { rd, rs1, offset } => vec![Field::Opcode("lw"), Field::Reg(rd), Field::Indirect(offset, rs1)],
            Op::Ld { rd, rs1, offset } => vec![Field::Opcode("ld"), Field::Reg(rd), Field::Indirect(offset, rs1)],
            Op::Lbu { rd, rs1, offset } => vec![Field::Opcode("lbu"), Field::Reg(rd), Field::Indirect(offset, rs1)],
            Op::Lhu { rd, rs1, offset } => vec![Field::Opcode("lhu"), Field::Reg(rd), Field::Indirect(offset, rs1)],
            Op::Lwu { rd, rs1, offset } => vec![Field::Opcode("lwu"), Field::Reg(rd), Field::Indirect(offset, rs1)],

            // store
            Op::Sb { rs1, rs2, offset } => vec![Field::Opcode("sb"), Field::Reg(rs2), Field::Indirect(offset, rs1)],
            Op::Sh { rs1, rs2, offset } => vec![Field::Opcode("sh"), Field::Reg(rs2), Field::Indirect(offset, rs1)],
            Op::Sw { rs1, rs2, offset } => vec![Field::Opcode("sw"), Field::Reg(rs2), Field::Indirect(offset, rs1)],
            Op::Sd { rs1, rs2, offset } => vec![Field::Opcode("sd"), Field::Reg(rs2), Field::Indirect(offset, rs1)],

            // u-type
            Op::Lui { rd, imm } => vec![Field::Opcode("lui"), Field::Reg(rd), Field::Imm(imm)],
            Op::Auipc { rd, imm } => vec![Field::Opcode("auipc"), Field::Reg(rd), Field::Imm(imm)],

            // misc
            Op::Fence => vec![Field::Opcode("fence")],
            Op::Ecall => vec![Field::Opcode("ecall")],
            Op::Ebreak => vec![Field::Opcode("ebreak")],

            // m extension
            Op::Mul { rd, rs1, rs2 } => vec![Field::Opcode("mul"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Mulh { rd, rs1, rs2 } => vec![Field::Opcode("mulh"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Mulhsu { rd, rs1, rs2 } => {
                vec![Field::Opcode("mulhsu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)]
            }
            Op::Mulhu { rd, rs1, rs2 } => {
                vec![Field::Opcode("mulhu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)]
            }
            Op::Div { rd, rs1, rs2 } => vec![Field::Opcode("div"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Divu { rd, rs1, rs2 } => vec![Field::Opcode("divu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Rem { rd, rs1, rs2 } => vec![Field::Opcode("rem"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Remu { rd, rs1, rs2 } => vec![Field::Opcode("remu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],

            // m extension rv64-specific
            Op::Mulw { rd, rs1, rs2 } => vec![Field::Opcode("mulw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Divw { rd, rs1, rs2 } => vec![Field::Opcode("divw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Divuw { rd, rs1, rs2 } => {
                vec![Field::Opcode("divuw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)]
            }
            Op::Remw { rd, rs1, rs2 } => vec![Field::Opcode("remw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)],
            Op::Remuw { rd, rs1, rs2 } => {
                vec![Field::Opcode("remuw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)]
            }
        }
    }

    pub fn to_pseudo_fields(&self) -> Vec<Field> {
        match *self {
            Op::Addi { rd: ZERO, rs1: ZERO, imm: 0 } => vec![Field::Opcode("nop")],
            Op::Addi { rd, rs1: ZERO, imm } => vec![Field::Opcode("li"), Field::Reg(rd), Field::Imm(imm)],
            Op::Addi { rd, rs1, imm: 0 } => vec![Field::Opcode("mv"), Field::Reg(rd), Field::Reg(rs1)],
            Op::Jalr { rd: ZERO, rs1: RA, offset: 0 } => vec![Field::Opcode("ret")],
            Op::Jalr { rd: ZERO, rs1, offset: 0 } => vec![Field::Opcode("jr"), Field::Reg(rs1)],
            Op::Jalr { rd: RA, rs1, offset: 0 } => vec![Field::Opcode("jalr"), Field::Reg(rs1)],
            Op::Jal { rd: ZERO, offset } => vec![Field::Opcode("j"), Field::PCRelAddr(offset)],
            Op::Jal { rd: RA, offset } => vec![Field::Opcode("jal"), Field::PCRelAddr(offset)],
            Op::Addi { rd, rs1: GP, imm } => vec![Field::Opcode("la"), Field::Reg(rd), Field::GPRelAddr(imm)],
            Op::Xori { rd, rs1, imm: -1 } => vec![Field::Opcode("not"), Field::Reg(rd), Field::Reg(rs1)],
            Op::Sltiu { rd, rs1, imm: 1 } => vec![Field::Opcode("seqz"), Field::Reg(rd), Field::Reg(rs1)],
            Op::Sltu { rd, rs1: ZERO, rs2 } => vec![Field::Opcode("snez"), Field::Reg(rd), Field::Reg(rs2)],
            Op::Beq { rs1: ZERO, rs2, offset } => {
                vec![Field::Opcode("beqz"), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Beq { rs1, rs2: ZERO, offset } => {
                vec![Field::Opcode("beqz"), Field::Reg(rs1), Field::PCRelAddr(offset)]
            }
            Op::Bne { rs1: ZERO, rs2, offset } => {
                vec![Field::Opcode("bnez"), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Bne { rs1, rs2: ZERO, offset } => {
                vec![Field::Opcode("bnez"), Field::Reg(rs1), Field::PCRelAddr(offset)]
            }
            Op::Blt { rs1, rs2: ZERO, offset } => {
                vec![Field::Opcode("bltz"), Field::Reg(rs1), Field::PCRelAddr(offset)]
            }
            Op::Bge { rs1, rs2: ZERO, offset } => {
                vec![Field::Opcode("bgez"), Field::Reg(rs1), Field::PCRelAddr(offset)]
            }
            Op::Bge { rs1: ZERO, rs2, offset } => {
                vec![Field::Opcode("blez"), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Blt { rs1: ZERO, rs2, offset } => {
                vec![Field::Opcode("bgtz"), Field::Reg(rs2), Field::PCRelAddr(offset)]
            }
            Op::Subw { rd, rs1: ZERO, rs2 } => vec![Field::Opcode("negw"), Field::Reg(rd), Field::Reg(rs2)],
            Op::Sub { rd, rs1: ZERO, rs2 } => vec![Field::Opcode("neg"), Field::Reg(rd), Field::Reg(rs2)],
            Op::Slt { rd, rs1: ZERO, rs2 } => vec![Field::Opcode("sgtz"), Field::Reg(rd), Field::Reg(rs2)],
            Op::Slt { rd, rs1, rs2: ZERO } => vec![Field::Opcode("sltz"), Field::Reg(rd), Field::Reg(rs1)],

            // no matching pseudo-instruction
            _ => self.to_fields(),
        }
    }

    pub fn to_string(
        &self,
        pc: i64,
        gp: i64,
        hex: bool,
        verbose: bool,
        show_addresses: bool,
        symbols: &HashMap<i64, String>,
    ) -> String {
        let fields = if verbose { self.to_fields() } else { self.to_pseudo_fields() };
        fields_to_string(&fields, pc, gp, hex, verbose, show_addresses, symbols)
    }

    pub fn branch_target(&self, pc: i64) -> Option<i64> {
        match self {
            Self::Beq { offset, .. } => Some(pc + offset),
            Self::Bne { offset, .. } => Some(pc + offset),
            Self::Blt { offset, .. } => Some(pc + offset),
            Self::Bge { offset, .. } => Some(pc + offset),
            Self::Bltu { offset, .. } => Some(pc + offset),
            Self::Bgeu { offset, .. } => Some(pc + offset),
            Self::Jal { rd: ZERO, offset, .. } => Some(pc + offset),
            _ => None,
        }
    }
}

pub fn get_pseudo_sequence(
    instructions: &[Instruction],
    symbols: &HashMap<i64, String>,
) -> Option<(usize, Vec<Field>)> {
    if instructions.len() < 2 {
        return None;
    }
    let (inst1, inst2) = (&instructions[0], &instructions[1]);

    // do not merge instructions if the second one is labeled
    if symbols.contains_key(&inst2.address) {
        return None;
    }

    match (&inst1.op, &inst2.op) {
        (Op::Auipc { rd: rd1, imm: imm1 }, Op::Addi { rd: rd2, rs1: rs2, imm: imm2 }) if rd1 == rd2 && rd2 == rs2 => {
            Some((2, vec![Field::Opcode("la"), Field::Reg(*rd1), Field::PCRelAddr(imm1 + imm2)]))
        }

        (Op::Auipc { rd: RA, imm }, Op::Jalr { rd: RA, rs1: RA, offset }) => {
            Some((2, vec![Field::Opcode("call"), Field::PCRelAddr(imm + offset)]))
        }

        _ => None,
    }
}

pub fn fields_to_string(
    fields: &[Field],
    pc: i64,
    gp: i64,
    hex: bool,
    verbose: bool,
    show_addresses: bool,
    symbols: &HashMap<i64, String>,
) -> String {
    let addr_part = if !show_addresses {
        String::new()
    } else if hex {
        format!("0x{:5x} ", pc)
    } else {
        format!("{:>7} ", pc)
    };

    let label = if let Some(label) = symbols.get(&pc) { format!("{}:", label) } else { String::new() };

    let operands =
        fields[1..].iter().map(|elt| elt.to_string(pc, gp, hex, verbose, symbols)).collect::<Vec<_>>().join(", ");
    let disasm = format!("{:<8}{}", fields[0].to_string(pc, gp, hex, verbose, symbols), operands);

    format!("{addr_part}{label:<16}{disasm:<48}")
}

pub enum Field {
    Opcode(&'static str),
    Reg(usize),
    Imm(i64),
    Indirect(i64, usize),
    PCRelAddr(i64),
    GPRelAddr(i64),
}

impl Field {
    pub fn to_string(&self, pc: i64, gp: i64, hex: bool, verbose: bool, symbols: &HashMap<i64, String>) -> String {
        match self {
            Field::Opcode(inst) => String::from(*inst),
            Field::Reg(reg) => String::from(R[*reg]),
            Field::Imm(i) if !hex || (0..=9).contains(i) => format!("{}", i),
            Field::Imm(i) => format!("0x{:x}", i),
            Field::Indirect(0, reg) if !verbose => format!("({})", R[*reg]),
            Field::Indirect(imm, reg) if hex => format!("0x{:x}({})", imm, R[*reg]),
            Field::Indirect(imm, reg) => format!("{}({})", imm, R[*reg]),
            Field::PCRelAddr(offset) => {
                let addr = offset + pc;
                match symbols.get(&addr) {
                    Some(symbol) if !verbose => match symbol.parse::<i64>() {
                        Ok(num) if num > 0 => {
                            let suffix = if addr <= pc { "b" } else { "f" };
                            format!("{}{}", symbol, suffix)
                        }
                        _ => symbol.clone(),
                    },
                    _ => {
                        if !hex || (0..=9).contains(offset) {
                            format!("{}", offset)
                        } else {
                            format!("0x{:x}", offset)
                        }
                    }
                }
            }
            Field::GPRelAddr(offset) => {
                // gp-relative only applies to pseudo-instructions in !verbose mode
                // i.e., "la"
                let addr = offset + gp;
                match symbols.get(&addr) {
                    Some(symbol) => symbol.clone(),
                    _ => {
                        if !hex || (0..=9).contains(&addr) {
                            format!("{}", addr)
                        } else {
                            format!("0x{:x}", addr)
                        }
                    }
                }
            }
        }
    }
}
