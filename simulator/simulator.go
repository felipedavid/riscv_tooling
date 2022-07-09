package main

const (
	ILLEGAL_OP = iota
	LUI
	AUIPC
	JAL
	JALR
	BEQ
	BNE
	BLT
	BGE
	BLTU
	BGEU
	LB
	LH
	LW
	LBU
	LHU
	SB
	SH
	SW
	ADDI
	SLTI
	SLTIU
	XORI
	ORI
	ANDI
	SLLI
	SRLI
	SRAI
	ADD
	SUB
	SLL
	SLT
	SLTU
	XOR
	SRL
	SRA
	OR
	AND
	FENCE
	FENCEI
	ECALL
	EBREAK
	CSRRW
	CSRRS
	CSRRC
	CSRRWI
	CSRRSI
	CSRRCI
)

type Op uint8
type Reg uint8

type Instruction struct {
	op           Op
	rs1, rs2, rd Reg
}

func bits(data uint32, start uint32, length uint32) uint32 {
	return (data >> start) & ((1 << length) - 1)
}

var funct3ToBranchOp = []Op{
	0b000: BEQ,
	0b001: BNE,
	0b100: BLT,
	0b101: BGE,
	0b110: BLTU,
	0b111: BGEU,
}

var funct3ToLoadOp = []Op{
	0b000: LB,
	0b001: LH,
	0b010: LW,
	0b100: LBU,
	0b101: LHU,
}

var funct3ToStoreOp = []Op{
	0b000: SB,
	0b001: SH,
	0b010: SW,
}

var funct3ToImmOp = []Op{
	0b000: ADDI,
	0b010: SLTI,
	0b011: SLTIU,
	0b100: XORI,
	0b110: ORI,
	0b111: ANDI,
	0b001: SLLI,
	0b101: SRLI,
}

var funct4ToRegOp = []Op{
	0b0000: ADD,
	0b1000: SUB,
	0b0001: SLL,
	0b0010: SLT,
	0b0011: SLTU,
	0b0100: XOR,
	0b0101: SRL,
	0b1101: SRA,
	0b0110: OR,
	0b0111: AND,
}

var funct3ToCsrOp = []Op{
	0b001: CSRRW,
	0b010: CSRRS,
	0b011: CSRRC,
	0b101: CSRRWI,
	0b110: CSRRSI,
	0b111: CSRRCI,
}

func decodeIInstruction(instr *Instruction, data uint32) {
}

func decodeUInstruction(instr *Instruction, data uint32) {
}

func decodeJInstruction(instr *Instruction, data uint32) {
}

func decodeBInstruction(instr *Instruction, data uint32) {
}

func decodeSInstruction(instr *Instruction, data uint32) {
}

func decodeInstruction(data uint32) (instr Instruction) {
	rs1 := Reg(bits(data, 15, 5))
	rs2 := Reg(bits(data, 20, 5))
	rd := Reg(bits(data, 7, 5))

	funct3 := bits(data, 12, 3)
	funct7 := bits(data, 25, 7)

	instr = Instruction{
		rs1: rs1,
		rs2: rs2,
		rd:  rd,
	}

	// Figure out which instruction we are talking about
	switch op := bits(data, 0, 7); op {
	case 0b0110111: // LUI
		instr.op = LUI
		decodeUInstruction(&instr, data)
	case 0b0010111: // AUIPC
		instr.op = AUIPC
		decodeUInstruction(&instr, data)
	case 0b1101111: // JAL
		instr.op = JAL
		decodeJInstruction(&instr, data)
	case 0b1100111: // JALR
		instr.op = JALR
		decodeIInstruction(&instr, data)

	// Some instructions share the same opcode and only differenciate based on
	// the funct fields
	case 0b1100011: // BEQ, BNE, BLT, BGE, BLTU, BGEU
		// We are using tables to minimize branching. This small optimization
		// might be silly, but I don't care :P
		instr.op = funct3ToBranchOp[funct3]
		decodeBInstruction(&instr, data)
	case 0b0000011: // LB, LH, LW, LBU, LHU
		instr.op = funct3ToLoadOp[funct3]
		decodeIInstruction(&instr, data)
	case 0b0100011: // SB, SH, SW
		instr.op = funct3ToStoreOp[funct3]
		decodeSInstruction(&instr, data)
	case 0b0010011: // ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI
		instr.op = funct3ToImmOp[funct3]
		switch instr.op {
		case SLLI:
			if funct7 != 0b0000000 {
				instr.op = ILLEGAL_OP
			}
		case SRLI:
			if funct7 == 0b0100000 {
				instr.op = SRAI
			} else if funct7 != 0b0000000 {
				instr.op = ILLEGAL_OP
			}
		}
	case 0b0110011: // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
		// funct4 is the concatination of funct3 and funct7 fifth bit
		mask := 0b0100000
		// To the instruction be valid, all bits select by ^mask (0b1011111) must be zero
		if (funct7 & ^mask) == 0 {
			funct4 := funct3 | ((funct7 & mask) >> 2)
			instr.op = funct4ToRegOp[funct4]
		}
	case 0b0001111: // FENCE, FENCEI
		if data == 0b0000_0000_0000_00000_001_00000_0001111 {
			instr.op = FENCEI
		} else if (data & 0b1111_0000_0000_11111_111_11111_0000000) == 0 {
			instr.op = FENCE
		}
	case 0b1110011: // ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI
		if data == 0b000000000000_00000_000_00000_1110011 {
			instr.op = ECALL
		} else if data == 0b000000000001_00000_000_00000_1110011 {
			instr.op = EBREAK
		} else {
			instr.op = funct3ToCsrOp[funct3]
			decodeCsrInstruction(&instr, data)
		}
	default:
		instr.op = ILLEGAL_OP
	}

	return
}

func encodeInstruction(instr Instruction) (data uint32) {
	data = 0
	return data
}

func main() {
}
