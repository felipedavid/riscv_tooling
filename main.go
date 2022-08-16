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

type Opcode uint8
type Reg uint8
type Word uint32

type Instruction struct {
	op           Opcode
	rs1, rs2, rd Reg
	imm          Word
}

const (
	opcodeMask uint32 = (1 << 7) - 1
	regMask    uint32 = (1 << 5) - 1
	rdMask     uint32 = regMask << 7
	rs1Mask    uint32 = regMask << 15
	rs2Mask    uint32 = regMask << 20
	funct3Mask uint32 = ((1 << 3) - 1) << 12
	funct7Mask uint32 = ((1 << 7) - 1) << 25
	fenceMask  uint32 = 0b1111_0000_0000_11111_111_11111_1111111
)

var funct3ToBranchOp = []Opcode{
	0b000: BEQ,
	0b001: BNE,
	0b100: BLT,
	0b101: BGE,
	0b110: BLTU,
	0b111: BGEU,
}

var funct3ToLoadOp = []Opcode{
	0b000: LB,
	0b001: LH,
	0b010: LW,
	0b100: LBU,
	0b101: LHU,
}

var funct3ToStoreOp = []Opcode{
	0b000: SB,
	0b001: SH,
	0b010: SW,
}

var funct3ToImmOp = []Opcode{
	0b000: ADDI,
	0b010: SLTI,
	0b011: SLTIU,
	0b100: XORI,
	0b110: ORI,
	0b111: ANDI,
	0b001: SLLI,
	0b101: SRLI, // or SRAI
}

var funct4ToRegOp = []Opcode{
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

var funct3ToCSROp = []Opcode{
	0b001: CSRRW,
	0b010: CSRRS,
	0b011: CSRRC,
	0b101: CSRRWI,
	0b110: CSRRSI,
	0b111: CSRRCI,
}

func decodeBInstruction(instr *Instruction, data uint32) {

}

func decodeUInstruction(instr *Instruction, data uint32) {

}

func decodeIInstruction(instr *Instruction, data uint32) {

}

func decodeJInstruction(instr *Instruction, data uint32) {

}

func decodeSInstruction(instr *Instruction, data uint32) {

}

func decodeRInstruction(instr *Instruction, data uint32) {

}

func decodeCSRInstruction(instr *Instruction, data uint32) {

}

func DecodeInstruction(data uint32) Instruction {
	rs1 := Reg(data & rs1Mask >> 15)
	rs2 := Reg(data & rs2Mask >> 20)
	rd := Reg(data & rdMask >> 7)

	// Since Golang automagically initialized instr.op to 0 (ILLEGAL_OP),
	// in some cases I will not set instr.op to ILLEGAL_OP because it already
	// holds this value
	instr := Instruction{
		rs1: rs1,
		rs2: rs2,
		rd:  rd,
	}

	funct3 := data & funct3Mask >> 12
	funct7 := data & funct7Mask >> 25

	switch Opcode(data & opcodeMask) {
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
		if funct3 == 0b000 {
			instr.op = JALR
		}
		decodeIInstruction(&instr, data)
	case 0b1100011: // BEQ, BNE, BLT, BGE, BLTU, BGEU
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
		case SRLI: // SRAI
			if funct7 == 0b0100000 {
				instr.op = SRAI
			} else if funct7 != 0b0000000 {
				instr.op = ILLEGAL_OP
			}
		}
		decodeIInstruction(&instr, data)
	case 0b0110011: // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
		var mask uint32 = 0b0100000
		if funct7 & ^mask == 0b0000000 {
			funct4 := ((funct7 & mask) >> 2) | funct3
			instr.op = funct4ToRegOp[funct4]
		}
		decodeRInstruction(&instr, data)
	case 0b0001111: // FENCE, FENCE.I
		if data == 0b0000_0000_0000_00000_001_00000_0001111 {
			instr.op = FENCEI
		} else if data&fenceMask == 0b0000_0000_0000_00000_000_00000_0001111 {
			// Reminder: I'm not decoding the pred/succ fields
			instr.op = FENCE
		}
	case 0b1110011: // ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI
		if data == 0b000000000000_00000_000_00000_1110011 {
			instr.op = ECALL
		} else if data == 0b000000000001_00000_000_00000_1110011 {
			instr.op = EBREAK
		} else {
			instr.op = funct3ToCSROp[funct3]
			decodeCSRInstruction(&instr, data)
		}
	default:
		// This is kinda redundant since Go is already initializing instr.op to 0 and ILLEGAL_OP == 0
		instr.op = ILLEGAL_OP
	}

	return instr
}

func EncodeInstruction(instr Instruction) uint32 {
	return 0
}

func main() {
}
