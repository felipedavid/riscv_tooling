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

type Instruction struct {
	op           uint8
	rs1, rs2, rd uint8
}

func bits(data uint32, start uint32, length uint32) uint32 {
	return (data >> start) & ((1 << length) - 1)
}

func decodeUInstruction(instr *Instruction, data uint32) {
}

func decodeInstruction(data uint32) (instr Instruction) {
	rs1 := bits(data, 15, 5)
	rs2 := bits(data, 20, 5)
	rd := bits(data, 7, 5)

	funct3 := bits(data, 12, 3)
	funct7 := bits(data, 25, 7)

	instr = Instruction{
		rs1: rs1,
		rs2: rs2,
		rd:  rd,
	}

	switch op := data & OP_MASK; op {
	case 0b0110111: // LUI
		instr.op = LUI
		decodeUInstruction(&instr, data)
	case 0b0010111: // AUIPC
		instr.op = AUIPC
		decodeUInstruction(&instr, data)
	default:
		instr.op = ILLEGAL_OP
	}
}

func encodeInstruction(instr Instruction) (data uint32) {
	data = 0
	return data
}

func main() {
}
