package main

import (
	"fmt"
	"log"
	"unsafe"
)

func signExtendByte(val uint32) uint32 {
	return uint32(int32(val<<24) >> 24)
}

func signExtendHalfWord(val uint32) uint32 {
	return uint32(int32(val<<16) >> 16)
}

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

type Instruction struct {
	op           Opcode
	rs1, rs2, rd uint8
	csr          uint16
	imm          uint32
}

const (
	opcodeMask        uint32 = (1 << 7) - 1
	regMask           uint32 = (1 << 5) - 1
	rdShiftAmount     uint32 = 7
	rs1ShiftAmount    uint32 = 15
	rs2ShiftAmount    uint32 = 20
	funct3ShiftAmount uint32 = 12
	funct3Mask        uint32 = (1 << 3) - 1
	funct7ShiftAmount uint32 = 25
	funct7Mask        uint32 = (1 << 7) - 1
	fenceMask         uint32 = 0b1111_0000_0000_11111_111_11111_1111111
	immUMask          uint32 = ((1 << 20) - 1) << 12
	immIMask          uint32 = ((1 << 12) - 1) << 20
	immSLoShiftAmount uint32 = 7
	immSHiShiftAmount uint32 = 25
	immSLoMask        uint32 = (1 << 5) - 1
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

func bits(x, min, max uint32) uint32 {
	return (x >> min) & (1<<(max-min) - 1)
}

func decodeBInstruction(instr *Instruction, data uint32) {
	imm12 := data >> 31
	imm11 := (data >> 7) & 1
	imm10_5 := (data >> 25) & ((1 << 6) - 1)
	imm4_1 := (data >> 8) & ((1 << 4) - 1)
	imm := (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1)
	instr.imm = uint32(int32(imm<<19) >> 19)
}

func decodeUInstruction(instr *Instruction, data uint32) {
	instr.imm = uint32(data & immUMask)
}

func decodeIInstruction(instr *Instruction, data uint32) {
	instr.imm = uint32(int32(data&immIMask) >> 20)
}

func decodeJInstruction(instr *Instruction, data uint32) {
	imm20 := data >> 31
	imm19_12 := (data >> 12) & ((1 << 8) - 1)
	imm11 := (data >> 20) & 1
	imm10_1 := (data >> 21) & ((1 << 10) - 1)
	imm := (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1)
	instr.imm = uint32(int32(imm<<11) >> 11)
}

func decodeSInstruction(instr *Instruction, data uint32) {
	// TODO: Make sure this works as expected
	hiImm := uint32(int32(data)>>int32(immSHiShiftAmount)) << 5
	loImm := (data >> immSLoShiftAmount) & immSLoMask
	instr.imm = uint32(hiImm | loImm)
}

func DecodeInstruction(data uint32) Instruction {
	rs1 := uint8((data >> rs1ShiftAmount) & regMask)
	rs2 := uint8((data >> rs2ShiftAmount) & regMask)
	rd := uint8((data >> rdShiftAmount) & regMask)

	// Since Golang automagically initialized instr.op to 0 (ILLEGAL_OP),
	// in some cases I will not set instr.op to ILLEGAL_OP because it already
	// holds this value
	instr := Instruction{
		rs1: rs1,
		rs2: rs2,
		rd:  rd,
	}

	funct3 := (data >> funct3ShiftAmount) & funct3Mask
	funct7 := (data >> funct7ShiftAmount) & funct7Mask

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
			instr.imm = uint32(rs2)
			if funct7 != 0b0000000 {
				instr.op = ILLEGAL_OP
			}
		case SRLI: // SRAI
			instr.imm = uint32(rs2)
			if funct7 == 0b0100000 {
				instr.op = SRAI
			} else if funct7 != 0b0000000 {
				instr.op = ILLEGAL_OP
			}
		default:
			decodeIInstruction(&instr, data)
		}
	case 0b0110011: // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
		var mask uint32 = 0b0100000
		if funct7 & ^mask == 0b0000000 {
			funct4 := ((funct7 & mask) >> 2) | funct3
			instr.op = funct4ToRegOp[funct4]
		}
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
			instr.rs1 = uint8((data >> 15) & regMask)
			instr.csr = uint16(data >> 20)
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

type Hart struct {
	pc      uint32
	regFile [32]uint32
	mem     [4096]uint8
}

func (h *Hart) readInstruction(addr uint32) uint32 {
	if addr+4 < uint32(len(h.mem)) {
		return *(*uint32)(unsafe.Pointer(&h.mem[addr]))
	}
	return 0
}

func (h *Hart) readRegister(regIndex uint8) uint32 {
	return h.regFile[regIndex]
}

func (h *Hart) writeRegister(regIndex uint8, val uint32) {
	if regIndex != 0 {
		h.regFile[regIndex] = val
	}
}

func (h *Hart) loadByte(addr uint32) uint32 {
	if addr < uint32(len(h.mem)) {
		return uint32(h.mem[addr])
	}
	return 0
}

func (h *Hart) loadHalfWord(addr uint32) uint32 {
	if addr+1 < uint32(len(h.mem)) {
		val := *(*uint16)(unsafe.Pointer(&h.mem[addr]))
		return uint32(val)
	}
	return 0
}

func (h *Hart) loadWord(addr uint32) uint32 {
	if addr+3 < uint32(len(h.mem)) {
		return *(*uint32)(unsafe.Pointer(&h.mem[addr]))
	}
	return 0
}

func (h *Hart) storeByte(addr, val uint32) {
	if addr < uint32(len(h.mem)) {
		h.mem[addr] = uint8(val)
	}
}

func (h *Hart) storeHalfWord(addr, val uint32) {
	if addr+1 < uint32(len(h.mem)) {
		*(*uint16)(unsafe.Pointer(&h.mem[addr])) = uint16(val)
	}
}

func (h *Hart) storeWord(addr, val uint32) {
	if addr+3 < uint32(len(h.mem)) {
		*(*uint32)(unsafe.Pointer(&h.mem[addr])) = val
	}
}

func (h *Hart) readCsr(csr uint16) uint32 {
	return 0
}

func (h *Hart) writeCsr(csr uint16, val uint32) {

}

const shiftMask uint32 = (1 << 5) - 1

func (h *Hart) step() {
	for {
		instrData := h.readInstruction(0)
		instr := DecodeInstruction(instrData)

		rs1 := instr.rs1
		rs2 := instr.rs2
		rd := instr.rd
		imm := instr.imm
		csr := instr.csr

		rs1Val := h.regFile[rs1]
		rs2Val := h.regFile[rs2]

		nextPc := h.pc + 4
		branchPc := h.pc + imm

		switch instr.op {
		case LUI:
			h.writeRegister(rd, imm)
		case AUIPC:
			h.writeRegister(rd, branchPc)
		case JAL:
			h.writeRegister(rd, nextPc)
			nextPc = branchPc
		case JALR:
			h.writeRegister(rd, nextPc)
			nextPc = (imm + rs1Val) & ^uint32(1)
		case BEQ:
			if rs1Val == rs2Val {
				nextPc = branchPc
			}
		case BNE:
			if rs1Val != rs2Val {
				nextPc = branchPc
			}
		case BLT:
			if int32(rs1Val) < int32(rs2Val) {
				nextPc = branchPc
			}
		case BGE:
			if int32(rs1Val) >= int32(rs2Val) {
				nextPc = branchPc
			}
		case BLTU:
			if rs1Val < rs2Val {
				nextPc = branchPc
			}
		case BGEU:
			if rs1Val >= rs2Val {
				nextPc = branchPc
			}
		case LB:
			h.writeRegister(rd, signExtendByte(h.loadByte(rs1Val+imm)))
		case LH:
			h.writeRegister(rd, signExtendHalfWord(h.loadHalfWord(rs1Val+imm)))
		case LW:
			h.writeRegister(rd, h.loadWord(rs1Val+imm))
		case LBU:
			h.writeRegister(rd, h.loadByte(rs1Val+imm))
		case LHU:
			h.writeRegister(rd, h.loadHalfWord(rs1Val+imm))
		case SB:
			h.storeByte(rs1Val+imm, rs2Val)
		case SH:
			h.storeHalfWord(rs1Val+imm, rs2Val)
		case SW:
			h.storeWord(rs1Val+imm, rs2Val)
		case ADDI:
			h.writeRegister(rd, rs1Val+imm)
		case SLTI:
			val := uint32(0)
			if int32(rs1Val) < int32(imm) {
				val = 1
			}
			h.writeRegister(rd, val)
		case SLTIU:
			val := uint32(0)
			if rs1Val < imm {
				val = 1
			}
			h.writeRegister(rd, val)
		case XORI:
			h.writeRegister(rd, rs1Val^imm)
		case ORI:
			h.writeRegister(rd, rs1Val|imm)
		case ANDI:
			h.writeRegister(rd, rs1Val&imm)
		case SLLI:
			h.writeRegister(rd, rs1Val<<imm)
		case SRLI:
			h.writeRegister(rd, rs1Val>>imm)
		case SRAI:
			h.writeRegister(rd, uint32(int32(rs1Val)>>int32(imm)))
		case ADD:
			h.writeRegister(rd, rs1Val+rs2Val)
		case SUB:
			h.writeRegister(rd, rs1Val-rs2Val)
		case SLL:
			h.writeRegister(rd, rs1Val<<(rs2Val&shiftMask))
		case SLT:
			val := uint32(0)
			if int32(rs1Val) < int32(rs2Val) {
				val = 1
			}
			h.writeRegister(rd, val)
		case SLTU:
			val := uint32(0)
			if rs1Val < rs2Val {
				val = 1
			}
			h.writeRegister(rd, val)
		case XOR:
			h.writeRegister(rd, rs1Val^rs2Val)
		case SRL:
			h.writeRegister(rd, rs1Val>>(rs2Val&shiftMask))
		case SRA:
			h.writeRegister(rd, uint32(int32(rs1Val)>>(int32(rs2Val)&int32(shiftMask))))
		case OR:
			h.writeRegister(rd, rs1Val|rs2Val)
		case AND:
			h.writeRegister(rd, rs1Val&rs2Val)
		case FENCE:
		case FENCEI:
		case ECALL:
		case EBREAK:
		case CSRRW:
			if rd != 0 {
				csrVal := h.readCsr(csr)
				h.writeRegister(rd, csrVal)
			}
			h.writeCsr(csr, rs1Val)
		case CSRRS:
			csrVal := h.readCsr(csr)
			h.writeRegister(rd, csrVal)
			if rs1 != 0 {
				h.writeCsr(csr, csrVal|rs1Val)
			}
		case CSRRC:
			csrVal := h.readCsr(csr)
			if rs1 != 0 {
				h.writeRegister(rd, csrVal&(^rs1Val))
			}
		case CSRRWI:
			if rd != 0 {
				csrVal := h.readCsr(csr)
				h.writeRegister(rd, csrVal)
			}
			h.writeCsr(csr, imm)
		case CSRRSI:
			csrVal := h.readCsr(csr)
			h.writeRegister(rd, csrVal)
			if imm != 0 {
				h.writeCsr(csr, csrVal|imm)
			}
		case CSRRCI:
			csrVal := h.readCsr(csr)
			if rs1 != 0 {
				h.writeRegister(rd, csrVal&(^imm))
			}
		default:
			log.Fatal("Instruction not implemented yet. (%v)", instr.op)
		}
		h.pc = nextPc
	}
}

func (h *Hart) printState() {
	fmt.Printf("pc = %d (0x%08x)\n", h.pc, h.pc)
	for i := 1; i < 32; i++ {
		fmt.Printf("x%d = %d (0x%08x)\n", i, h.regFile[i], h.regFile[i])
	}
}

func main() {
	hart := Hart{}
	hart.printState()
}
