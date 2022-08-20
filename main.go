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

func signExtend(x uint32, width uint32) uint32 {
	return uint32(int32(x<<(32-width)) >> int32(32-width))
}

func bits(data, start, lenn uint32) uint32 {
	return (data >> start) & ((1 << lenn) - 1)
}

func decodeUImmediate(data uint32) uint32 {
	imm12_31 := bits(data, 12, 20) << 12
	imm0_31 := imm12_31
	return signExtend(imm0_31, 32)
}

func decodeJImmediate(data uint32) uint32 {
	imm1_10 := bits(data, 21, 10) << 1
	imm11 := bits(data, 20, 1) << 11
	imm12_19 := bits(data, 12, 8) << 12
	imm20 := bits(data, 31, 1) << 20
	imm0_20 := imm1_10 | imm11 | imm12_19 | imm20
	return signExtend(imm0_20, 21)
}

func decodeBImmediate(data uint32) uint32 {
	imm1_4 := bits(data, 8, 4) << 1
	imm5_10 := bits(data, 25, 6) << 5
	imm11 := bits(data, 7, 1) << 11
	imm12 := bits(data, 31, 1) << 12
	imm0_12 := imm1_4 | imm5_10 | imm11 | imm12
	return signExtend(imm0_12, 13)
}

func decodeIImmediate(data uint32) uint32 {
	imm0_11 := bits(data, 20, 12)
	return signExtend(imm0_11, 12)
}

func decodeSImmediate(data uint32) uint32 {
	imm0_4 := bits(data, 7, 5)
	imm5_11 := bits(data, 25, 7) << 5
	imm0_11 := hiImm | loImm
	return signExtend(imm0_11, 12)
}

func DecodeInstruction(data uint32) Instruction {
	opcode := bits(data, 0, 7)
	funct3 := bits(data, 12, 3)
	funct7 := bits(data, 25, 7)
	rs1 := bits(data, 15, 5)
	rs2 := bits(data, 20, 5)
	rd := bits(data, 7, 5)

	switch opcode {
	case 0b0110111: // LUI
		return Instruction{op: LUI, rd: rd, imm: decodeUImmediate(data)}
	case 0b0010111: // AUIPC
		return Instruction{op: AUIPC, rd: rd, imm: decodeUImmediate(data)}
	case 0b1101111: // JAL
		return Instruction{op: JAL, rd: rd, imm: decodeJImmediate(data)}
	case 0b1100111: // JALR
		if funct3 == 0b000 {
			return Instruction{op: JALR, rd: rd, rs1: rs1, imm: decodeIImmediate(data)}
		}
	case 0b1100011: // BEQ, BNE, BLT, BGE, BLTU, BGEU
		return Instruction{op: funct3ToBranchOp[funct3], rs1: rs1, rs2: rs2, imm: decodeBImmediate(data)}
	case 0b0000011: // LB, LH, LW, LBU, LHU
		return Instruction{op: funct3ToLoadOp[funct3], rd: rd, rs1: rs1, imm: decodeIImmediate(data)}
	case 0b0100011: // SB, SH, SW
		return Instruction{op: funct3ToStoreOp[funct3], rs1: rs1, rs2: rs2, imm: decodeSImmediate(data)}
	case 0b0010011: // ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI
		op = funct3ToImmOp[funct3]
		switch op {
		case SLLI:
			instr.imm = uint32(rs2)
			if funct7 == 0b0000000 {
				return Instruction{op: SLLI, rd: rd, rs1: rs1, imm: rs2}
			}
		case SRLI: // SRAI
			if funct7 == 0b0000000 {
				return Instruction{op: SRLI, rd: rd, rs1: rs1, imm: rs2}
			} else if funct7 == 0b0100000 {
				return Instruction{op: SRAI, rd: rd, rs1: rs1, imm: rs2}
			}
		default:
			return Instruction{op: op, rd: rd, rs1: rs1, imm: decodeIImmediate(data)}
		}
	case 0b0110011: // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
		if funct7&0b1011111 == 0 {
			funct4 := funct3 | (bits(funct7, 5, 1) << 3)
			return Instruction{op: funct4ToRegOp[funct4], rd: rd, rs1: rs1, rs2: rs2}
		}
	case 0b0001111: // FENCE, FENCE.I
		if data == 0b0000_0000_0000_00000_001_00000_0001111 {
			return Instruction{op: FENCEI}
		} else if data&fenceMask == 0b0000_0000_0000_00000_000_00000_0001111 {
			// Reminder: I'm not decoding the pred/succ fields
			return Instruction{op: FENCE, succPred: bits(data, 20, 8)}
		}
	case 0b1110011: // ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI
		if data == 0b000000000000_00000_000_00000_1110011 {
			return Instruction{op: ECALL}
		} else if data == 0b000000000001_00000_000_00000_1110011 {
			return Instruction{op: EBREAK}
		} else {
			op = funct3ToCSROp[funct3]
			csr := bits(data, 20, 12)
			if bits(funct3, 2, 1) != 0 {
				return Instruction{op: op, rd: rd, csr: csr, imm: bits(data, 15, 5)}
			} else {
				return Instruction{op: op, rd: rd, csr: csr, rs1: rs1}
			}
		}
	default:
	}

	return Instruction{op: ILLEGAL_OP}
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
