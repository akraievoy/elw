package elw.dp.mips;

public class Alu {
	@InstructionDesc(
			syntax = "addu $d, $s, $t",
			template = "000000ssssstttttddddd00000100001",
			writeRegs = "$d",
			unsigned = true
	)
	public void addu(final InstructionContext ctx) {
		ctx.setD(ctx.getS() + ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "add $d, $s, $t",
			template = "000000ssssstttttddddd00000100000",
			writeRegs = "$d"
	)
	public void add(final InstructionContext ctx) {
		ctx.setD(ctx.getS() + ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "addi $t, $s, imm16",
			template = "001000ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void addi(final InstructionContext ctx) {
		ctx.setT(ctx.getS() + ctx.getI16());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "addiu $t, $s, imm16",
			template = "001001ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t",
			unsigned = true
	)
	public void addiu(final InstructionContext ctx) {
		ctx.setT(ctx.getS() + ctx.getI16());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "and $d, $s, $t",
			template = "000000ssssstttttddddd00000100100",
			writeRegs = "$d"
	)
	public void and(final InstructionContext ctx) {
		ctx.setD(ctx.getS() & ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "andi $t, $s, imm16",
			template = "001100ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void andi(final InstructionContext ctx) {
		ctx.setT(ctx.getS() & ctx.getI16());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "beq $s, $t, addr16",
			template = "000100ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void beq(final InstructionContext ctx) {
		if (ctx.getS() == ctx.getT()) {
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "bgez $s, addr16",
			template = "000001sssss00001iiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void bgez(final InstructionContext ctx) {
		if (ctx.getS() >= 0) {
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "bgezal $s, addr16",
			template = "000001sssss10001iiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void bgezal(final InstructionContext ctx) {
		if (ctx.getS() >= 0) {
			ctx.storeRa();
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "bgtz $s, addr16",
			template = "000111sssss00000iiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void bgtz(final InstructionContext ctx) {
		if (ctx.getS() > 0) {
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "blez $s, addr16",
			template = "000110sssss00000iiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void blez(final InstructionContext ctx) {
		if (ctx.getS() <= 0) {
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "bltz $s, addr16",
			template = "000001sssss00000iiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void bltz(final InstructionContext ctx) {
		if (ctx.getS() < 0) {
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "bltzal $s, addr16",
			template = "000001sssss10000iiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void bltzal(final InstructionContext ctx) {
		if (ctx.getS() < 0) {
			ctx.storeRa();
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "bne $s, $t, addr16",
			template = "000101ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void bne(final InstructionContext ctx) {
		if (ctx.getS() != ctx.getT()) {
			ctx.advPc(ctx.getI16() << 2);
		} else {
			ctx.advPc();
		}
	}

	@InstructionDesc(
			syntax = "divu $s, $t",
			template = "000000sssssttttt0000000000011011",
			writeRegs = "",
			unsigned = true
	)
	public void divu(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		ctx.getRegisters().setReg(Reg.lo, s / t);
		ctx.getRegisters().setReg(Reg.hi, s % t);
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "div $s, $t",
			template = "000000sssssttttt0000000000011010",
			writeRegs = ""
	)
	public void div(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		ctx.getRegisters().setReg(Reg.lo, s / t);
		ctx.getRegisters().setReg(Reg.hi, s % t);
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "j addr26",
			template = "000010iiiiiiiiiiiiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void j(final InstructionContext ctx) {
		final Registers regs = ctx.getRegisters();

		regs.setReg(Reg.pc, regs.getReg(Reg.pc) & 0xF0000000 | ctx.getI26() << 2);
	}

	@InstructionDesc(
			syntax = "jal addr26",
			template = "000011iiiiiiiiiiiiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void jal(final InstructionContext ctx) {
		final Registers regs = ctx.getRegisters();
		ctx.storeRa();
		regs.setReg(Reg.pc, regs.getReg(Reg.pc) & 0xF0000000 | ctx.getI26() << 2);
	}

	@InstructionDesc(
			syntax = "jr $s",
			template = "000000sssss000000000000000001000",
			writeRegs = ""
	)
	public void jr(final InstructionContext ctx) {
		final Registers regs = ctx.getRegisters();

		regs.setReg(Reg.pc, ctx.getS());
	}

	@InstructionDesc(
			syntax = "lb $t, imm16($s)",
			template = "100000ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void lb(final InstructionContext ctx) {
		ctx.setT(ctx.getMemory().getByte(ctx.getS() + ctx.getI16()));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "lui $t, imm16",
			template = "001111-----tttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void lui(final InstructionContext ctx) {
		ctx.setT(ctx.getI16() << 16);
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "lw $t, imm16($s)",
			template = "100011ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void lw(final InstructionContext ctx) {
		ctx.setT(ctx.getMemory().getWord(ctx.getS() + ctx.getI16()));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "mfhi $d",
			template = "0000000000000000ddddd00000010000",
			writeRegs = "$d"
	)
	public void mfhi(final InstructionContext ctx) {
		ctx.setD(ctx.getRegisters().getReg(Reg.hi));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "mflo $d",
			template = "0000000000000000ddddd00000010010",
			writeRegs = "$d"
	)
	public void mflo(final InstructionContext ctx) {
		ctx.setD(ctx.getRegisters().getReg(Reg.lo));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "multu $s, $t",
			template = "000000sssssttttt0000000000011001",
			writeRegs = "",
			unsigned = true
	)
	public void multu(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		final long value = ((long) s & 0xFFFFFFFFL) * ((long) t & 0xFFFFFFFFL);

		ctx.getRegisters().setReg(Reg.lo, (int) value);
		ctx.getRegisters().setReg(Reg.hi, (int) (value >> 32));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "mult $s, $t",
			template = "000000sssssttttt0000000000011000",
			writeRegs = ""
	)
	public void mult(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		final long value = (long) s * t;

		ctx.getRegisters().setReg(Reg.lo, (int) value);
		ctx.getRegisters().setReg(Reg.hi, (int) (value >> 32));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "noop",
			template = "00000000000000000000000000000000",
			writeRegs = ""
	)
	public void noop(final InstructionContext ctx) {
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "or $d, $s, $t",
			template = "000000ssssstttttddddd00000100101",
			writeRegs = "$d"
	)
	public void or(final InstructionContext ctx) {
		ctx.setD(ctx.getS() | ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "ori $t, $s, imm16",
			template = "001101ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void ori(final InstructionContext ctx) {
		ctx.setT(ctx.getS() | ctx.getI16());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sb $t, imm16($s)",
			template = "101000ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void sb(final InstructionContext ctx) {
		ctx.getMemory().setByte(ctx.getS() + ctx.getI16(), (byte) (0xFF & ctx.getT()));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sll $d, $t, h5",
			template = "000000-----tttttdddddhhhhh000000",
			writeRegs = "$d"
	)
	public void sll(final InstructionContext ctx) {
		ctx.setD(ctx.getT() << ctx.getInstruction().getBits(Instruction.T_H5));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sllv $d, $t, $s",
			template = "000000ssssstttttddddd-----000100",
			writeRegs = "$d"
	)
	public void sllv(final InstructionContext ctx) {
		ctx.setD(ctx.getT() << ctx.getS());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sltu $d, $s, $t",
			template = "000000ssssstttttddddd00000101011",
			writeRegs = "$d",
			unsigned = true
	)
	public void sltu(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getT()) {
			ctx.setD(1);
		} else {
			ctx.setD(0);
		}
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "slt $d, $s, $t",
			template = "000000ssssstttttddddd00000101010",
			writeRegs = "$d"
	)
	public void slt(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getT()) {
			ctx.setD(1);
		} else {
			ctx.setD(0);
		}
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sltiu $t, $s, imm16",
			template = "001011ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t",
			unsigned = true
	)
	public void sltiu(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getI16()) {
			ctx.setT(1);
		} else {
			ctx.setT(0);
		}
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "slti $t, $s, imm16",
			template = "001010ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void slti(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getI16()) {
			ctx.setT(1);
		} else {
			ctx.setT(0);
		}
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sra $d, $t, h5",
			template = "000000-----tttttdddddhhhhh000011",
			writeRegs = "$d"
	)
	public void sra(final InstructionContext ctx) {
		ctx.setD(ctx.getT() >> ctx.getInstruction().getBits(Instruction.T_H5));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "srl $d, $t, h5",
			template = "000000-----tttttdddddhhhhh000010",
			writeRegs = "$d"
	)
	public void srl(final InstructionContext ctx) {
		ctx.setD(ctx.getT() >>> ctx.getInstruction().getBits(Instruction.T_H5));
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "srlv $d, $t, $s",
			template = "000000ssssstttttddddd00000000110",
			writeRegs = "$d"
	)
	public void srlv(final InstructionContext ctx) {
		ctx.setD(ctx.getT() >>> ctx.getS());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sub $d, $s, $t",
			template = "000000ssssstttttddddd00000100010",
			writeRegs = "$d"
	)
	public void sub(final InstructionContext ctx) {
		ctx.setD(ctx.getS() - ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "subu $d, $s, $t",
			template = "000000ssssstttttddddd00000100011",
			writeRegs = "$d",
			unsigned = true
	)
	public void subu(final InstructionContext ctx) {
		ctx.setD(ctx.getS() - ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "sw $t, imm16($s)",
			template = "101011ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = ""
	)
	public void sw(final InstructionContext ctx) {
		ctx.getMemory().setWord(ctx.getS() + ctx.getI16(), ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "syscall",
			template = "000000--------------------001100",
			writeRegs = ""
	)
	public void syscall(final InstructionContext ctx) {
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "xor $d, $s, $t",
			template = "000000ssssstttttddddd-----100110",
			writeRegs = "$d"
	)
	public void xor(final InstructionContext ctx) {
		ctx.setD(ctx.getS() ^ ctx.getT());
		ctx.advPc();
	}

	@InstructionDesc(
			syntax = "xori $t, $s, imm16",
			template = "001110ssssstttttiiiiiiiiiiiiiiii",
			writeRegs = "$t"
	)
	public void xori(final InstructionContext ctx) {
		ctx.setT(ctx.getS() ^ ctx.getI16());
		ctx.advPc();
	}
}