package elw.dp.mips;

public class Alu {
	public void addu(final InstructionContext ctx) {
		ctx.setD(ctx.getS() + ctx.getT());
	}

	public void add(final InstructionContext ctx) {
		ctx.setD(ctx.getS() + ctx.getT());
	}

	public void addi(final InstructionContext ctx) {
		ctx.setT(ctx.getS() + ctx.getI16());
	}

	public void addiu(final InstructionContext ctx) {
		ctx.setD(ctx.getS() + ctx.getI16());
	}

	public void and(final InstructionContext ctx) {
		ctx.setT(ctx.getS() & ctx.getT());
	}

	public void andi(final InstructionContext ctx) {
		ctx.setT(ctx.getS() & ctx.getI16());
	}

	public void beq(final InstructionContext ctx) {
		if (ctx.getS() == ctx.getT()) {
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void bgez(final InstructionContext ctx) {
		if (ctx.getS() >= 0) {
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void bgezal(final InstructionContext ctx) {
		if (ctx.getS() >= 0) {
			ctx.storeRa();
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void bgtz(final InstructionContext ctx) {
		if (ctx.getS() > 0) {
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void blez(final InstructionContext ctx) {
		if (ctx.getS() <= 0) {
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void bltz(final InstructionContext ctx) {
		if (ctx.getS() < 0) {
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void bltzal(final InstructionContext ctx) {
		if (ctx.getS() < 0) {
			ctx.storeRa();
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void bne(final InstructionContext ctx) {
		if (ctx.getS() != ctx.getT()) {
			ctx.advPc(ctx.getI16() << 2);
		}
	}

	public void divu(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		ctx.getRegisters().setReg(Reg.lo, s / t);
		ctx.getRegisters().setReg(Reg.hi, s % t);
	}

	public void div(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		ctx.getRegisters().setReg(Reg.lo, s / t);
		ctx.getRegisters().setReg(Reg.hi, s % t);
	}

	public void j(final InstructionContext ctx) {
		final Registers regs = ctx.getRegisters();

		regs.setReg(Reg.pc, regs.getReg(Reg.pc) & 0xF0000000 | ctx.getI26());
	}

	public void jal(final InstructionContext ctx) {
		final Registers regs = ctx.getRegisters();
		ctx.storeRa();
		regs.setReg(Reg.pc, regs.getReg(Reg.pc) & 0xF0000000 | ctx.getI26());
	}

	public void jr(final InstructionContext ctx) {
		final Registers regs = ctx.getRegisters();

		regs.setReg(Reg.pc, regs.getReg(Reg.ra));
	}

	public void lb(final InstructionContext ctx) {
		ctx.setT(ctx.getMemory().getByte(ctx.getS() + ctx.getI16()));
	}

	public void lui(final InstructionContext ctx) {
		ctx.setT(ctx.getI16() << 16);
	}

	public void lw(final InstructionContext ctx) {
		ctx.setT(ctx.getMemory().getWord(ctx.getS() + ctx.getI16()));
	}

	public void mfhi(final InstructionContext ctx) {
		ctx.setD(ctx.getRegisters().getReg(Reg.hi));
	}

	public void mflo(final InstructionContext ctx) {
		ctx.setD(ctx.getRegisters().getReg(Reg.lo));
	}

	public void multu(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		final long value = ((long) s & 0xFFFFFFFFL) * ((long) t & 0xFFFFFFFFL);

		ctx.getRegisters().setReg(Reg.lo, (int) value);
		ctx.getRegisters().setReg(Reg.hi, (int) (value >> 32));
	}

	public void mult(final InstructionContext ctx) {
		final int s = ctx.getS();
		final int t = ctx.getT();

		final long value = (long) s * t;

		ctx.getRegisters().setReg(Reg.lo, (int) value);
		ctx.getRegisters().setReg(Reg.hi, (int) (value >> 32));
	}

	public void noop(final InstructionContext ctx) {
	}

	public void or(final InstructionContext ctx) {
		ctx.setD(ctx.getS() | ctx.getT());
	}

	public void ori(final InstructionContext ctx) {
		ctx.setT(ctx.getS() | ctx.getI16());
	}

	public void sb(final InstructionContext ctx) {
		ctx.getMemory().setByte(ctx.getS() + ctx.getI16(), (byte) (0xFF & ctx.getT()));
	}

	public void sll(final InstructionContext ctx) {
		ctx.setD(ctx.getT() << ctx.getInstruction().getH());
	}

	public void sllv(final InstructionContext ctx) {
		ctx.setD(ctx.getT() << ctx.getS());
	}

	public void sltu(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getT()) {
			ctx.setD(1);
		} else {
			ctx.setD(0);
		}
	}

	public void slt(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getT()) {
			ctx.setD(1);
		} else {
			ctx.setD(0);
		}
	}

	public void sltiu(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getI16()) {
			ctx.setT(1);
		} else {
			ctx.setT(0);
		}
	}

	public void slti(final InstructionContext ctx) {
		if (ctx.getS() < ctx.getI16()) {
			ctx.setT(1);
		} else {
			ctx.setT(0);
		}
	}

	public void sra(final InstructionContext ctx) {
		ctx.setD(ctx.getT() >> ctx.getInstruction().getH());
	}

	public void srl(final InstructionContext ctx) {
		ctx.setD(ctx.getT() >>> ctx.getInstruction().getH());
	}

	public void srlv(final InstructionContext ctx) {
		ctx.setD(ctx.getT() >>> ctx.getS());
	}

	public void sub(final InstructionContext ctx) {
		ctx.setD(ctx.getS() - ctx.getT());
	}

	public void subu(final InstructionContext ctx) {
		ctx.setD(ctx.getS() - ctx.getT());
	}

	public void sw(final InstructionContext ctx) {
		ctx.getMemory().setWord(ctx.getS() + ctx.getI16(), ctx.getT());
	}

	public void syscall(final InstructionContext ctx) {
	}

	public void xor(final InstructionContext ctx) {
		ctx.setD(ctx.getS() ^ ctx.getT());
	}

	public void xori(final InstructionContext ctx) {
		ctx.setT(ctx.getS() ^ ctx.getI16());
	}
}