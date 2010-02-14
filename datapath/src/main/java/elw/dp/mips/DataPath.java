package elw.dp.mips;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class DataPath {
	protected final Instructions instructions = new Instructions();
	protected final Memory memory = new Memory();
	protected final Registers registers = new Registers();

	protected final Alu alu = new Alu();

	protected final InstructionContext ctx;

	public DataPath() {
		ctx = new InstructionContext(instructions, memory, registers);
	}

	public Instruction execute() {
		instructions.resetAccess();
		memory.resetAccess();
		registers.resetAccess();

		final int pc = registers.getReg(Reg.pc);
		if (!instructions.hasInstruction(pc)) {
			return null;
		}

		final Instruction instruction = instructions.get(pc);
		ctx.setInstruction(instruction);

		final Method method;

		try {
			method = alu.getClass().getMethod(instruction.getOpName(), InstructionContext.class);
		} catch (NoSuchMethodException e) {
			throw base.Die.ifReached(e);
		}

		try {
			method.invoke(alu, ctx);
		} catch (IllegalAccessException e) {
			throw base.Die.ifReached(e);
		} catch (InvocationTargetException e) {
			throw base.Die.ifReached(e);
		}

		if (!registers.getWriteRegs().contains(Reg.pc.ordinal())) {
			ctx.advPc();
		}
		instructions.updateMinStack();
		return instruction;
	}

	public Instructions getInstructions() {
		return instructions;
	}

	public Memory getMemory() {
		return memory;
	}

	public Registers getRegisters() {
		return registers;
	}

	public Alu getAlu() {
		return alu;
	}

	public InstructionContext getCtx() {
		return ctx;
	}

	public void reset() {

	}
}
