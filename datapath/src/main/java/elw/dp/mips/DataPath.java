package elw.dp.mips;

import gnu.trove.TIntArrayList;
import org.akraievoy.base.Die;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class DataPath {
    private final Instructions instructions = new Instructions();
    private final Memory memory = new Memory();
    private final Registers registers = new Registers();

    private final Alu alu = new Alu();

    private final InstructionContext ctx;
    private int stepsToWipeHiLo = -1;

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
        if (instruction != null) {
            ctx.setInstruction(instruction);

            final Method method;

            try {
                method = alu.getClass().getMethod(instruction.getOpName(), InstructionContext.class);
            } catch (NoSuchMethodException e) {
                throw Die.ifReached(e);
            }

            try {
                method.invoke(alu, ctx);
            } catch (IllegalAccessException e) {
                throw Die.ifReached(e);
            } catch (InvocationTargetException e) {
                throw Die.ifReached(e);
            }
        }

        final TIntArrayList writeRegs = registers.getWriteRegs();
        final TIntArrayList readRegs = registers.getReadRegs();

        final boolean hiLoWritten = writeRegs.contains(Reg.hi.ordinal()) || writeRegs.contains(Reg.lo.ordinal());
        final boolean hiLoRead = readRegs.contains(Reg.hi.ordinal()) || readRegs.contains(Reg.lo.ordinal());

        if (hiLoWritten) {
            stepsToWipeHiLo = 2;
        } else if (hiLoRead && stepsToWipeHiLo > 0) {
            stepsToWipeHiLo--;
        }
        if (!hiLoWritten && (!hiLoRead || stepsToWipeHiLo == 0)) {
            registers.unset(new int[]{Reg.hi.ordinal(), Reg.lo.ordinal()});
            stepsToWipeHiLo = -1;
        }

        instructions.updateMinStack(registers.getRegInternal(Reg.sp));
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
