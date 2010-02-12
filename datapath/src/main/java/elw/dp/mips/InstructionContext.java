package elw.dp.mips;

public class InstructionContext {
    protected final Instructions instructions;
    protected final Memory memory;
    protected final Registers registers;

    protected Instruction instruction;

    public InstructionContext(final Instructions instructions, final Memory memory, final Registers registers) {
        this.instructions = instructions;
        this.memory = memory;
        this.registers = registers;
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

    public Instruction getInstruction() {
        return instruction;
    }

    public void setInstruction(final Instruction instruction) {
        this.instruction = instruction;
    }

    public int getS() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.s", instruction.getS());

        return registers.getReg(instruction.getS());
    }

    public int setS(final int s) {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.s", instruction.getS());

        return registers.setReg(instruction.getS(), s);
    }

    public int getT() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.t", instruction.getT());

        return registers.getReg(instruction.getT());
    }

    public int setT(final int t) {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.t", instruction.getT());

        return registers.setReg(instruction.getT(), t);
    }

    public int getD() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.d", instruction.getD());

        return registers.getReg(instruction.getD());
    }

    public int setD(final int d) {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.d", instruction.getD());

        return registers.setReg(instruction.getD(), d);
    }

    public int getI16() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.i16", instruction.getI16());

        return instruction.getI16();
    }

    public int getI26() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.i26", instruction.getI26());

        return instruction.getI26();
    }

    protected void advPc() {
        advPc(4);
    }

    protected void advPc(int offset) {
        registers.setReg(Reg.pc, registers.getReg(Reg.pc) + offset);
    }

    public void storeRa() {
        registers.setReg(Reg.ra, registers.getReg(Reg.pc) + 4);
    }
}
