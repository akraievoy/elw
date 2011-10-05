package elw.dp.mips;

public class InstructionContext {
    private final Instructions instructions;
    private final Memory memory;
    private final Registers registers;

    private Instruction instruction;

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
        base.Die.ifNull("instruction.s", instruction.getReg(Instruction.T_REG_S));

        return registers.getReg(instruction.getReg(Instruction.T_REG_S));
    }

    public int setS(final int s) {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.s", instruction.getReg(Instruction.T_REG_S));

        return registers.setReg(instruction.getReg(Instruction.T_REG_S), s);
    }

    public int getT() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.t", instruction.getReg(Instruction.T_REG_T));

        return registers.getReg(instruction.getReg(Instruction.T_REG_T));
    }

    public int setT(final int t) {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.t", instruction.getReg(Instruction.T_REG_T));

        return registers.setReg(instruction.getReg(Instruction.T_REG_T), t);
    }

    public int getD() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.d", instruction.getReg(Instruction.T_REG_D));

        return registers.getReg(instruction.getReg(Instruction.T_REG_D));
    }

    public int setD(final int d) {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.d", instruction.getReg(Instruction.T_REG_D));

        return registers.setReg(instruction.getReg(Instruction.T_REG_D), d);
    }

    public int getI16() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.i16", instruction.getBits(Instruction.T_IMM16));

        return instruction.getBits(Instruction.T_IMM16);
    }

    public int getI26() {
        base.Die.ifNull("instruction", instruction);
        base.Die.ifNull("instruction.i26", instruction.getBits(Instruction.T_IMM26));

        return instruction.getBits(Instruction.T_IMM26);
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
