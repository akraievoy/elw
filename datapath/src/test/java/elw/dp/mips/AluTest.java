package elw.dp.mips;

import base.pattern.Result;
import junit.framework.TestCase;

import java.lang.reflect.Method;

public class AluTest extends TestCase {
	protected InstructionContext iCtx;
	protected Memory memory;

	public void testAnnotations() {
		final Method[] aluMethods = Alu.class.getDeclaredMethods();

		for (Method m : aluMethods) {
			final InstructionDesc descAnnot = m.getAnnotation(InstructionDesc.class);
			assertNotNull(m.getName(), descAnnot);

			assertEquals(m.getName(), 32, descAnnot.template().length());
			assertTrue(m.getName(), descAnnot.syntax().startsWith(m.getName()));
		}
	}

	private MipsValidator validator;
	protected Registers registers;
	private Alu alu;

	public void setUp() {
		validator = new MipsValidator();
		alu = validator.getDataPath().getAlu();
		registers = validator.getDataPath().getRegisters();
		memory = validator.getDataPath().getMemory();
		iCtx = new InstructionContext(
				validator.getDataPath().getInstructions(),
				memory,
				registers
		);
	}

	protected void assembleInstruction(final String instructionCode) {
		final Instruction instruction = validator.assemble(new Result[1], new String[]{instructionCode})[0];
		iCtx.setInstruction(instruction);
	}

	public void testAdd() {
		registers.setReg(Reg.t2, -1);
		registers.setReg(Reg.t3, 2);
		registers.setReg(Reg.t1, 18);

		assembleInstruction("add $t1, $t2, $t3");

		alu.add(iCtx);

		assertEquals(-1, registers.getReg(Reg.t2));
		assertEquals(2, registers.getReg(Reg.t3));
		assertEquals(1, registers.getReg(Reg.t1));
	}

}
