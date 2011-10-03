package elw.dp.mips;

import base.pattern.Result;
import junit.framework.TestCase;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

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
		assembleInstructions(Arrays.asList(
				instructionCode,
				"noop",
				"label: noop"
		));
	}

	protected void assembleInstructions(final List<String> intstructions) {
		final Instruction instruction = validator.assemble(new Result[1], intstructions)[0];
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

	public void testAdd_forOverflow() {
		registers.setReg(Reg.t1, 18);
		registers.setReg(Reg.t2, 2);
		registers.setReg(Reg.t3, Integer.MAX_VALUE);

		assembleInstruction("add $t1, $t2, $t3");

		alu.add(iCtx);

		assertEquals(Integer.MIN_VALUE + 1, registers.getReg(Reg.t1));
		assertEquals(2, registers.getReg(Reg.t2));
		assertEquals(Integer.MAX_VALUE, registers.getReg(Reg.t3));
	}

	public void testBGEZAL_forJAL() {
		registers.setReg(Reg.t1, 18);

		assembleInstruction("bgezal $t1, label");

		final int codeBase = validator.getDataPath().getInstructions().getCodeBase();
		assertEquals(codeBase, registers.getReg(Reg.pc));

		alu.bgezal(iCtx);

		assertEquals(18, registers.getReg(Reg.t1));
		assertEquals(codeBase + 8, registers.getReg(Reg.pc));
		assertEquals(codeBase + 4, registers.getReg(Reg.ra));
	}

	public void testBGEZAL_forDefault() {
		registers.setReg(Reg.t1, -18);
		registers.setReg(Reg.ra, 0x123456);

		assembleInstruction("bgezal $t1, label");

		final int codeBase =
			validator.getDataPath().getInstructions().getCodeBase();
		assertEquals(codeBase, registers.getReg(Reg.pc));

		alu.bgezal(iCtx);

		assertEquals(-18, registers.getReg(Reg.t1));
		assertEquals(codeBase + 4, registers.getReg(Reg.pc));
		assertEquals(0x123456, registers.getReg(Reg.ra));
	}

}
