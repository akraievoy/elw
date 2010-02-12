package elw.dp.mips;

import elw.dp.mips.asm.AssemblerRegistry;
import elw.dp.mips.asm.AssemblyException;
import elw.dp.mips.asm.Data;
import elw.dp.mips.asm.Noop;

import java.util.regex.Pattern;

public class Instruction {
	protected static final AssemblerRegistry.InstructionSetup NOOP_SETUP =
			new AssemblerRegistry.InstructionSetup("noop", new Noop(), "00000000000000000000000000000000", "noop");
	public static final Instruction NOOP = new Instruction(NOOP_SETUP, "noop", 0);

	private static final Pattern PATTERN_COMPLETE = Pattern.compile("^[-01]+$");

	private static final String MASK_S = "sssss";
	private static final String MASK_T = "ttttt";
	private static final String MASK_D = "ddddd";
	private static final String MASK_I16 = "iiiiiiiiiiiiiiii";
	private static final String MASK_I26 = "iiiiiiiiiiiiiiiiiiiiiiiiii";
	private static final String MASK_H = "hhhhh";

	StringBuffer template;

	final AssemblerRegistry.InstructionSetup setup;
	final int memOffset;

	Reg s;
	Reg t;
	Reg d;

	Integer i16;
	Integer i26;
	Integer h;

	String codeLine;

	public Instruction(AssemblerRegistry.InstructionSetup setup, String codeLine, int memOffset) {
		this.codeLine = codeLine;
		this.setup = setup;
		this.memOffset = memOffset;
		this.template = new StringBuffer(setup.template);
	}

	public void setS(Reg reg) throws AssemblyException {
		replaceMask(MASK_S, Data.uint2bin(reg.ordinal(), 5, "s"));

		s = reg;
	}

	public void setD(Reg reg) throws AssemblyException {
		replaceMask(MASK_D, Data.uint2bin(reg.ordinal(), 5, "d"));

		d = reg;
	}

	public void setT(Reg reg) throws AssemblyException {
		replaceMask(MASK_T, Data.uint2bin(reg.ordinal(), 5, "t"));

		t = reg;
	}

	public void setI16(int value) throws AssemblyException {
		if (setup.unsigned) {
			replaceMask(MASK_I16, Data.uint2bin(value, 16, "imm16"));
		} else {
			replaceMask(MASK_I16, Data.int2bin(value, 16, "imm16"));
		}

		i16 = value;
	}

	public void setI26(int value) throws AssemblyException {
		replaceMask(MASK_I26, Data.int2bin(value, 26, "imm26"));
		i26 = value;
	}

	public void setH(int value) throws AssemblyException {
		replaceMask(MASK_H, Data.uint2bin(value, 5, "h"));
		h = value;
	}

	public Reg getS() {
		return s;
	}

	public Reg getT() {
		return t;
	}

	public Reg getD() {
		return d;
	}

	public Integer getI16() {
		return i16;
	}

	public Integer getI26() {
		return i26;
	}

	public Integer getH() {
		return h;
	}

	public String getOpName() {
		return setup.name;
	}

	protected void replaceMask(final String maskSource, final String str) {
		final int srcStart = template.indexOf(maskSource);

		base.Die.ifFalse(srcStart > 0, "mask '" + maskSource + "' not found in template: '" + template + "'");

		template.replace(srcStart, srcStart + maskSource.length(), str);
	}

	public String getBinaryCode() {
		return template.toString();
	}

	public boolean isAssembled() {
		return PATTERN_COMPLETE.matcher(template).matches();
	}

	/**
	 * Instructions, not bytes.
	 */
	public int getMemOffset() {
		return memOffset;
	}

	public AssemblerRegistry.InstructionSetup getSetup() {
		return setup;
	}

	public String getCodeLine() {
		return codeLine;
	}

	public String toString() {
		return "'" + setup + "' : '" + template + "'";
	}

	public int intValue() {
		base.Die.ifFalse(isAssembled(), "should be assembled: " + this);

		final String templateValue = template.toString();
		final String templateBinValue = templateValue.replaceAll("-", "0");
		base.Die.ifFalse(templateBinValue.length() == 32, "wrong bit length: '" + templateBinValue + "'");

		return Data.bin2int(templateBinValue);
	}
}
