package elw.dp.mips.asm;

import elw.dp.mips.Instruction;
import elw.dp.mips.Reg;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class InstructionAssembler {
	/**
	 * Please no groups here
	 */
	public static final String RE_OP = "[a-zA-Z]+";
	/**
	 * Please no groups here
	 */
	public static final String RE_REG = "[a-zA-Z]+\\d?|\\d+";
	/**
	 * Please no groups here
	 */
	public static final String RE_NUM = "-?0x[0-9a-f]+|-?[0-9]+";
	/**
	 * Please no groups here
	 */
	public static final String RE_LABEL = "[a-zA-Z_][a-zA-Z_0-9]*";
	/**
	 * Please no groups here
	 */
	public static final String RE_NUM_OR_LABEL = RE_NUM + "|" + RE_LABEL;

	protected static final Pattern PATTERN_NUM = Pattern.compile(RE_NUM);

	protected static final Pattern PATTERN_LABEL = Pattern.compile(RE_LABEL);

	public Instruction assemble(final Instruction template) throws AssemblyException {
		final Matcher matcher = getPattern().matcher(template.getCodeLine());

		if (!matcher.matches()) {
			throw AssemblyException.formatUnsupported(template.getCodeLine(), template.getSetup().syntax);
		}

		base.Die.ifNotEqual("opName", template.getOpName(), matcher.group(1));

		assembleInternal(matcher, template);

		return template;
	}

	protected abstract Pattern getPattern();

	protected abstract void assembleInternal(Matcher matcher, Instruction template) throws AssemblyException;

	public int parseNum(final String s) {
		final boolean negative = s.startsWith("-");
		final String number = negative ? s.substring(1) : s;

		final boolean hex = number.startsWith("0x");
		final String digits = hex ? number.substring(2) : number;

		final int absValue = hex ? Integer.parseInt(digits, 16) : Integer.parseInt(digits);

		return negative ? -absValue : absValue;
	}

	protected Reg reg(final String regGroup) {
		return Reg.fromString(regGroup);
	}

	public void assembleDeps(Instruction template, Map<String, Integer> labelMap) throws AssemblyException {
		final Matcher matcher = getPattern().matcher(template.getCodeLine());

		if (!matcher.matches()) {
			throw AssemblyException.formatUnsupported(template.getCodeLine(), template.getSetup().syntax);
		}

		if (!template.isAssembled()) {
			assembleDepsInternal(matcher, template, labelMap);
		}
	}

	protected void assembleDepsInternal(final Matcher matcher, Instruction template, Map<String, Integer> labelMap) throws AssemblyException {
		throw AssemblyException.internalError("Second-pass assembler code required for template: " + template);
	}

	protected int parseLabel(final String labelGroup, final Instruction template, final Map<String, Integer> labelMap) throws AssemblyException {
		if (!PATTERN_LABEL.matcher(labelGroup).matches()) {
			throw AssemblyException.internalError("Label '" + labelGroup + "' is invalid");
		}

		final Integer labelOffs = labelMap.get(labelGroup);
		if (labelOffs == null) {
			throw AssemblyException.missingLabel(template.getCodeLine(), labelGroup);
		}

		return labelOffs;
	}
}
