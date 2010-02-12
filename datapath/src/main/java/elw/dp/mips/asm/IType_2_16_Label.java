package elw.dp.mips.asm;

import elw.dp.mips.Instruction;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IType_2_16_Label extends InstructionAssembler {
	static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + "), \\$(" + RE_REG + "), (" + RE_NUM_OR_LABEL + ")$";
	static final Pattern PATTERN = Pattern.compile(RE);

	protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
		template.setS(reg(matcher.group(2)));
		template.setT(reg(matcher.group(3)));

		final String group4 = matcher.group(4);
		if (PATTERN_NUM.matcher(group4).matches()) {
			template.setI16(parseNum(group4));
		}
	}

	protected void assembleDepsInternal(Matcher matcher, Instruction template, Map<String, Integer> labelMap) throws AssemblyException {
		final String group4 = matcher.group(4);
		final int labelOffs = parseLabel(group4, template, labelMap); //  this is already counted in instructions, not bytes

		template.setI16(labelOffs - template.getMemOffset());
	}

	protected Pattern getPattern() {
		return PATTERN;
	}
}
