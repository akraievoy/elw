package elw.dp.mips.asm;

import elw.dp.mips.Instruction;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RType_2_5 extends InstructionAssembler {
	static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + "), \\$(" + RE_REG + "), (" + RE_NUM + ")$";
	static final Pattern PATTERN = Pattern.compile(RE);

	protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
		template.setD(reg(matcher.group(2)));
		template.setT(reg(matcher.group(3)));
		template.setH(parseNum(matcher.group(4)));
	}

	protected Pattern getPattern() {
		return PATTERN;
	}
}
