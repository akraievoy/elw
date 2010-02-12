package elw.dp.mips.asm;

import elw.dp.mips.Instruction;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RType_2 extends InstructionAssembler {
	static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + "), \\$(" + RE_REG + ")$";
	static final Pattern PATTERN = Pattern.compile(RE);

	protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
		template.setS(reg(matcher.group(2)));
		template.setT(reg(matcher.group(3)));
	}

	protected Pattern getPattern() {
		return PATTERN;
	}
}
