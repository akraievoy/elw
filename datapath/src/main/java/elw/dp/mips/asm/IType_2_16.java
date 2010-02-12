package elw.dp.mips.asm;

import elw.dp.mips.*;

import java.util.regex.*;

public class IType_2_16 extends InstructionAssembler {
    static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + "), \\$(" + RE_REG + "), (" + RE_NUM + ")$";
    static final Pattern PATTERN = Pattern.compile(RE);

    protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
        template.setT(reg(matcher.group(2)));
        template.setS(reg(matcher.group(3)));

        template.setI16(parseNum(matcher.group(4)));
    }

    protected Pattern getPattern() {
        return PATTERN;
    }
}
