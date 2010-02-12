package elw.dp.mips.asm;

import elw.dp.mips.*;

import java.util.regex.*;

public class RType_1 extends InstructionAssembler {
    static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + ")$";
    static final Pattern PATTERN = Pattern.compile(RE);

    protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
        final String opName = template.getOpName();

        if ("mfhi".equals(opName) || "mflo".equals(opName)) {
            template.setD(reg(matcher.group(2)));
        } else {
            template.setS(reg(matcher.group(2)));
        }
    }

    protected Pattern getPattern() {
        return PATTERN;
    }
}
