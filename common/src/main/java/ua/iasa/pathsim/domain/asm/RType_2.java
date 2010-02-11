package ua.iasa.pathsim.domain.asm;

import ua.iasa.pathsim.domain.*;

import java.util.regex.*;

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
