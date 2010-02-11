package ua.iasa.pathsim.domain.asm;

import ua.iasa.pathsim.domain.*;

import java.util.regex.*;

public class IType_2_16rel extends InstructionAssembler {
    static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + "), (" + RE_NUM + ")\\(\\$(" + RE_REG + ")\\)$";
    static final Pattern PATTERN = Pattern.compile(RE);

    protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
        template.setT(reg(matcher.group(2)));
        template.setS(reg(matcher.group(4)));
        template.setI16(parseNum(matcher.group(3)));
    }

    protected Pattern getPattern() {
        return PATTERN;
    }
}
