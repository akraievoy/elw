package ua.iasa.pathsim.domain.asm;

import ua.iasa.pathsim.domain.Instruction;

import java.util.regex.*;

public class IType_1_16 extends InstructionAssembler {
    static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + "), (" + RE_NUM + ")$";

    static final Pattern PATTERN = Pattern.compile(RE);
    static final Pattern PATTERN_NUM = Pattern.compile(RE_NUM);

    protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
        if ("lui".equals(template.getOpName())) {
            template.setT(reg(matcher.group(2)));
        } else {
            template.setS(reg(matcher.group(2)));
        }

        template.setI16(parseNum(matcher.group(3)));
    }

    protected Pattern getPattern() {
        return PATTERN;
    }
}
