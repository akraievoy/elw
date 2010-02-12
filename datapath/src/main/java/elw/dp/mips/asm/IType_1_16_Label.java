package elw.dp.mips.asm;

import elw.dp.mips.Instruction;

import java.util.regex.*;
import java.util.Map;

public class IType_1_16_Label extends InstructionAssembler {
    static final String RE = "^(" + RE_OP + ") \\$(" + RE_REG + "), (" + RE_NUM_OR_LABEL + ")$";

    static final Pattern PATTERN = Pattern.compile(RE);

    protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
        if ("lui".equals(template.getOpName())) {
            template.setT(reg(matcher.group(2)));
        } else {
            template.setS(reg(matcher.group(2)));
        }

        final String group3 = matcher.group(3);
        if (PATTERN_NUM.matcher(group3).matches()) {
            template.setI16(parseNum(group3));
        }
    }

    protected void assembleDepsInternal(final Matcher matcher, Instruction template, Map<String, Integer> labelMap) throws AssemblyException {
        final String group3 = matcher.group(3);
        final int labelOffs = parseLabel(group3, template, labelMap); //  this is already counted in instructions, not bytes

        template.setI16(labelOffs - template.getMemOffset());
    }

    protected Pattern getPattern() {
        return PATTERN;
    }
}
