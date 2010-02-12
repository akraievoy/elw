package elw.dp.mips.asm;

import elw.dp.mips.*;

import java.util.regex.*;
import java.util.Map;

public class IType_26 extends InstructionAssembler {
    static final String RE = "^(" + RE_OP + ") (" + RE_NUM_OR_LABEL + ")$";
    static final Pattern PATTERN = Pattern.compile(RE);

    protected void assembleInternal(final Matcher matcher, final Instruction template) throws AssemblyException {
        final String group2 = matcher.group(2);
        if (PATTERN_NUM.matcher(group2).matches()) {
            template.setI26(parseNum(group2));
        }
    }

    protected void assembleDepsInternal(final Matcher matcher, Instruction template, Map<String, Integer> labelMap) throws AssemblyException {
        final String group2 = matcher.group(2);
        final int labelOffs = parseLabel(group2, template, labelMap);

        template.setI26(labelOffs-template.getMemOffset());
    }

    protected Pattern getPattern() {
        return PATTERN;
    }
}
