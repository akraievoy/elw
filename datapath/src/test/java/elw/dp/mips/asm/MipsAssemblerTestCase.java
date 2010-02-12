package elw.dp.mips.asm;

import junit.framework.TestCase;

public class MipsAssemblerTestCase extends TestCase {
	public void testPatterns() {
		assertTrue(MipsAssembler.PATTERN_LABELS.matcher("label1").matches());
		assertTrue(MipsAssembler.PATTERN_LABELS.matcher("\t label1   \t").matches());
		assertTrue(MipsAssembler.PATTERN_LABELS.matcher("\t label1   \t, label2").matches());
		assertTrue(MipsAssembler.PATTERN_LABELS.matcher("\t label1   \t, label2,_label3").matches());

		assertFalse(MipsAssembler.PATTERN_LABELS.matcher("\t label1  , \t, label2,_label3").matches());
		assertFalse(MipsAssembler.PATTERN_LABELS.matcher("\t, label1  \t, label2,_label3").matches());
		assertFalse(MipsAssembler.PATTERN_LABELS.matcher("\t label1  \t, la bel2,_label3").matches());
		assertFalse(MipsAssembler.PATTERN_LABELS.matcher("\t 4label1   \t, label2,_label3").matches());
	}
}

