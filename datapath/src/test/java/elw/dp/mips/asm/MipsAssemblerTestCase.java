package elw.dp.mips.asm;

import base.pattern.Result;
import elw.dp.mips.Instruction;
import junit.framework.TestCase;

import java.util.Arrays;
import java.util.HashMap;

public class MipsAssemblerTestCase extends TestCase {
	public void testPatterns() {
		final MipsAssembler asm = new MipsAssembler();
		final HashMap<String, Integer> indexes = new HashMap<String, Integer>();

		final Result[] results = {new Result("oops", false)};
		final Instruction[] instructions = asm.loadInstructions(
				Arrays.asList(
						"begin:",
						"\tadd\t$3,\t$0,\t$5\t",
						"add $t1, $t2, $a0",
						"   add $t1, $t2, $a0  ",
						"bgez $t5, forward",
						"   add $3, $0, $5  ",
						"bgtz $t0, 12",
						"forward: BLEZ $T3, -0x12",
						"\txori\t$3,\t$3, -18\t",
						"\tandi\t$3,\t$3, -0x2\t",
						"\tbne\t$3,\t$0,\t0xFF\t",
						"add\t$s0,\t$zero,\t$a0",
						"j begin"
				),
				results,
				indexes
		);

		assertTrue(results[0].getMessage(), results[0].isSuccess());
		assertNotNull(instructions);
	}
}

