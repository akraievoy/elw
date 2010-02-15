package elw.dp.mips;

import junit.framework.TestCase;

import java.lang.reflect.Method;

public class AluTest extends TestCase {
	public void testAnnotations() {
		final Method[] aluMethods = Alu.class.getDeclaredMethods();

		for (Method m : aluMethods) {
			final InstructionDesc descAnnot = m.getAnnotation(InstructionDesc.class);
			assertNotNull(m.getName(), descAnnot);

			assertEquals(m.getName(), 32, descAnnot.template().length());
			assertTrue(m.getName(), descAnnot.syntax().startsWith(m.getName()));
		}
	}
}
