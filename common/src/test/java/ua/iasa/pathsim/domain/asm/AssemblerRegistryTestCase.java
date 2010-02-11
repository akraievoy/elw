package ua.iasa.pathsim.domain.asm;

import junit.framework.TestCase;

public class AssemblerRegistryTestCase extends TestCase {
    AssemblerRegistry registry;

    public void setUp() {
        registry = AssemblerRegistry.create();
    }

    public void testAssemble() throws Exception {
        assertNotNull(registry.assemble("add $t1, $t2, $a0", 1));
        assertNotNull(registry.assemble("   add $t1, $t2, $a0  ", 1));
        assertNotNull(registry.assemble("   add $3, $0, $5  ", 1));

        assertNotNull(registry.assemble("bgtz $t0, 12", 1));
        assertNotNull(registry.assemble("BLEZ $T3, -0x12", 1));

        assertNotNull(registry.assemble("\tadd\t$3,\t$0,\t$5\t", 1));
        assertNotNull(registry.assemble("\txori\t$3,\t$3, -18\t", 1));
        assertNotNull(registry.assemble("\tandi\t$3,\t$3, -0x2\t", 1));
        assertNotNull(registry.assemble("\tbne\t$3,\t$0,\t0xFF\t", 1));

        assertNotNull(registry.assemble("add\t$s0,\t$zero,\t$a0", 1));
    }
}

