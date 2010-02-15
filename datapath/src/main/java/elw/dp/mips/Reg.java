package elw.dp.mips;

import org.akraievoy.gear.G4Parse;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public enum Reg {
	zero, at, v0, v1,
	a0, a1, a2, a3,

	t0, t1, t2, t3,
	t4, t5, t6, t7,

	s0, s1, s2, s3,
	s4, s5, s6, s7,

	t8, t9, k0, k1,
	gp, sp, fp, ra,

	pc, hi, lo; //  those are extended, usages should be cut off by validations

	public final static List<Reg> publicRegs = Arrays.asList(zero, v0, v1, a0, a1, a2, a3, t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, s2, s3, s4, s5, s6, s7, t8, t9, sp, ra, pc);
	public final static List<Reg> roRegs = Arrays.asList(zero);
	public final static List<Reg> autoRegs = Arrays.asList(pc, sp, ra);
	public final static List<Reg> tempRegs = Arrays.asList(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, pc, hi, lo);

	protected static final Map<String, Reg> byName = new TreeMap<String, Reg>();

	protected static TreeMap<String, Reg> createByNameMap() {
		final TreeMap<String, Reg> byNameMap = new TreeMap<String, Reg>();

		for (Reg reg : values()) {
			byName.put(reg.toString().toLowerCase(), reg);
		}

		return byNameMap;
	}

	public synchronized static Map<String, Reg> getByName() {
		if (byName.isEmpty()) {
			byName.putAll(createByNameMap());
		}

		return byName;
	}

	public static Reg[] values(int[] regOrdinals) {
		final Reg[] regs = new Reg[regOrdinals.length];

		for (int i = 0; i < regOrdinals.length; i++) {
			regs[i] = values()[regOrdinals[i]];
		}

		return regs;
	}
}
