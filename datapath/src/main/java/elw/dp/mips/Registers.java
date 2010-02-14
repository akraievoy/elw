package elw.dp.mips;

import gnu.trove.TIntArrayList;
import gnu.trove.TIntIntHashMap;

public class Registers {
	final TIntIntHashMap regToValue = new TIntIntHashMap();

	final TIntArrayList readRegs = new TIntArrayList();
	final TIntArrayList writeRegs = new TIntArrayList();

	public int getReg(Reg reg) {
		readRegs.add(reg.ordinal());

		return getRegInternal(reg);
	}

	public int getRegInternal(final Reg reg) {
		final Integer integer = regToValue.get(reg.ordinal());

		return integer == null ? 0 : integer;
	}

	public int setReg(Reg reg, int value) {
		writeRegs.add(reg.ordinal());

		final Integer oldValue = regToValue.put(reg.ordinal(), value);
		return oldValue != null ? oldValue : 0;
	}

	public void resetAccess() {
		readRegs.clear();
		writeRegs.clear();
	}

	public TIntArrayList getReadRegs() {
		return readRegs;
	}

	public TIntArrayList getWriteRegs() {
		return writeRegs;
	}

	public void load(final TIntIntHashMap lastLoaded) {
		resetAccess();

		for (Reg reg : Reg.values()) {
			final int regIndex = reg.ordinal();
			final int regValue = lastLoaded.containsKey(regIndex) ? lastLoaded.get(regIndex) : 0;
			regToValue.put(regIndex, regValue);
		}
	}
}
