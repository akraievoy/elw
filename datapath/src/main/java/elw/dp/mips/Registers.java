package elw.dp.mips;

import gnu.trove.TIntArrayList;
import gnu.trove.TIntIntHashMap;

import java.util.Arrays;

public class Registers {
	final TIntIntHashMap regToValue = new TIntIntHashMap();

	final TIntArrayList readRegs = new TIntArrayList();
	final TIntArrayList writeRegs = new TIntArrayList();

	public int getReg(Reg reg) {
		readRegs.add(reg.ordinal());

		return getRegInternal(reg);
	}

	public int getRegInternal(final Reg reg) {
		final int key = reg.ordinal();

		return regToValue.get(key);
	}

	public int setReg(Reg reg, int value) {
		final int key = reg.ordinal();
		writeRegs.add(key);

		return regToValue.put(key, value);
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

	public int[] getSetupRegOrdinals() {
		//	LATER optimize here
		final int[] keys = regToValue.keys();
		Arrays.sort(keys);
		return keys;
	}

	public Reg[] getSetupRegs() {
		final int[] regOrdinals = getSetupRegOrdinals();
		final Reg[] regs = new Reg[regOrdinals.length];

		for (int i = 0; i < regOrdinals.length; i++) {
			regs[i] = Reg.values()[regOrdinals[i]];
		}

		return regs;
	}

	public void load(final TIntIntHashMap regMap) {
		resetAccess();

		regToValue.clear();
		final int[] keys = regMap.keys();
		for (int key : keys) {
			regToValue.put(key, regMap.get(key));
		}
	}
}
