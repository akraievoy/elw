package elw.dp.mips;

import gnu.trove.TIntArrayList;
import gnu.trove.TIntIntHashMap;
import org.akraievoy.gear.G;

import java.util.Arrays;

public class Registers {
    private final TIntIntHashMap regToValue = new TIntIntHashMap();

    private final TIntArrayList readRegs = new TIntArrayList();
    private final TIntArrayList writeRegs = new TIntArrayList();

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

        return setRegInternal(reg, value);
    }

    private int setRegInternal(Reg reg, int value) {
        if (G.contains(Reg.roRegs, reg)) {
            //	LATER log
            return 0;
        }

        return regToValue.put(reg.ordinal(), value);
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

        return Reg.values(regOrdinals);
    }

    public void load(final TIntIntHashMap regMap) {
        resetAccess();

        regToValue.clear();
        final int[] keys = regMap.keys();
        for (int key : keys) {
            regToValue.put(key, regMap.get(key));
        }
    }

    public void unset(int[] regs) {
        for (int reg : regs) {
            regToValue.remove(reg);
        }
    }
}
