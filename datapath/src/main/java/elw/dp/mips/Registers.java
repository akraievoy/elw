package elw.dp.mips;

import java.util.*;

public class Registers {
    final Map <Reg, Integer> regToValue= new HashMap<Reg, Integer>();

    final Set <Reg> readRegs= new HashSet<Reg>();
    final Set <Reg> writeRegs= new HashSet<Reg>();

    public int getReg(Reg reg) {
        readRegs.add(reg);

        return getRegInternal(reg);
    }

    public int getRegInternal(final Reg reg) {
        final Integer integer = regToValue.get(reg);

        return integer == null ? 0 : integer;
    }

    public int setReg(Reg reg, int value) {
        writeRegs.add(reg);

        final Integer oldValue = regToValue.put(reg, value);
        return oldValue != null ? oldValue : 0;
    }

    public void reset() {
        resetAccess();

        regToValue.clear();
    }

    public void resetAccess() {
        readRegs.clear();
        writeRegs.clear();
    }

    public Set<Reg> getReadRegs() {
        return Collections.unmodifiableSet(readRegs);
    }

    public Set<Reg> getWriteRegs() {
        return Collections.unmodifiableSet(writeRegs);
    }

    public void load(final int[] lastLoaded) {
        resetAccess();

        for (Reg reg : Reg.values()) {
            regToValue.put(reg, lastLoaded[reg.ordinal()]);
        }
    }
}
