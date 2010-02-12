package elw.dp.mips;

import java.util.*;

public class Instructions {
    protected SortedMap<Integer, Instruction> addressToInstruction = new TreeMap<Integer, Instruction>();
    protected Set<Integer> readAddresses = new HashSet<Integer>();

    protected int base;

    public void resetAccess() {
        readAddresses.clear();
    }

    public Instruction get(int address) {
        readAddresses.add(address);

        return getInternal(address);
    }

    public Instruction getInternal(final int address) {
        if (addressToInstruction.containsKey(address)) {
            return addressToInstruction.get(address);
        }

        return Instruction.NOOP;
    }

    public int getSize() {
        return addressToInstruction.size();    
    }

    public int getAddressAt(int index) {
        //  LATER this is memory drain
        final List<Integer> addrs = new ArrayList<Integer>(addressToInstruction.keySet());

        Collections.sort(addrs);

        return addrs.get(index);
    }

    public Set<Integer> getReadAddresses() {
        return Collections.unmodifiableSet(readAddresses);
    }

    public void setInstructions(final List<Instruction> instructions) {
        resetAccess();

        base = (Math.abs(new Random().nextInt()) % 0x1000) * 0x10;
        int address= base;

        addressToInstruction.clear();

        for (Instruction instruction : instructions) {
            addressToInstruction.put(address, instruction);
            address+=4;
        }
    }

    public int getBase() {
        return base;
    }
}
