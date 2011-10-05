package elw.dp.mips;

import java.util.*;

public class Instructions {
    private SortedMap<Integer, Instruction> addressToInstruction = new TreeMap<Integer, Instruction>();
    private Set<Integer> readAddresses = new HashSet<Integer>();

    private final Random random = new Random();
    private int codeBase;
    private int stackBase;
    private int minStackBase;

    public void resetAccess() {
        readAddresses.clear();
    }

    public Instruction get(int address) {
        readAddresses.add(address);

        return getInternal(address);
    }

    public boolean hasInstruction(int address) {
        return addressToInstruction.containsKey(address);
    }

    public Instruction getInternal(final int address) {
        if (addressToInstruction.containsKey(address)) {
            return addressToInstruction.get(address);
        }

        return null;
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

    public void setInstructions(final List<Instruction> instructions, HashMap<String, Integer> labelIndex) {
        resetAccess();

        codeBase = (Math.abs(random.nextInt()) % 0x1000) * 0x10;
        stackBase = 0x800000 - (Math.abs(random.nextInt()) % 0x1000) * 0x10;
        minStackBase = stackBase;

        int address = codeBase;
        addressToInstruction.clear();
        for (Instruction instruction : instructions) {
            instruction.resolve(codeBase, labelIndex);    //	update the abs. addresses, if any
            addressToInstruction.put(address, instruction);
            address += 4;
        }
    }

    public int getCodeBase() {
        return codeBase;
    }

    public int getStackBase() {
        return stackBase;
    }

    public int getMinStackBase() {
        return minStackBase;
    }

    protected void updateMinStack(int spValue) {
        minStackBase = Math.min(minStackBase, spValue);
    }

}
