package elw.dp.mips;

import base.Die;
import base.pattern.Result;
import elw.dp.mips.asm.MipsAssembler;
import gnu.trove.TIntIntHashMap;
import org.akraievoy.gear.G;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class MipsValidator {
	private static final Logger log = LoggerFactory.getLogger(MipsValidator.class);

	private final MipsAssembler assembler = new MipsAssembler();
	private final DataPath dataPath = new DataPath();

	private final HashMap<String,Integer> labelIndex = new HashMap<String, Integer>();
	private Instruction[] instructions = null;
	private TIntIntHashMap[] regs = null;
	private TIntIntHashMap[] data = null;
	//	static setup, may be spring-injected at some time
	private int runSteps = 16384;

	public DataPath getDataPath() {
		return dataPath;
	}

	public int getRunSteps() {
		return runSteps;
	}

	public void setRunSteps(int runSteps) {
		this.runSteps = runSteps;
	}

	public Instruction[] assemble(Result[] resRef, String[] sourceLines) {
		final Instruction[] newInstructions = assembler.loadInstructions(sourceLines, resRef, labelIndex);

		if (resRef[0].isSuccess()) {
			Die.ifNull("newInstructions", newInstructions);
			instructions = newInstructions;
			return newInstructions;
		} else {
			return instructions = null;
		}
	}

	public void loadTest(Result[] resRef, String test) {
		final String[] regs = new String[1];
		final String[] mem = new String[1];
		if (!TaskBean.parseTest(test, regs, mem)) {
			Result.failure(log, resRef, "test marks broken");
			return;
		}

		final String[] regsLines = regs[0].split("\r|\n");

		final TIntIntHashMap[] newRegs = assembler.loadRegs(regsLines, resRef);
		if (newRegs != null && resRef[0].isSuccess()) {
			final String[] memLines = mem[0].split("\r|\n");
			final TIntIntHashMap[] newData = assembler.loadData(memLines, resRef);

			if (newData != null && resRef[0].isSuccess()) {
				this.regs = newRegs;
				this.data = newData;
			} else {
				this.regs = this.data = null;
			}
		} else {
			this.regs = this.data = null;
		}
	}

	public void reset(Result[] resRef) {
		if (instructions != null && data != null && regs != null) {
			dataPath.getInstructions().setInstructions(Arrays.asList(instructions), labelIndex);
			dataPath.getMemory().setData(data[0]);
			dataPath.getRegisters().load(regs[0]);
			dataPath.getRegisters().setReg(Reg.pc, dataPath.getInstructions().getCodeBase());
			dataPath.getRegisters().setReg(Reg.ra, dataPath.getInstructions().getCodeBase() - 4);
			dataPath.getRegisters().setReg(Reg.sp, dataPath.getInstructions().getStackBase());
			regs[1].put(Reg.ra.ordinal(), dataPath.getInstructions().getCodeBase() - 4);
			regs[1].put(Reg.sp.ordinal(), dataPath.getInstructions().getStackBase());
			Result.success(log, resRef, "Instructions, Data and Regs loaded");
		} else {
			Result.failure(log, resRef, "Instructions, Data or Regs NOT loaded!");
		}
	}

	public boolean step(Result[] resRef, int steps) {
		for (int step = 0; step < steps; step++) {
			final Instruction instruction = dataPath.execute();
			if (instruction != null) {
				if (step == 0) {
					Result.success(log, resRef, "Executed " + instruction.getOpName());
				}
			} else {
				verifyRegs(resRef);
				if (resRef[0].isSuccess()) {
					verifyMem(resRef);
				}
				if (resRef[0].isSuccess()) {
					Result.success(log, resRef, "Test Passed");
				}
				return true;
			}
		}

		return false;
	}

	private void verifyMem(Result[] resRef) {
		final Memory memory = dataPath.getMemory();
		final TIntIntHashMap expectedMemMap = data[1];
		final int[] expectedAddrs = expectedMemMap.keys();

		for (int expectedMem : expectedAddrs) {
			final String expectedMemHex = Integer.toString(expectedMem, 16);
			if (!memory.hasWord(expectedMem)) {
				Result.failure(log, resRef, "Test Failed: expecting data at 0x" + expectedMemHex + ", but word never set");
				return;
			}

			final int value = memory.getWordInternal(expectedMem);
			final int expectedValue = expectedMemMap.get(expectedMem);
			if (expectedValue != value) {
				Result.failure(log, resRef, "Test Failed: expecting " + expectedValue + " at 0x" + expectedMemHex + ", but found " + value);
				return;
			}
		}

		final Instructions instructions = dataPath.getInstructions();
		final int memSetBytes = memory.getSize();
		for (int byteIndex = 0; byteIndex < memSetBytes; byteIndex++) {
			int byteAddr = memory.getAddressAt(byteIndex);
			if (instructions.getStackBase() > byteAddr && instructions.getMinStackBase() <= byteAddr) {
				continue;
			}
			final int byteAddrAligned = byteAddr - byteAddr % 4;
			if (expectedMemMap.contains(byteAddrAligned)) {
				continue;
			}
			final String byteAddrHex = Integer.toString(byteAddr, 16);
			Result.failure(log, resRef, "Test Failed: expecting clean byte at 0x" + byteAddrHex + ", but memory corrupted");
			return;
		}

		Result.success(log, resRef, "Test Passed Memory Spec");
	}

	private void verifyRegs(Result[] resRef) {
		final Reg[] setupRegs = dataPath.getRegisters().getSetupRegs();
		final TIntIntHashMap expectedRegMap = regs[1];
		final Reg[] expectedRegs = Reg.values(expectedRegMap.keys());
		for (Reg expectedReg : expectedRegs) {
			if (!G.contains(setupRegs, expectedReg)) {
				Result.failure(log, resRef, "Test Failed: expecting $" + expectedReg.toString() + ", but register never set");
				return;
			}

			final int value = dataPath.getRegisters().getRegInternal(expectedReg);
			final int expectedValue = expectedRegMap.get(expectedReg.ordinal());
			if (expectedValue != value) {
				Result.failure(log, resRef, "Test Failed: expecting $" + expectedReg.toString() + "=" + expectedValue + ", but $" + expectedReg.toString() + "=" + value);
				return;
			}
		}

		for (Reg setupReg : setupRegs) {
			if (G.contains(Reg.tempRegs, setupReg)) {
				continue;
			}
			if (G.contains(expectedRegs, setupReg)) {
				continue;
			}

			Result.failure(log, resRef, "Test Failed: expecting clean $" + setupReg.toString() + ", but register corrupted");
			return;
		}

		Result.success(log, resRef, "Test Passed Register Spec");
	}

	public void run(Result[] resRef, final String test, final String[] code) {
		assemble(resRef, code);
		if (resRef[0].isSuccess()) {
			loadTest(resRef, test);
		}
		if (resRef[0].isSuccess()) {
			reset(resRef);
		}
		if (resRef[0].isSuccess()) {
			if (!step(resRef, runSteps)) {
				Result.failure(log, resRef, "Execution timed out");
			}
		}
	}

	public void batch(Result[] resRef, final TaskBean task, final String[] code, final int[] passFailCounts) {
		int failCount = 0;
		List<String> tests = task.getTests();
		for (int i = 0, testsLength = tests.size(); i < testsLength; i++) {
			String test = tests.get(i);
			final Result[] localResRef = {new Result("test status unknown", false)};
			try {
				run(localResRef, test, code);
				if (!localResRef[0].isSuccess()) {
					failCount++;
					if (passFailCounts != null) {
						passFailCounts[1]++;
					}
				} else {
					if (passFailCounts != null) {
						passFailCounts[0]++;
					}
				}
			} catch (Throwable t) {
				failCount++;
				Result.failure(log, resRef, "Failed: " + G.report(t));
				log.trace("trace", t);
			}
		}

		if (failCount > 0) {
			Result.failure(log, resRef, failCount + " of " + tests.size() + " tests failed");
		} else {
			Result.success(log, resRef, tests.size() + " tests passed");
		}
	}

	public void clearTest() {
		data = regs = null;
	}
}
