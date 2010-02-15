package elw.dp.app;

import base.pattern.Result;
import elw.dp.mips.*;
import elw.dp.mips.asm.MipsAssembler;
import elw.dp.ui.DataPathForm;
import elw.dp.ui.FeedbackAppender;
import elw.vo.Course;
import elw.vo.Test;
import elw.vo.Version;
import gnu.trove.TIntIntHashMap;
import org.akraievoy.gear.G;
import org.akraievoy.gear.G4Str;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicLong;

public class Controller {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(Controller.class);

	protected static final String LINE_SEPARATOR = System.getProperty("line.separator");

	protected final DataPathForm view = new DataPathForm();

	protected final MipsAssembler assembler = new MipsAssembler();
	protected DataPath dataPath = new DataPath();

	//  application state
	protected AtomicLong sourceStamp = new AtomicLong(1); // NOTE: no modifications still require assembly to run before stepping
	protected AtomicLong assembleStamp = new AtomicLong(0);
	protected Version selectedTask;
	protected final DefaultComboBoxModel testComboModel = new DefaultComboBoxModel();
	//	app data (compile/test/run cycle)
	protected Instruction[] instructions = null;
	protected TIntIntHashMap[] regs = null;
	protected TIntIntHashMap[] data = null;

	InstructionsTableModel tmInstructions = new InstructionsTableModel(dataPath.getInstructions());
	RegistersTableModel tmRegs = new RegistersTableModel(dataPath.getRegisters());
	MemoryTableModel tmMemory = new MemoryTableModel(dataPath.getMemory());

	//	actions
	protected final ScheduledExecutorService executor = new ScheduledThreadPoolExecutor(3);
	protected final AssembleAction aAssemble = new AssembleAction("Assemble>", true);
	protected final AssembleAction aVerify = new AssembleAction("Verify", false);
	protected final UpdateTestSelectionAction aUpdateTestSelection = new UpdateTestSelectionAction();
	protected final TestStepAction aTestStep = new TestStepAction("Step>");
	protected final TestRunAction aTestRun = new TestRunAction("Run", 16384);
	protected final TestBatchAction aTestBatch = new TestBatchAction("Batch", 16384);
	protected final RunStepAction aRunStep = new RunStepAction("Step", 1);
	protected final RunStepAction aRunRun = new RunStepAction("Run", 4096);
	protected final RunResetAction aRunReset = new RunResetAction("Reset");

	public void init() throws IOException {
		final InputStream modelStream = Controller.class.getResourceAsStream("/aos-s10.json");
		final ObjectMapper mapper = new ObjectMapper();
		final Course course = mapper.readValue(modelStream, Course.class);

		selectedTask = course.getAssBundles()[0].getAssignments()[0].getVersions()[0];


		testComboModel.removeAllElements();
		int testId = 0;
		for (Test test : selectedTask.getTests()) {
			testId ++;
			test.setId(String.valueOf(testId));
			test.setId("#" + String.valueOf(testId));
			
			testComboModel.addElement(test);
		}
		selectTest(selectedTask.getTests()[0]);

		final FeedbackAppender feedbackAppender = new FeedbackAppender(view.getLogTextPane());
		feedbackAppender.setThreshold(Level.ALL);
		Logger.getRootLogger().addAppender(feedbackAppender);

		view.getProblemTextPane().setText(G4Str.join(selectedTask.getStatementHtml(), "\n"));
		view.getTestComboBox().setModel(testComboModel);

		//  TODO hide this
		view.getSourceTextArea().setText(G4Str.join(selectedTask.getSolution(), "\n"));
		view.getSourceTextArea().getDocument().addDocumentListener(new SourceDocumentListener());

		view.getSourceAssembleButton().setAction(aAssemble);
		view.getSourceAssembleButton().setMnemonic('a');
		view.getSourceVerifyButton().setAction(aVerify);
		view.getSourceVerifyButton().setMnemonic('v');

		view.getTestComboBox().setAction(aUpdateTestSelection);
		view.getTestAddCustomButton().setEnabled(false); //	LATER #163
		view.getTestStepButton().setAction(aTestStep);
		view.getTestStepButton().setMnemonic('s');
		view.getTestRunButton().setAction(aTestRun);
		view.getTestRunButton().setMnemonic('r');
		view.getTestBatchButton().setAction(aTestBatch);
		view.getTestBatchButton().setMnemonic('b');

		view.getRunInstructionsTable().setModel(tmInstructions);
		view.getRunRegsTable().setModel(tmRegs);
		view.getRunMemTable().setModel(tmMemory);
		view.getRunStepButton().setAction(aRunStep);
		view.getRunStepButton().setMnemonic('t');
		view.getRunRunButton().setAction(aRunRun);
		view.getRunRunButton().setMnemonic('r');
		view.getRunResetButton().setAction(aRunReset);
		view.getRunResetButton().setMnemonic('e');

		log.info("started up, yeah!");
	}

	protected void fireDataPathChanged() {
		tmInstructions.fireTableDataChanged();
		tmMemory.fireTableDataChanged();
		tmRegs.fireTableDataChanged();
	}

	protected void selectTest(Test test) {
		testComboModel.setSelectedItem(test);
		final String[] memText = test.getArgs().get("mem") != null ? test.getArgs().get("mem") : G.STRINGS_EMPTY;
		view.getTestMemTextArea().setText(G4Str.join(memText, LINE_SEPARATOR));
		final String[] regsText = test.getArgs().get("regs") != null ? test.getArgs().get("regs") : G.STRINGS_EMPTY;
		view.getTestRegsTextArea().setText(G4Str.join(regsText, LINE_SEPARATOR));

		view.getTestMemTextArea().setEditable(!test.isShared());
		view.getTestRegsTextArea().setEditable(!test.isShared());
	}

	protected void job_assemble(final JLabel statusLabel, final Result[] resRef) {
		setupStatus(statusLabel, "Assembling...");

		final String source = view.getSourceTextArea().getText();
		final String[] sourceLines = source.split(LINE_SEPARATOR);

		final Instruction[] newInstructions = assembler.assembleLoad(sourceLines, resRef);
		if (newInstructions != null) {
			assembleStamp.set(System.currentTimeMillis());
			instructions = newInstructions;
		} else {
			instructions = null;
		}
	}

	protected void job_loadTest(final JLabel statusLabel, Result[] resRef, final Test test) {
		final boolean assemble = assembleStamp.get() < sourceStamp.get();
		if (assemble) {
			job_assemble(statusLabel, resRef);
		}

		if (!assemble || resRef[0].isSuccess()) {
			setupStatus(statusLabel, "Loading Regs and Mem for " + test.getName() + "...");
//	LATER #163
/*
			final String regsText = view.getTestRegsTextArea().getText();
			final String[] regsLines = regsText.split(LINE_SEPARATOR);
*/
			final String[] regsLines = test.getArgs().get("regs") != null ? test.getArgs().get("regs") : G.STRINGS_EMPTY;

			final TIntIntHashMap[] newRegs = assembler.loadRegs(regsLines, resRef);
			if (newRegs != null && resRef[0].isSuccess()) {
//	LATER #163
/*
				final String memText = view.getTestMemTextArea().getText();
				final String[] memLines = memText.split(LINE_SEPARATOR);
*/
				final String[] memLines = test.getArgs().get("mem") != null ? test.getArgs().get("mem") : G.STRINGS_EMPTY;
				final TIntIntHashMap[] newData = assembler.loadData(memLines, resRef);

				if (newData != null && resRef[0].isSuccess()) {
					regs = newRegs;
					data = newData;
				} else {
					regs = data = null;
				}
			} else {
				regs = data = null;
			}
		} else {
			regs = data = null;
		}
	}

	protected void job_reset(JLabel statusLabel, Result[] resRef) {
		setupStatus(statusLabel, "Resetting...");

		if (instructions != null && data != null && regs != null) {
			dataPath.getInstructions().setInstructions(Arrays.asList(instructions));
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

	public boolean job_step(JLabel statusLabel, Result[] resRef, final int steps) {
		setupStatus(statusLabel, "Stepping...");

		for (int step = 0; step < steps; step++) {
			final Instruction instruction = dataPath.execute();
			if (instruction != null) {
				Result.success(log, resRef, "Executed " + instruction.getOpName());
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

	public boolean job_run(JLabel statusLabel, Result[] resRef, final Test test, final int steps) {
		setupStatus(statusLabel, "Running...");
		job_loadTest(statusLabel, resRef, test);
		if (resRef[0].isSuccess()) {
			job_reset(statusLabel, resRef);
		}
		if (resRef[0].isSuccess()) {
			if (!job_step(statusLabel, resRef, steps)) {
				Result.failure(log, resRef, "Execution timed out");
			} else {
				return true;
			}
		}
		return false;
	}

	public void job_batch(JLabel statusLabel, Result[] resRef, final int steps) {
		setupStatus(statusLabel, "Running...");

		int failCount = 0;
		for (Test test : selectedTask.getTests()) {
			try {
				if (!job_run(statusLabel, resRef, test, steps)) {
					failCount++;
				}
			} catch (Throwable t) {
				failCount++;
				Result.failure(log, resRef, "Failed: " + G.report(t));
				log.trace("trace", t);
			}
		}

		if (failCount > 0) {
			Result.failure(log, resRef, failCount + " of " + selectedTask.getTests().length + " tests failed");
		} else {
			Result.success(log, resRef, selectedTask.getTests().length + " tests passed");
		}
	}

	protected void verifyMem(Result[] resRef) {
		final Memory memory = dataPath.getMemory();
		final TIntIntHashMap expectedMemMap = data[1];
		final int[] expectedAddrs = expectedMemMap.keys();

		for (int expectedMem : expectedAddrs) {
			if (!memory.hasWord(expectedMem)) {
				Result.failure(log, resRef, "Test Failed: expecting data at " + expectedMem + ", but word never set");
				return;
			}

			final int value = memory.getWordInternal(expectedMem);
			final int expectedValue = expectedMemMap.get(expectedMem);
			if (expectedValue != value) {
				Result.failure(log, resRef, "Test Failed: expecting " + expectedValue + " at " + expectedMem + ", but found " + value);
				return;
			}
		}

		final Instructions instructions = dataPath.getInstructions();
		final int memSetBytes = memory.getSize();
		for (int byteIndex = 0; byteIndex < memSetBytes; byteIndex++) {
			int byteAddr = memory.getAddressAt(byteIndex);
			if (instructions.getStackBase() + 4 > byteAddr && dataPath.getInstructions().getMinStackBase() <= byteAddr) {
				continue;
			}
			final int byteAddrAligned = byteAddr - byteAddr % 4;
			if (expectedMemMap.contains(byteAddrAligned)) {
				continue;
			}
			Result.failure(log, resRef, "Test Failed: expecting clean byte at " + byteAddr + ", but memory corrupted");
			return;
		}

		Result.success(log, resRef, "Test Passed Memory Spec");
	}

	protected void verifyRegs(Result[] resRef) {
		final Reg[] setupRegs = dataPath.getRegisters().getSetupRegs();
		final TIntIntHashMap expectedRegMap = regs[1];
		final Reg[] expectedRegs = Reg.values(expectedRegMap.keys());
		for (Reg expectedReg : expectedRegs) {
			if (!G.contains(setupRegs, expectedReg)) {
				Result.failure(log, resRef, "Test Failed: expecting $" + expectedReg.toString() + ", but register never set");
				return;
			}

			final int value = dataPath.getRegisters().getReg(expectedReg);
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

	class AssembleAction extends AbstractAction {
		final boolean switchToTest;

		public AssembleAction(final String name, boolean switchToTest) {
			super(name);
			this.switchToTest = switchToTest;
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getSourceFeedbackLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};
					try {
						job_assemble(statusLabel, resRef);
					} finally {
						setEnabled(true);
						setupStatus(statusLabel, resRef[0]);
						if (switchToTest && resRef[0].isSuccess()) {
							view.getStrTabbedPane().setSelectedIndex(1);
						}
					}
				}
			});
		}
	}

	class UpdateTestSelectionAction extends AbstractAction {
		public UpdateTestSelectionAction() {
			super("Update Test Selection");
		}

		public void actionPerformed(ActionEvent e) {
			selectTest((Test) testComboModel.getSelectedItem());
		}
	}

	class TestStepAction extends AbstractAction {
		public TestStepAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getTestStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						job_loadTest(statusLabel, resRef, (Test) testComboModel.getSelectedItem());

						if (resRef[0].isSuccess()) {
							job_reset(statusLabel, resRef);
						}
					} finally {
						setEnabled(true);
						setupStatus(statusLabel, resRef[0]);
						if (resRef[0].isSuccess()) {
							view.getStrTabbedPane().setSelectedIndex(2);
							fireDataPathChanged();
						}
					}
				}
			});
		}
	}

	class RunStepAction extends AbstractAction {
		protected final int steps;

		public RunStepAction(final String name, final int steps) {
			super(name);
			this.steps = steps;
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getRunStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						job_step(statusLabel, resRef, steps);
					} catch (Throwable t) {
						Result.failure(log, resRef, "Failed: " + G.report(t));
						log.trace("trace", t);
					} finally {
						setEnabled(true);
						setupStatus(statusLabel, resRef[0]);
						if (resRef[0].isSuccess()) {
							fireDataPathChanged();
						}
					}
				}
			});
		}
	}

	class RunResetAction extends AbstractAction {
		public RunResetAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getRunStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						job_reset(statusLabel, resRef);
					} finally {
						setEnabled(true);
						setupStatus(statusLabel, resRef[0]);
						if (resRef[0].isSuccess()) {
							fireDataPathChanged();
						}
					}
				}
			});
		}
	}

	class TestRunAction extends AbstractAction {
		protected final int steps;

		public TestRunAction(final String name, final int steps) {
			super(name);
			this.steps = steps;
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getTestStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						job_run(statusLabel, resRef, (Test) Controller.this.testComboModel.getSelectedItem(), steps);
					} catch (Throwable t) {
						Result.failure(log, resRef, "Failed: " + G.report(t));
						log.trace("trace", t);
					} finally {
						setEnabled(true);
						setupStatus(statusLabel, resRef[0]);
					}
				}
			});
		}
	}

	class TestBatchAction extends AbstractAction {
		protected final int steps;

		public TestBatchAction(final String name, final int steps) {
			super(name);
			this.steps = steps;
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getTestStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						job_batch(statusLabel, resRef, steps);
					} finally {
						setEnabled(true);
						setupStatus(statusLabel, resRef[0]);
					}
				}
			});
		}
	}

	protected class SourceDocumentListener implements DocumentListener {
		public void insertUpdate(DocumentEvent e) {
			sourceStamp.set(System.currentTimeMillis());
		}

		public void removeUpdate(DocumentEvent e) {
			sourceStamp.set(System.currentTimeMillis());
		}

		public void changedUpdate(DocumentEvent e) {
			sourceStamp.set(System.currentTimeMillis());
		}
	}

	public static void setupStatus(final JLabel label, final String text) {
		if (SwingUtilities.isEventDispatchThread()) {
			label.setText(text);
			label.setToolTipText(text);
			label.setForeground(Color.DARK_GRAY);
		} else {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					label.setText(text);
					label.setToolTipText(text);
					label.setForeground(Color.DARK_GRAY);
				}
			});
		}
	}

	public static void setupStatus(final JLabel label, final Result result) {
		if (SwingUtilities.isEventDispatchThread()) {
			label.setToolTipText(result.getMessage());
			label.setText(result.getMessage());
			label.setForeground(result.isSuccess() ? Color.GREEN.darker().darker() : Color.RED.darker().darker());
		} else {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					label.setToolTipText(result.getMessage());
					label.setText(result.getMessage());
					label.setForeground(result.isSuccess() ? Color.GREEN.darker().darker() : Color.RED.darker().darker());
				}
			});
		}
	}

	public static void main(String[] args) throws IOException {
		BasicConfigurator.configure();

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			System.out.println("Error setting native LAF: " + e);
		}

		final Controller instance = new Controller();
		instance.init();

		final JFrame frame = new JFrame();
		frame.setTitle("DataPath");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				frame.getContentPane().add(instance.view.getRootPanel());
				frame.pack();
				frame.setSize(600, 400);
				frame.setLocation(20, 20);
				frame.setVisible(true);
			}
		});
	}
}
