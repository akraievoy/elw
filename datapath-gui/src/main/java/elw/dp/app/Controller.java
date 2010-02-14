package elw.dp.app;

import base.pattern.Result;
import elw.dp.mips.DataPath;
import elw.dp.mips.Instruction;
import elw.dp.mips.Reg;
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

	protected void job_loadTest(final JLabel statusLabel, Result[] resRef) {
		final boolean assemble = assembleStamp.get() < sourceStamp.get();
		if (assemble) {
			job_assemble(statusLabel, resRef);
		}

		if (!assemble || resRef[0].isSuccess()) {
			setupStatus(statusLabel, "Loading Regs and Mem...");

			final String regsText = view.getTestRegsTextArea().getText();
			final String[] regsLines = regsText.split(LINE_SEPARATOR);

			final TIntIntHashMap[] newRegs = assembler.loadRegs(regsLines, resRef);
			if (newRegs != null && resRef[0].isSuccess()) {
				final String memText = view.getTestMemTextArea().getText();
				final String[] memLines = memText.split(LINE_SEPARATOR);
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
			dataPath.getRegisters().setReg(Reg.pc, dataPath.getInstructions().getBase());
			dataPath.getRegisters().setReg(Reg.ra, dataPath.getInstructions().getBase() - 4);
			Result.success(log, resRef, "Instructions, Data and Regs loaded");
		} else {
			Result.failure(log, resRef, "Instructions, Data or Regs NOT loaded!");
		}
	}

	public void job_step(JLabel statusLabel, Result[] resRef, final int steps) {
		setupStatus(statusLabel, "Stepping...");
		try {
			for (int step = 0; step < steps; step++) {
				final Instruction instruction = dataPath.execute();
				if (instruction != null) {
					Result.success(log, resRef, "Executed " + instruction.getOpName());
				} else {
					Result.success(log, resRef, "Complete");
					//	TODO proceed with result verification
					break;
				}
			}
		} catch (Throwable t) {
			Result.failure(log, resRef, "Failed: " + G.report(t));
			log.trace("trace", t);
		}
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
						job_loadTest(statusLabel, resRef);

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
