package elw.dp.app;

import base.pattern.Result;
import elw.dp.mips.DataPath;
import elw.dp.mips.Instruction;
import elw.dp.mips.Reg;
import elw.dp.mips.asm.Data;
import elw.dp.mips.asm.MipsAssembler;
import elw.dp.ui.DataPathForm;
import elw.dp.ui.FeedbackAppender;
import elw.vo.*;
import gnu.trove.TIntIntHashMap;
import gnu.trove.TLongLongHashMap;
import org.akraievoy.gear.G;
import org.akraievoy.gear.G4Str;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;

public class Controller {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(Controller.class);

	protected static final String LINE_SEPARATOR = System.getProperty("line.separator");

	protected final DataPathForm view = new DataPathForm();

	protected final MipsAssembler assembler = new MipsAssembler();
	protected DataPath dataPath = new DataPath();

	//  application state
	protected Version selectedTask;
	protected final DefaultComboBoxModel testComboModel = new DefaultComboBoxModel();

	InstructionsTableModel instructionsTableModel;
	RegistersTableModel registersTableModel;
	MemoryTableModel memoryTableModel;

	//	actions
	protected final ScheduledExecutorService executor = new ScheduledThreadPoolExecutor(3);
	protected final CompileAction aCompile = new CompileAction();
	protected final UpdateTestSelectionAction aUpdateTestSelection = new UpdateTestSelectionAction();
	protected final TestStepAction aTestStep = new TestStepAction();

	public void init() throws IOException {
		final InputStream modelStream = Controller.class.getResourceAsStream("/aos-s10.json");
		final ObjectMapper mapper = new ObjectMapper();
		final Course course = mapper.readValue(modelStream, Course.class);

		selectedTask = course.getAssBundles()[0].getAssignments()[0].getVersions()[0];

		testComboModel.removeAllElements();
		for (Test test: selectedTask.getTests()) {
			testComboModel.addElement(test);
		}
		selectTest(selectedTask.getTests()[0]);

		final FeedbackAppender feedbackAppender = new FeedbackAppender(view.getLogTextPane());
		feedbackAppender.setThreshold(Level.ALL);
		Logger.getRootLogger().addAppender(feedbackAppender);

		log.info("started up, yeah!");
		view.getProblemTextPane().setText(G4Str.join(selectedTask.getStatementHtml(), "\n"));
		view.getTestComboBox().setModel(testComboModel);

		//  TODO hide this
		view.getSourceTextArea().setText(G4Str.join(selectedTask.getSolution(), "\n"));

		view.getSourceCompileButton().setAction(aCompile);
		view.getTestComboBox().setAction(aUpdateTestSelection);
		view.getTestStepButton().setAction(aTestStep);

		view.getSourceCompileButton().setMnemonic('c');
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

	public AbstractTableModel getInstructionsModel() {
		if (instructionsTableModel == null) {
			instructionsTableModel = new InstructionsTableModel(dataPath.getInstructions());
		}

		return instructionsTableModel;
	}

	public AbstractTableModel getRegistersModel() {
		if (registersTableModel == null) {
			registersTableModel = new RegistersTableModel(dataPath.getRegisters());
		}

		return registersTableModel;
	}

	public AbstractTableModel getMemoryModel() {
		if (memoryTableModel == null) {
			memoryTableModel = new MemoryTableModel(dataPath.getMemory());
		}

		return memoryTableModel;
	}

	class CompileAction extends AbstractAction {
		public CompileAction() {
			super("Compile");
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getSourceFeedbackLabel();
			setupStatus(statusLabel, "Compiling...");

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[] {new Result("status unknown", false)};
					try {
						final String source = view.getSourceTextArea().getText();
						final String[] sourceLines = source.split(LINE_SEPARATOR);

						final Instruction[] instructions = assembler.assembleLoad(sourceLines, resRef);
						if (instructions != null) {
							dataPath.getInstructions().setInstructions(Arrays.asList(instructions));
						}
					} finally {
						SwingUtilities.invokeLater(new Runnable() {
							public void run() {
								setEnabled(true);
								setupStatus(statusLabel, resRef[0]);
							}
						});
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
		public TestStepAction() {
			super("Step");
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);

			final JLabel statusLabel = view.getTestStatusLabel();
			setupStatus(statusLabel, "Initializing stepping...");

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[] {new Result("status unknown", false)};
					try {
						final String regsText = view.getTestRegsTextArea().getText();
						final String[] regsLines = regsText.split(LINE_SEPARATOR);

						final TIntIntHashMap[] regs = assembler.loadRegs(regsLines, resRef);
						if (regs != null && resRef[0].isSuccess()) {
							final String memText = view.getTestMemTextArea().getText();
							final String[] memLines = memText.split(LINE_SEPARATOR);
							final TIntIntHashMap[] data = assembler.loadData(memLines, resRef);

							if (data != null && resRef[0].isSuccess()) {
								//	TODO: setup the DataPath with all this data we have processed
							}
						}
					} finally {
						SwingUtilities.invokeLater(new Runnable() {
							public void run() {
								setEnabled(true);
								setupStatus(statusLabel, resRef[0]);
							}
						});
					}
				}
			});
		}
	}

	public void do_reset(ActionEvent e) {
		dataPath.reset();
	}

	public void do_step(ActionEvent e) {
		try {

			dataPath.execute();

		} catch (Throwable t) {
			log.warn(G.report(t));
			log.info("details", t);
		}

		getInstructionsModel().fireTableDataChanged();
		getRegistersModel().fireTableDataChanged();
		getMemoryModel().fireTableDataChanged();
	}

	public static void setupStatus(final JLabel label, final String text) {
		label.setText(text);
		label.setToolTipText(text);
		label.setForeground(Color.DARK_GRAY);
	}

	public static void setupStatus(final JLabel label, final Result result) {
		label.setToolTipText(result.getMessage());
		label.setText(result.getMessage());
		label.setForeground(result.isSuccess() ? Color.GREEN.darker().darker() : Color.RED.darker().darker());
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

		frame.getContentPane().add(instance.view.getRootPanel());
		frame.pack();
		frame.setSize(600, 400);
		frame.setLocation(20, 20);
		frame.setVisible(true);
	}
}
