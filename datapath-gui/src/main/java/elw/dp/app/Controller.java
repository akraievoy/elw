package elw.dp.app;

import base.pattern.Result;
import elw.dp.mips.*;
import elw.dp.ui.DataPathForm;
import elw.dp.ui.FeedbackAppender;
import elw.dp.ui.RendererFactory;
import org.akraievoy.gear.G;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.*;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

public class Controller implements ControllerSetup, CallbackLoadTask {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(Controller.class);

	private static final String PREFIX_TEST = "Test #";
	private static final Pattern PATTERN_LINE_SEPARATOR = Pattern.compile("\r|\r\n|\n");

	private final DataPathForm view = new DataPathForm();
	private final JLabel labelStatus = new JLabel();

	//  application state
	private AtomicLong sourceStamp = new AtomicLong(1); // NOTE: no modifications still require assembly to run before stepping
	private AtomicLong assembleStamp = new AtomicLong(0);

	private TaskBean task;

	private String baseUrl;
	private String uploadHeader;
	private String elwCtx;

	private final DefaultComboBoxModel testComboModel = new DefaultComboBoxModel();
	//	app data (compile/test/run cycle)

	private final MipsValidator validator = new MipsValidator();

	private InstructionsTableModel tmInstructions = new InstructionsTableModel(validator.getDataPath().getInstructions());
	private RegistersTableModel tmRegs = new RegistersTableModel(validator.getDataPath().getRegisters());
	private MemoryTableModel tmMemory = new MemoryTableModel(validator.getDataPath().getMemory());

	//	actions
	private final ScheduledExecutorService executor = new ScheduledThreadPoolExecutor(3);
	private final AssembleAction aAssemble = new AssembleAction("Assemble>", true);
	private final AssembleAction aVerify = new AssembleAction("Verify", false);
	private final SubmitAction aSubmit = new SubmitAction("Submit");
	private final UpdateTestSelectionAction aUpdateTestSelection = new UpdateTestSelectionAction();
	private final TestStepAction aTestStep = new TestStepAction("Step>");
	private final TestRunAction aTestRun = new TestRunAction("Run");
	private final TestBatchAction aTestBatch = new TestBatchAction("Batch");
	private final RunStepAction aRunStep = new RunStepAction("Step", 1);
	private final RunStepAction aRunRun = new RunStepAction("Run", validator.getRunSteps());
	private final RunResetAction aRunReset = new RunResetAction("Reset");

	public void setBaseUrl(String baseUrl) {
		this.baseUrl = baseUrl;
	}

	public void setUploadHeader(String uploadHeader) {
		this.uploadHeader = uploadHeader;
	}

	public void setElwCtx(String elwCtx) {
		this.elwCtx = elwCtx;
	}

	public String getElwCtx() {
		return elwCtx;
	}

	public String getBaseUrl() {
		return baseUrl;
	}

	public String getUploadHeader() {
		return uploadHeader;
	}

	public JPanel getPanelView() {
		return view.getRootPanel();
	}

	public JLabel getLabelStatus() {
		return labelStatus;
	}

	public void setTask(TaskBean task) {
		this.task = task;
	}

	public void start() {
		view.getTestComboBox().setEnabled(false);

		final FeedbackAppender feedbackAppender = new FeedbackAppender(view.getLogTextPane());
		feedbackAppender.setThreshold(Level.ALL);
		Logger.getRootLogger().addAppender(feedbackAppender);

		view.getProblemTextPane().setText("loading...");
		view.getSourceTextArea().setText("loading...");

		view.getSourceAssembleButton().setEnabled(false);
		view.getSourceVerifyButton().setEnabled(false);
		view.getSourceSubmitButton().setEnabled(false);

		view.getTestAddCustomButton().setEnabled(false);
		view.getTestStepButton().setEnabled(false);
		view.getTestRunButton().setEnabled(false);
		view.getTestBatchButton().setEnabled(false);

		view.getRunStepButton().setEnabled(false);
		view.getRunRunButton().setEnabled(false);
		view.getRunResetButton().setEnabled(false);

		executor.submit(new RunnableLoadTask(this, this));
	}

	public void updateStatus(final String newStatus, Throwable fault) {
		if (fault == null) {
			log.info(newStatus);
		} else {
			log.error(newStatus, fault);
		}
	}

	public void onTaskLoadComplete() {
		testComboModel.removeAllElements();
		java.util.List<String> tests = task.getTests();
		for (int i = 0, testsSize = tests.size(); i < testsSize; i++) {
			testComboModel.addElement(PREFIX_TEST + (i + 1));
		}
		selectTest(0);

		view.getProblemTextPane().setText(task.getStatement());
		view.getTestComboBox().setModel(testComboModel);

		//  TODO hide this
		view.getSourceTextArea().setText(task.getSolution());
		view.getSourceTextArea().getDocument().addDocumentListener(new SourceDocumentListener());

		view.getSourceAssembleButton().setAction(aAssemble);
		view.getSourceAssembleButton().setMnemonic('a');
		view.getSourceVerifyButton().setAction(aVerify);
		view.getSourceVerifyButton().setMnemonic('v');
		view.getSourceSubmitButton().setAction(aSubmit);
		view.getSourceSubmitButton().setMnemonic('u');

		view.getTestComboBox().setAction(aUpdateTestSelection);
		view.getTestAddCustomButton().setEnabled(false); //	LATER #163
		view.getTestStepButton().setAction(aTestStep);
		view.getTestStepButton().setMnemonic('s');
		view.getTestRunButton().setAction(aTestRun);
		view.getTestRunButton().setMnemonic('r');
		view.getTestBatchButton().setAction(aTestBatch);
		view.getTestBatchButton().setMnemonic('b');

		final RendererFactory rFactory = new RendererFactory();

		final JTable instrTable = view.getRunInstructionsTable();
		instrTable.setModel(tmInstructions);
		rFactory.install(instrTable);
		rFactory.findColByName(instrTable, InstructionsTableModel.COL_ACC).setMaxWidth(18);
		rFactory.findColByName(instrTable, InstructionsTableModel.COL_ADDR).setMaxWidth(80);
		rFactory.findColByName(instrTable, InstructionsTableModel.COL_CODE).setMaxWidth(240);
		rFactory.findColByName(instrTable, InstructionsTableModel.COL_CODE).setMinWidth(160);

		final JTable regsTable = view.getRunRegsTable();
		regsTable.setModel(tmRegs);
		rFactory.install(regsTable);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_ACC).setMaxWidth(18);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_NAME).setMinWidth(32);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_NAME).setMaxWidth(40);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_NUMBER).setMaxWidth(72);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_NUMBER).setMinWidth(64);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_HEX).setMinWidth(72);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_HEX).setMaxWidth(96);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_DEC).setMinWidth(72);
		rFactory.findColByName(regsTable, RegistersTableModel.COL_DEC).setMaxWidth(96);

		final JTable memTable = view.getRunMemTable();
		memTable.setModel(tmMemory);
		rFactory.install(memTable);
		rFactory.findColByName(memTable, MemoryTableModel.COL_ACC).setMaxWidth(18);
		rFactory.findColByName(memTable, MemoryTableModel.COL_ADDR).setMinWidth(72);
		rFactory.findColByName(memTable, MemoryTableModel.COL_ADDR).setMaxWidth(80);

		view.getRunStepButton().setAction(aRunStep);
		view.getRunStepButton().setMnemonic('t');
		view.getRunRunButton().setAction(aRunRun);
		view.getRunRunButton().setMnemonic('r');
		view.getRunResetButton().setAction(aRunReset);
		view.getRunResetButton().setMnemonic('e');

		log.info("startup sequence complete");
	}

	private void fireDataPathChanged() {
		tmInstructions.fireTableDataChanged();
		tmMemory.fireTableDataChanged();
		tmRegs.fireTableDataChanged();
	}

	private void selectTest(int i) {
		testComboModel.setSelectedItem(testComboModel.getElementAt(i));

		final String[] regsText = new String[1];
		final String[] memText = new String[1];
		if (!TaskBean.parseTest(task.getTests().get(i), regsText, memText)) {
			log.warn("marks broken, not loading test");
			return;
		}

		view.getTestRegsTextArea().setText(regsText[0]);
		view.getTestMemTextArea().setText(memText[0]);

		view.getTestMemTextArea().setEditable(false);
		view.getTestRegsTextArea().setEditable(false);

		setupStatus(view.getTestStatusLabel());
		setupStatus(view.getRunStatusLabel());
	}

	private void job_assemble(final JLabel statusLabel, final Result[] resRef) {
		setupStatus(statusLabel, "Assembling...");

		final String[] sourceLines = getSource();
		final Instruction[] newInstructions = validator.assemble(resRef, sourceLines);
		if (newInstructions != null) {
			assembleStamp.set(System.currentTimeMillis());
		}
	}

	private String[] getSource() {
		final String source = view.getSourceTextArea().getText();
		
		return PATTERN_LINE_SEPARATOR.split(source);
	}

	private void job_submit(final JLabel statusLabel, final Result[] resRef, String sourceText) {
		if (baseUrl == null || baseUrl.length() == 0) {
			Result.failure(log, resRef, "Upload URL not set");
			return;
		}
		if (elwCtx == null || elwCtx.length() == 0) {
			Result.failure(log, resRef, "elw_ctx not set");
			return;
		}

		setupStatus(statusLabel, "Submitting...");

		OutputStream up = null;
		InputStream down = null;
		try {
			final byte[] textBytes = sourceText.getBytes("UTF-8");

			final String ulStr = baseUrl + "ul?elw_ctx=" + elwCtx+"&sId=code&s=s";
			final HttpURLConnection connection = (HttpURLConnection) new URL(ulStr).openConnection();
			connection.setFixedLengthStreamingMode(textBytes.length);
			connection.setRequestMethod("PUT");
			connection.setDoOutput(true);
			connection.setRequestProperty("Content-type", "text; charset=UTF-8");
			connection.setRequestProperty("Accept", "text");
			if (uploadHeader != null && uploadHeader.length() > 0) {
				connection.setRequestProperty("Cookie", uploadHeader);
			}

			up = connection.getOutputStream();
			up.write(textBytes);
			up.flush();

			if (connection.getResponseCode() != 200) {
				Result.failure(log, resRef, "Failed: " + URLDecoder.decode(connection.getResponseMessage(), "UTF-8"));
				return;
			}
		} catch (IOException e) {
			Result.failure(log, resRef, e.getMessage());
			return;
		} finally {
			if (up != null) {
				try {
					up.close();
				} catch (IOException e) {
					log.info("ignoring", e);
				}
			}
			if (down != null) {
				try {
					down.close();
				} catch (IOException e) {
					log.info("ignoring", e);
				}
			}
		}

		Result.success(log, resRef, "Submitted");
	}

	private void job_loadTest(final JLabel statusLabel, Result[] resRef, final String test) {
		final boolean assemble = assembleStamp.get() < sourceStamp.get();
		if (assemble) {
			job_assemble(statusLabel, resRef);
		}

		if (!assemble || resRef[0].isSuccess()) {
			setupStatus(statusLabel, "Loading Regs and Mem for...");
			validator.loadTest(resRef, test);
		} else {
			validator.clearTest();
		}
	}

	class AssembleAction extends AbstractAction {
		final boolean switchToTest;

		private AssembleAction(final String name, boolean switchToTest) {
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
						SwingUtilities.invokeLater(new Runnable() {
							public void run() {
								setEnabled(true);
								setupStatus(statusLabel, resRef[0]);
								if (switchToTest && resRef[0].isSuccess()) {
									view.getStrTabbedPane().setSelectedIndex(1);
								}
							}
						});
					}
				}
			});
		}
	}

	class SubmitAction extends AbstractAction {
		private SubmitAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getSourceFeedbackLabel();
			final String sourceText = view.getSourceTextArea().getText();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};
					try {
						job_submit(statusLabel, resRef, sourceText);
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

		private UpdateTestSelectionAction() {
			super("Update Test Selection");
		}

		public void actionPerformed(ActionEvent e) {
			final String selectedItem = (String) testComboModel.getSelectedItem();
			if (selectedItem.startsWith(PREFIX_TEST)) {
				selectTest(Integer.parseInt(selectedItem.substring(PREFIX_TEST.length())) - 1);
			}
		}
	}

	class TestStepAction extends AbstractAction {
		private TestStepAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getTestStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						final String testItem = (String) testComboModel.getSelectedItem();
						if (testItem.startsWith(PREFIX_TEST)) {
							final int testIndex = Integer.parseInt(testItem.substring(PREFIX_TEST.length())) - 1;
							job_loadTest(statusLabel, resRef, task.getTests().get(testIndex));
						} else {
							Result.failure(log, resRef, "Failed to parse test index: " + testItem);
						}

						if (resRef[0].isSuccess()) {
							setupStatus(statusLabel, "Resetting...");

							validator.reset(resRef);
						}
					} finally {
						SwingUtilities.invokeLater(new Runnable() {
							public void run() {
								setEnabled(true);
								setupStatus(statusLabel, resRef[0]);

								if (resRef[0].isSuccess()) {
									view.getStrTabbedPane().setSelectedIndex(2);
									setupStatus(view.getRunStatusLabel());
									fireDataPathChanged();
								}
							}
						});
					}
				}
			});
		}
	}

	class RunStepAction extends AbstractAction {
		protected final int steps;

		private RunStepAction(final String name, final int steps) {
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
						setupStatus(statusLabel, "Stepping...");

						validator.step(resRef, steps);
					} catch (Throwable t) {
						Result.failure(log, resRef, "Failed: " + G.report(t));
						log.trace("trace", t);
					} finally {
						SwingUtilities.invokeLater(new Runnable() {
							public void run() {
								setEnabled(true);
								setupStatus(statusLabel, resRef[0]);
								if (resRef[0].isSuccess()) {
									fireDataPathChanged();
								}
							}
						});
					}
				}
			});
		}
	}

	class RunResetAction extends AbstractAction {
		private RunResetAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getRunStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						setupStatus(statusLabel, "Resetting...");

						validator.reset(resRef);
					} finally {
						SwingUtilities.invokeLater(new Runnable() {
							public void run() {
								setupStatus(statusLabel, resRef[0]);
								if (resRef[0].isSuccess()) {
									fireDataPathChanged();
								}
								setEnabled(true);
							}
						});
					}
				}
			});
		}
	}

	class TestRunAction extends AbstractAction {
		private TestRunAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getTestStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						setupStatus(statusLabel, "Running...");

						final String testItem = (String) testComboModel.getSelectedItem();
						if (testItem.startsWith(PREFIX_TEST)) {
							final int testIndex = Integer.parseInt(testItem.substring(PREFIX_TEST.length())) - 1;
							validator.run(resRef, task.getTests().get(testIndex), getSource());
						} else {
							Result.failure(log, resRef, "Failed to parse test index: " + testItem);
						}
					} catch (Throwable t) {
						Result.failure(log, resRef, "Failed: " + G.report(t));
						log.trace("trace", t);
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

	class TestBatchAction extends AbstractAction {
		private TestBatchAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getTestStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						validator.batch(resRef, task, getSource(), null);
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

	private class SourceDocumentListener implements DocumentListener {
		public void insertUpdate(DocumentEvent e) {
			sourceStamp.set(System.currentTimeMillis());
			setupStatus(view.getSourceFeedbackLabel());
			setupStatus(view.getTestStatusLabel());
		}

		public void removeUpdate(DocumentEvent e) {
			sourceStamp.set(System.currentTimeMillis());
			setupStatus(view.getSourceFeedbackLabel());
			setupStatus(view.getTestStatusLabel());
		}

		public void changedUpdate(DocumentEvent e) {
			sourceStamp.set(System.currentTimeMillis());
			setupStatus(view.getSourceFeedbackLabel());
			setupStatus(view.getTestStatusLabel());
		}
	}

	private static void setupStatus(final JLabel label) {
		setupStatus(label, "...");
	}

	private static void setupStatus(final JLabel label, final String text) {
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

	private static void setupStatus(final JLabel label, final Result result) {
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

}
