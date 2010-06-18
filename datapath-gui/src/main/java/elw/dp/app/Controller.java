package elw.dp.app;

import base.pattern.Result;
import elw.dp.mips.*;
import elw.dp.ui.DataPathForm;
import elw.dp.ui.FeedbackAppender;
import elw.dp.ui.RendererFactory;
import elw.vo.Test;
import elw.vo.Version;
import org.akraievoy.gear.G;
import org.akraievoy.gear.G4Str;
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
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLDecoder;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

public class Controller {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(Controller.class);

	protected static final String LINE_SEPARATOR = System.getProperty("line.separator");
	protected static final Pattern PATTERN_LINE_SEPARATOR = Pattern.compile("\r|\r\n|\n");

	protected final DataPathForm view = new DataPathForm();

	//  application state
	protected AtomicLong sourceStamp = new AtomicLong(1); // NOTE: no modifications still require assembly to run before stepping
	protected AtomicLong assembleStamp = new AtomicLong(0);

	protected Version selectedTask;
	protected String uploadUrl;
	protected String uploadHeader;
	protected String elwCtx;

	protected final DefaultComboBoxModel testComboModel = new DefaultComboBoxModel();
	//	app data (compile/test/run cycle)

	private final MipsValidator validator = new MipsValidator();

	InstructionsTableModel tmInstructions = new InstructionsTableModel(validator.getDataPath().getInstructions());
	RegistersTableModel tmRegs = new RegistersTableModel(validator.getDataPath().getRegisters());
	MemoryTableModel tmMemory = new MemoryTableModel(validator.getDataPath().getMemory());

	//	actions
	protected final ScheduledExecutorService executor = new ScheduledThreadPoolExecutor(3);
	protected final AssembleAction aAssemble = new AssembleAction("Assemble>", true);
	protected final AssembleAction aVerify = new AssembleAction("Verify", false);
	protected final SubmitAction aSubmit = new SubmitAction("Submit");
	protected final UpdateTestSelectionAction aUpdateTestSelection = new UpdateTestSelectionAction();
	protected final TestStepAction aTestStep = new TestStepAction("Step>");
	protected final TestRunAction aTestRun = new TestRunAction("Run");
	protected final TestBatchAction aTestBatch = new TestBatchAction("Batch");
	protected final RunStepAction aRunStep = new RunStepAction("Step", 1);
	protected final RunStepAction aRunRun = new RunStepAction("Run", validator.getRunSteps());
	protected final RunResetAction aRunReset = new RunResetAction("Reset");

	public void setSelectedTask(Version selectedTask) {
		this.selectedTask = selectedTask;
	}

	public void setUploadUrl(String uploadUrl) {
		this.uploadUrl = uploadUrl;
	}

	public void setUploadHeader(String uploadHeader) {
		this.uploadHeader = uploadHeader;
	}

	public void setElwCtx(String elwCtx) {
		this.elwCtx = elwCtx;
	}

	public DataPathForm getView() {
		return view;
	}

	public void init() {
		if (selectedTask == null) {
			throw new IllegalStateException("No selected task");
		}

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

		setupStatus(view.getTestStatusLabel());
		setupStatus(view.getRunStatusLabel());
	}

	protected void job_assemble(final JLabel statusLabel, final Result[] resRef) {
		setupStatus(statusLabel, "Assembling...");

		final String[] sourceLines = getSource();
		final Instruction[] newInstructions = validator.assemble(resRef, sourceLines);
		if (newInstructions != null) {
			assembleStamp.set(System.currentTimeMillis());
		}
	}

	protected String[] getSource() {
		final String source = view.getSourceTextArea().getText();
		
		return PATTERN_LINE_SEPARATOR.split(source);
	}

	protected void job_submit(final JLabel statusLabel, final Result[] resRef, String sourceText) {
		if (uploadUrl == null || uploadUrl.length() == 0) {
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

			final HttpURLConnection connection = (HttpURLConnection) new URL(uploadUrl+"?elw_ctx="+ elwCtx).openConnection();
			connection.setFixedLengthStreamingMode(textBytes.length);
			connection.setRequestMethod("POST");
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

	protected void job_loadTest(final JLabel statusLabel, Result[] resRef, final Test test) {
		final boolean assemble = assembleStamp.get() < sourceStamp.get();
		if (assemble) {
			job_assemble(statusLabel, resRef);
		}

		if (!assemble || resRef[0].isSuccess()) {
			//	LATER #163
			//	TODO test.getName() seems to return null in most cases
			setupStatus(statusLabel, "Loading Regs and Mem for " + test.getName() + "...");
			validator.loadTest(resRef, test);
		} else {
			validator.clearTest();
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
		public SubmitAction(final String name) {
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
						setupStatus(statusLabel, "Resetting...");

						validator.reset(resRef);
					} finally {
						SwingUtilities.invokeLater(new Runnable() {
							public void run() {
								setEnabled(true);
							}
						});
					}
				}
			});
		}
	}

	class TestRunAction extends AbstractAction {
		public TestRunAction(final String name) {
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

						final Test test = (Test) testComboModel.getSelectedItem();
						validator.run(resRef, test, getSource());
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
		public TestBatchAction(final String name) {
			super(name);
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(false);
			final JLabel statusLabel = view.getTestStatusLabel();

			executor.submit(new Runnable() {
				public void run() {
					final Result[] resRef = new Result[]{new Result("status unknown", false)};

					try {
						validator.batch(resRef, selectedTask, getSource(), null);
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

	protected class SourceDocumentListener implements DocumentListener {
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

	public static void setupStatus(final JLabel label) {
		setupStatus(label, "...");
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
}
