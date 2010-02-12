package elw.dp.app;

import elw.dp.mips.DataPath;
import elw.dp.mips.Reg;
import elw.dp.mips.asm.Data;
import elw.dp.mips.asm.MipsAssembler;
import elw.dp.swing.Swing;
import elw.dp.ui.DataPathForm;
import elw.dp.ui.SourceEditorFrame;
import elw.dp.ui.StepperFrame;
import elw.vo.Course;
import elw.vo.Test;
import elw.vo.Version;
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
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

public class Controller {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(Controller.class);

	DataPathForm dpForm = new DataPathForm();
	Swing.ActionFactory actions = new Swing.ActionFactory(this);

	//  Source Editor frame
	SourceEditorFrame sourceEditor;
	DefaultComboBoxModel tCaseModel;

	//  Stepper frame
	StepperFrame stepper;

	InstructionsTableModel instructionsTableModel;
	RegistersTableModel registersTableModel;
	MemoryTableModel memoryTableModel;

	//  application state
	Version selectedTask;
	Test selectedCase;
	DataPath dataPath = new DataPath();

	public void init() throws IOException {
		final InputStream modelStream = Controller.class.getResourceAsStream("/aos-s10.json");
		final ObjectMapper mapper = new ObjectMapper();
		final Course course = mapper.readValue(modelStream, Course.class);

		selectedTask = course.getAssBundles()[0].getAssignments()[0].getVersions()[0];
		final Properties actionProps = new Properties();
		actionProps.load(this.getClass().getResourceAsStream("actions.properties"));

		actions.init(actionProps);

		tCaseModel = new DefaultComboBoxModel(selectedTask.getTests());

		final FeedbackAppender feedbackAppender = new FeedbackAppender(dpForm.getLogTextPane());
		feedbackAppender.setThreshold(Level.ALL);
		Logger.getRootLogger().addAppender(feedbackAppender);

		log.info("started up, yeah!");
		dpForm.getProblemTextPane().setText(G4Str.join(selectedTask.getStatementHtml(), "\n"));
		dpForm.getTestComboBox().setModel(tCaseModel);
		//  TODO hide this
		dpForm.getSourceTextArea().setText(G4Str.join(selectedTask.getSolution(), "\n"));
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

	public SourceEditorFrame getSourceEditor() {
		if (sourceEditor == null) {
			sourceEditor = new SourceEditorFrame();
			sourceEditor.init();

			sourceEditor.getRun().setAction(actions.forKey("run"));
			sourceEditor.getClose().setAction(actions.forKey("closeSource"));
			sourceEditor.getSubmit().setAction(actions.forKey("submit"));
		}

		return sourceEditor;
	}

	public StepperFrame getStepper() {
		if (stepper == null) {
			stepper = new StepperFrame();
			stepper.init();

			stepper.getStepInto().setAction(actions.forKey("step"));
			stepper.getReset().setAction(actions.forKey("reset"));
			stepper.getClose().setAction(actions.forKey("closeStepper"));

		}
		return stepper;
	}

	class AssembleAction extends AbstractAction {
		final MipsAssembler assembler = new MipsAssembler();

		public AssembleAction() {
			super("Assemble");
		}

		public void load(java.util.List<String> newCode) {
			assembler.assembleLoad(newCode);
			reload();
		}

		public void reload() {
			dataPath.getInstructions().setInstructions(assembler.getInstructions2());
		}

		public void actionPerformed(ActionEvent e) {
		}
	}

	class LoadDataAction extends AbstractAction {
		int[] lastLoaded = new int[0];

		public LoadDataAction() {
			super("Load Data");
		}

		public void listReload() {
			dataPath.getMemory().setData(lastLoaded);
		}

		public void load(List<String> temp) {
			int maxAddress = validateInput(temp);
			if (maxAddress < 0) {
				log.info("Please correct validation errors. Data loading terminated.");
				return;
			}

			lastLoaded = new int[maxAddress];
			for (String tempStr : temp) {
				int index = tempStr.indexOf(":");
				String address = tempStr.substring(0, index);
				int word = Data.hex2int(tempStr.substring(index + 1));
				int memoryIndex = Data.hex2int(address) / 4;
				lastLoaded[memoryIndex] = word;
			}

			listReload();

			log.info("Data Successfully Loaded!");
		}

		public int validateInput(List<String> temp) {
			int maxAddress = 0;
			for (String tempStr : temp) {
				int index = tempStr.indexOf(":");
				if (index == -1 || index == 0) {
					log.warn("Data item '" + tempStr + "' must be given with an address.");
					return -1;
				}

				String address = tempStr.substring(0, index);
				if (!Data.isHexPositive(address)) {
					log.warn("Address '" + address + "' must be an hexadecimal number.");
					return -1;
				}

				long addressValue = Data.hex2long(address);

				if (addressValue > Integer.MAX_VALUE) {
					log.warn("Address '" + address + "' must not exceed " + Integer.toString(Integer.MAX_VALUE, 16));
					return -1;
				}

				maxAddress = Math.max(maxAddress, (int) addressValue);

				String word = tempStr.substring(index + 1);
				if (word.length() != 8) {
					log.warn("'" + word + "' must have 8 hex digits!");
					return -1;
				}
				for (int j = 0; j < word.length(); j++) {
					int digit = word.charAt(j);
					if ((digit < '0' || digit > '9') &&
							(digit < 'A' || digit > 'F')) {
						log.warn("'" + word + "' has an ILLEGAL character (not a Hex digit).");
						return -1;
					}
				}
			}

			return maxAddress;
		}

		public void actionPerformed(ActionEvent e) {
		}
	}

	class LoadRegistersAction extends AbstractAction {
		protected final int[] lastLoaded = new int[Reg.values().length];

		public LoadRegistersAction() {
			super("Load registers");
		}

		public void actionPerformed(ActionEvent e) {
		}

		public void load(List<String> codeLines) {
			final boolean validated = validateInput(codeLines);

			if (!validated) {
				log.info("Please correct validation errors. Register loading terminated.");
				return;
			}

			Arrays.fill(this.lastLoaded, 0);
			for (String tempStr : codeLines) {
				final int index = tempStr.indexOf(':');
				final String regToken = tempStr.substring(0, index);
				final Reg reg = Reg.fromString(regToken);
				final String word = tempStr.substring(index + 1);

				lastLoaded[reg.ordinal()] = Data.hex2int(word);
			}

			listReload();

			log.info("Registers Successfully Loaded!");
		}

		private boolean validateInput(List<String> codeLines) {
			if (codeLines.isEmpty()) {
				log.info("Registers NOT loaded - empty source");
				return false;
			}

			for (String tempStr : codeLines) {
				int index = tempStr.indexOf(':');
				if (index == -1 || index == 0) {
					log.warn("'" + tempStr + "' must have reg:value format.");
					return false;
				}

				String registerNumber = tempStr.substring(0, index);
				final Reg reg = Reg.fromString(registerNumber);
				if (reg == Reg.zero) {
					log.warn("Register '" + reg + "' is Constant Holding 00000000. Its Value CANNOT be Changed!");
					return false;
				}

				final String word = tempStr.substring(index + 1);
				if (word.length() > 8) {
					log.warn("'" + word + "' must have not more than 8 hex digits.");
					return false;
				}

				if (!Data.isHexPositive(word)) {
					log.warn("'" + word + "' is not a legal hex number.");
					return false;
				}
			}

			return true;
		}

		public void listReload() {
			dataPath.getRegisters().load(lastLoaded);
			dataPath.getRegisters().setReg(Reg.pc, dataPath.getInstructions().getBase());
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

		frame.getContentPane().add(instance.dpForm.getRootPanel());
		frame.pack();
		frame.setVisible(true);
	}

	class TaskListSelectionListener implements ListSelectionListener {
		public void valueChanged(ListSelectionEvent e) {
		}
	}

	public void do_activate(ActionEvent e) {
		if (selectedTask == null) {
			return;
		}

	}

	public void do_exit(ActionEvent e) {
		System.exit(0);
	}


	public void do_run(ActionEvent e) {
		Test tCase = (Test) tCaseModel.getSelectedItem();

		getSourceEditor().setVisible(false);

		getStepper().setVisible(true);
	}

	public void do_submit(ActionEvent e) {
		//  TODO do something here
	}

	public void do_closeSource(ActionEvent e) {
		getSourceEditor().setVisible(false);
	}

	public void do_closeStepper(ActionEvent e) {
		getStepper().setVisible(false);
		getSourceEditor().setVisible(true);
	}
}
