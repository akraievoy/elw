package elw.dp.ui;

import elw.dp.swing.GBC;
import elw.dp.swing.Swing;

import javax.swing.*;
import java.awt.*;

public class TaskSelectorFrame extends JFrame {
	static final String TITLE_PREFIX = "DataPath";
	static final String TITLE = "Task Selection";

	JTable taskList;

	JTextPane taskDefinition;

	JButton activate;
	JButton exit;

	JPanel contentPanel;
	public static final String NO_TASK_TEXT = "Please select a task";

	public void init() {
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(getContentPanel(), BorderLayout.CENTER);

		pack();

		setTitle(TITLE_PREFIX + " :: " + TITLE);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	public JTable getTaskList() {
		if (taskList == null) {
			taskList = new JTable();

			taskList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		}

		return taskList;
	}

	public JTextPane getTaskDefinition() {
		if (taskDefinition == null) {
			taskDefinition = new JTextPane();

			taskDefinition.setContentType("text/html");
			taskDefinition.setEditable(false);
			taskDefinition.setText(NO_TASK_TEXT);
		}

		return taskDefinition;
	}

	public JButton getActivate() {
		if (activate == null) {
			activate = new JButton("Activate");
		}

		return activate;
	}

	public JButton getExit() {
		if (exit == null) {
			exit = new JButton("Exit");
		}

		return exit;
	}

	public JPanel getContentPanel() {
		if (contentPanel == null) {
			contentPanel = new JPanel(new GridBagLayout());

			GBC gbc = new GBC();

			gbc.moveTo(0, 0).span(1, 2).weight(1, 1).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

			contentPanel.add(Swing.border(Swing.scroll(getTaskList(), 160, 400), "Task list"), gbc.getVal());

			gbc.moveTo(1, 0).span(1, 1).weight(3, 1).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

			contentPanel.add(Swing.border(Swing.scroll(getTaskDefinition(), 360, 360), "Task definition"), gbc.getVal());

			gbc.moveTo(1, 1).span(1, 1).weight(0, 0).insets(2, 2).align(GridBagConstraints.EAST, GridBagConstraints.NONE);

			contentPanel.add(Swing.flow(new JComponent[]{getActivate(), getExit()}, 7), gbc.getVal());
		}

		return contentPanel;
	}

	public static void main(String[] args) {
		final TaskSelectorFrame frame = new TaskSelectorFrame();
		frame.init();
		frame.setVisible(true);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
}
