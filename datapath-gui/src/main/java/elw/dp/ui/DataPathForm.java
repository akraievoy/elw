package elw.dp.ui;

import javax.swing.*;
import java.awt.*;

public class DataPathForm {
	protected JTextPane problemTextPane;
	protected JTabbedPane strTabbedPane;
	protected JTextArea sourceTextArea;
	protected JComboBox testComboBox;
	protected JTabbedPane pclTabbedPane;
	protected JTextPane logTextPane;
	protected JButton sourceCompileButton;
	protected JButton testRunButton;
	protected JButton testStepButton;
	protected JTextArea testMemTextArea;
	protected JTextArea testRegsTextArea;
	protected JButton testAddCustomButton;
	protected JTextArea clipTextArea;
	protected JButton runStepButton;
	protected JButton runRunButton;
	protected JButton runResetButton;
	protected JTable runRegsTable;
	protected JTable runMemTable;
	protected JTable runInstructionsTable;
	protected JLabel sourceFeedbackLabel;
	protected JLabel runStatusLabel;
	protected JPanel rootPanel;

	public JPanel getRootPanel() {
		return rootPanel;
	}

	public JTextArea getClipTextArea() {
		return clipTextArea;
	}

	public JTextPane getLogTextPane() {
		return logTextPane;
	}

	public JTextPane getProblemTextPane() {
		return problemTextPane;
	}

	public JTable getRunInstructionsTable() {
		return runInstructionsTable;
	}

	public JTable getRunMemTable() {
		return runMemTable;
	}

	public JTable getRunRegsTable() {
		return runRegsTable;
	}

	public JButton getRunResetButton() {
		return runResetButton;
	}

	public JButton getRunRunButton() {
		return runRunButton;
	}

	public JLabel getRunStatusLabel() {
		return runStatusLabel;
	}

	public JButton getRunStepButton() {
		return runStepButton;
	}

	public JButton getSourceCompileButton() {
		return sourceCompileButton;
	}

	public JLabel getSourceFeedbackLabel() {
		return sourceFeedbackLabel;
	}

	public JTextArea getSourceTextArea() {
		return sourceTextArea;
	}

	public JTabbedPane getStrTabbedPane() {
		return strTabbedPane;
	}

	public JTabbedPane getPclTabbedPane() {
		return pclTabbedPane;
	}

	public JButton getTestAddCustomButton() {
		return testAddCustomButton;
	}

	public JComboBox getTestComboBox() {
		return testComboBox;
	}

	public JTextArea getTestMemTextArea() {
		return testMemTextArea;
	}

	public JTextArea getTestRegsTextArea() {
		return testRegsTextArea;
	}

	public JButton getTestRunButton() {
		return testRunButton;
	}

	public JButton getTestStepButton() {
		return testStepButton;
	}

	{
// GUI initializer generated by IntelliJ IDEA GUI Designer
// >>> IMPORTANT!! <<<
// DO NOT EDIT OR ADD ANY CODE HERE!
		$$$setupUI$$$();
	}

	/**
	 * Method generated by IntelliJ IDEA GUI Designer
	 * >>> IMPORTANT!! <<<
	 * DO NOT edit this method OR call it in your code!
	 *
	 * @noinspection ALL
	 */
	private void $$$setupUI$$$() {
		rootPanel = new JPanel();
		rootPanel.setLayout(new BorderLayout(0, 0));
		final JSplitPane splitPane1 = new JSplitPane();
		splitPane1.setDividerLocation(291);
		splitPane1.setOneTouchExpandable(true);
		splitPane1.setOrientation(0);
		rootPanel.add(splitPane1, BorderLayout.CENTER);
		strTabbedPane = new JTabbedPane();
		strTabbedPane.setTabPlacement(3);
		splitPane1.setLeftComponent(strTabbedPane);
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		strTabbedPane.addTab("Source", panel1);
		final JScrollPane scrollPane1 = new JScrollPane();
		panel1.add(scrollPane1, BorderLayout.CENTER);
		sourceTextArea = new JTextArea();
		sourceTextArea.setColumns(60);
		sourceTextArea.setFont(new Font("Courier New", sourceTextArea.getFont().getStyle(), sourceTextArea.getFont().getSize()));
		scrollPane1.setViewportView(sourceTextArea);
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		panel1.add(panel2, BorderLayout.SOUTH);
		sourceCompileButton = new JButton();
		sourceCompileButton.setActionCommand("Compile");
		sourceCompileButton.setLabel("Compile");
		sourceCompileButton.setText("Compile");
		panel2.add(sourceCompileButton);
		sourceFeedbackLabel = new JLabel();
		sourceFeedbackLabel.setText("...");
		panel2.add(sourceFeedbackLabel);
		final JPanel panel3 = new JPanel();
		panel3.setLayout(new BorderLayout(0, 0));
		strTabbedPane.addTab("Test", panel3);
		final JPanel panel4 = new JPanel();
		panel4.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		panel3.add(panel4, BorderLayout.SOUTH);
		final JLabel label1 = new JLabel();
		label1.setText("Test");
		panel4.add(label1);
		testComboBox = new JComboBox();
		panel4.add(testComboBox);
		testAddCustomButton = new JButton();
		testAddCustomButton.setText("Add Custom Test");
		panel4.add(testAddCustomButton);
		final JToolBar.Separator toolBar$Separator1 = new JToolBar.Separator();
		panel4.add(toolBar$Separator1);
		testRunButton = new JButton();
		testRunButton.setText("Run");
		panel4.add(testRunButton);
		testStepButton = new JButton();
		testStepButton.setText("Step");
		panel4.add(testStepButton);
		final JSplitPane splitPane2 = new JSplitPane();
		splitPane2.setResizeWeight(0.5);
		panel3.add(splitPane2, BorderLayout.CENTER);
		final JScrollPane scrollPane2 = new JScrollPane();
		splitPane2.setRightComponent(scrollPane2);
		testRegsTextArea = new JTextArea();
		scrollPane2.setViewportView(testRegsTextArea);
		final JScrollPane scrollPane3 = new JScrollPane();
		splitPane2.setLeftComponent(scrollPane3);
		testMemTextArea = new JTextArea();
		scrollPane3.setViewportView(testMemTextArea);
		final JPanel panel5 = new JPanel();
		panel5.setLayout(new BorderLayout(0, 0));
		strTabbedPane.addTab("Run", panel5);
		final JPanel panel6 = new JPanel();
		panel6.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		panel5.add(panel6, BorderLayout.SOUTH);
		runStepButton = new JButton();
		runStepButton.setText("Step");
		panel6.add(runStepButton);
		runRunButton = new JButton();
		runRunButton.setText("Run");
		panel6.add(runRunButton);
		final JToolBar.Separator toolBar$Separator2 = new JToolBar.Separator();
		panel6.add(toolBar$Separator2);
		runResetButton = new JButton();
		runResetButton.setText("Reset");
		panel6.add(runResetButton);
		runStatusLabel = new JLabel();
		runStatusLabel.setText("...");
		panel6.add(runStatusLabel);
		final JPanel panel7 = new JPanel();
		panel7.setLayout(new BorderLayout(0, 0));
		panel5.add(panel7, BorderLayout.CENTER);
		final JSplitPane splitPane3 = new JSplitPane();
		splitPane3.setDividerLocation(120);
		splitPane3.setOrientation(0);
		panel7.add(splitPane3, BorderLayout.CENTER);
		final JScrollPane scrollPane4 = new JScrollPane();
		splitPane3.setRightComponent(scrollPane4);
		runInstructionsTable = new JTable();
		scrollPane4.setViewportView(runInstructionsTable);
		final JSplitPane splitPane4 = new JSplitPane();
		splitPane4.setDividerLocation(180);
		splitPane3.setLeftComponent(splitPane4);
		final JScrollPane scrollPane5 = new JScrollPane();
		splitPane4.setLeftComponent(scrollPane5);
		runRegsTable = new JTable();
		scrollPane5.setViewportView(runRegsTable);
		final JScrollPane scrollPane6 = new JScrollPane();
		splitPane4.setRightComponent(scrollPane6);
		runMemTable = new JTable();
		scrollPane6.setViewportView(runMemTable);
		pclTabbedPane = new JTabbedPane();
		splitPane1.setRightComponent(pclTabbedPane);
		final JPanel panel8 = new JPanel();
		panel8.setLayout(new BorderLayout(0, 0));
		pclTabbedPane.addTab("Problem", panel8);
		final JScrollPane scrollPane7 = new JScrollPane();
		panel8.add(scrollPane7, BorderLayout.CENTER);
		problemTextPane = new JTextPane();
		problemTextPane.setEditable(false);
		scrollPane7.setViewportView(problemTextPane);
		final JPanel panel9 = new JPanel();
		panel9.setLayout(new BorderLayout(0, 0));
		pclTabbedPane.addTab("Clipper", panel9);
		final JScrollPane scrollPane8 = new JScrollPane();
		panel9.add(scrollPane8, BorderLayout.CENTER);
		clipTextArea = new JTextArea();
		scrollPane8.setViewportView(clipTextArea);
		final JPanel panel10 = new JPanel();
		panel10.setLayout(new BorderLayout(0, 0));
		pclTabbedPane.addTab("Log", panel10);
		final JScrollPane scrollPane9 = new JScrollPane();
		panel10.add(scrollPane9, BorderLayout.CENTER);
		logTextPane = new JTextPane();
		logTextPane.setEditable(false);
		logTextPane.setFont(new Font("Courier New", logTextPane.getFont().getStyle(), logTextPane.getFont().getSize()));
		scrollPane9.setViewportView(logTextPane);
	}

	/**
	 * @noinspection ALL
	 */
	public JComponent $$$getRootComponent$$$() {
		return rootPanel;
	}
}
