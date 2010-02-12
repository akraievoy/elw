/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.ui;

import java.awt.*;
import javax.swing.*;

import elw.dp.app.*;
import elw.dp.mips.vis.DataPathPanel;
import elw.dp.mips.vis.InstructionsModel;
import elw.dp.mips.vis.MemoryModel;
import elw.dp.mips.vis.RegistersModel;
import elw.dp.swing.JTextAreaLogHandler;

import java.util.logging.*;

public class DataPathOld implements Control {
    protected DataPathPanel dpPanel;
    protected JFrame appFrame;
    protected JTextArea loggerArea;

    protected LoadDataAction loadDataAction;
    protected ResetAction resetAction;
    protected AssembleAction assembleAction;
    protected LoadRegistersAction loadRegistersAction;

    public DataPathOld() {
    }

	public void init() {

        appFrame = new JFrame("A simple MIPS architecture");
        appFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        loggerArea = new JTextArea();

        final Container contentPane = appFrame.getContentPane();
        contentPane.setLayout(new BorderLayout());

        dpPanel = new DataPathPanel(this);

        resetAction = new ResetAction(this);
        assembleAction = new AssembleAction(getInstructions());
        loadDataAction = new LoadDataAction(getData());
        loadRegistersAction = new LoadRegistersAction(getRegisters());

        final EditorPanel editorPanel = new EditorPanel(/*this*/);
        final RunnerPanel runnerPanel = new RunnerPanel(/*this*/);

        contentPane.add(Util.wrap(dpPanel, "MIPS architecture"), BorderLayout.CENTER);
        contentPane.add(editorPanel, BorderLayout.WEST);
        contentPane.add(runnerPanel, BorderLayout.EAST);
        contentPane.add(createLoggerPanel(loggerArea), BorderLayout.SOUTH);

        appFrame.pack();
        appFrame.setVisible(true);

        final JTextAreaLogHandler jTextAreaLogHandler = new JTextAreaLogHandler(loggerArea, 65536*4);
        jTextAreaLogHandler.setLevel(Level.FINEST);
        Logger.getLogger("ua.iasa.pathsim").addHandler(jTextAreaLogHandler);
        Logger.getLogger("com.bws").addHandler(jTextAreaLogHandler);
    }

    public RegistersModel getRegisters() {
        return (RegistersModel) dpPanel.findDevice("registers");
    }

    public MemoryModel getData() {
        return (MemoryModel) dpPanel.findDevice("dataMemory");
    }

    public InstructionsModel getInstructions() {
        return (InstructionsModel) dpPanel.findDevice("instructionMemory");
    }

    public AssembleAction getAssembleAction() {
        return assembleAction;
    }

    public LoadDataAction getLoadDataAction() {
        return loadDataAction;
    }

    public LoadRegistersAction getLoadRegistersAction() {
        return loadRegistersAction;
    }

    public ResetAction getResetAction() {
        return resetAction;
    }

    protected JComponent createLoggerPanel(final JTextArea loggerArea) {
        JScrollPane scrollPane = new JScrollPane();
        scrollPane.setViewportView(loggerArea);
        scrollPane.setPreferredSize(new Dimension(320, 240));
        return scrollPane;
    }

    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            System.out.println("Error setting native LAF: " + e);
        }

        DataPathOld dataPath = new DataPathOld();
        dataPath.init();
    }

    public void resetMachine() {
        assembleAction.reload();
        loadDataAction.listReload();
        loadRegistersAction.listReload();
        dpPanel.resetDataPath();
    }
}
