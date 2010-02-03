/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim.app.ui;

import javax.swing.*;
import ua.iasa.pathsim.app.Control;
import java.awt.*;
import com.bws.base.swing.GBC;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: RunnerPanel.java,v 1.1 2006/12/28 11:55:36 Anton S. Kraievoy Exp $
 */

public class RunnerPanel extends JPanel {
    protected JTable instructionsTable = new JTable();
    protected JTable dataTable = new JTable();
    protected JTable registersTable = new JTable();

    public RunnerPanel(Control control) {
        instructionsTable.setModel(control.getInstructions().getTableModel());
        dataTable.setModel(control.getData().getTableModel());
        registersTable.setModel(control.getRegisters().getTableModel());

        setLayout(new GridBagLayout());

        final GBC gbc = new GBC();
        gbc.moveTo(0,0);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());
        gbc.moveTo(2,0);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());
        gbc.moveTo(4,0);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());

        gbc.moveTo(1,1).span(3,1).weight(1,0);
        add(createButtonsPanel(control), gbc.getVal());

        gbc.moveTo(2,0);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());

        gbc.moveTo(1,3).span(3, 1).weight(3,1);
        add(Util.wrap(instructionsTable, "Instructions"), gbc.getVal());

        gbc.moveTo(2,4);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());

        gbc.moveTo(1,5).weight(1.25, 1);
        add(Util.wrap(dataTable, "Data"), gbc.getVal());

        gbc.moveTo(3,5).weight(1.75, 1);
        add(Util.wrap(registersTable, "Registers"), gbc.getVal());

        gbc.moveTo(1,6).weight(1,0).span(3,1);
        add(Box.createRigidArea(new Dimension(400, 4)), gbc.getVal());
        gbc.moveTo(1,7).weight(1,0).span(3,1);
        add(Box.createHorizontalStrut(400), gbc.getVal());
        setPreferredSize(new Dimension(400, 300));
        setMinimumSize(new Dimension(400, 300));
    }

    protected JPanel createButtonsPanel(Control control) {
        final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));

        buttonsPanel.add(new JButton(control.getResetAction()));

        return buttonsPanel;
    }
}

