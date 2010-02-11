/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim.app.ui;

import com.bws.base.swing.GBC;
import java.awt.*;
import javax.swing.*;

public class RunnerPanel extends JPanel {
    JTable instructionsTable = new JTable();
    JTable dataTable = new JTable();
    JTable registersTable = new JTable();
    JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));

    public RunnerPanel() {
        setLayout(new GridBagLayout());

        final GBC gbc = new GBC();
        gbc.moveTo(0,0);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());
        gbc.moveTo(2,0);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());
        gbc.moveTo(4,0);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());

        gbc.moveTo(1,1).span(3,1).weight(1,0);
        add(buttonsPanel, gbc.getVal());

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

    public void installActions(final AbstractAction[] abstractActions) {
        for (AbstractAction action : abstractActions) {
            buttonsPanel.add(new JButton(action));
        }
    }

    public JTable getInstructionsTable() {
        return instructionsTable;
    }

    public JTable getDataTable() {
        return dataTable;
    }

    public JTable getRegistersTable() {
        return registersTable;
    }
}

