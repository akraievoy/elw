/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.ui;

import elw.dp.swing.GBC;
import java.awt.*;
import javax.swing.*;

public class EditorPanel extends JPanel {
    JTextArea instructionsInput = new JTextArea();
    JTextArea dataInput = new JTextArea();
    JTextArea registersInput = new JTextArea();
    JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));

    public EditorPanel() {
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

        gbc.moveTo(1,3).span(3, 1).weight(1,1);
        add(Util.wrap(instructionsInput, "Instructions"), gbc.getVal());

        gbc.moveTo(2,4);
        add(Box.createRigidArea(new Dimension(4,4)), gbc.getVal());

        gbc.moveTo(1,5).weight(1, 1);
        add(Util.wrap(dataInput, "Data"), gbc.getVal());

        gbc.moveTo(3,5).weight(1, 1);
        add(Util.wrap(registersInput, "Registers"), gbc.getVal());

        gbc.moveTo(1,6).weight(1,0).span(3,1);
        add(Box.createRigidArea(new Dimension(200, 4)), gbc.getVal());
        gbc.moveTo(1,7).weight(1,0).span(3,1);
        add(Box.createHorizontalStrut(200), gbc.getVal());
        setPreferredSize(new Dimension(200, 300));
        setMinimumSize(new Dimension(200, 300));
    }

    public JTextArea getInstructionsInput() {
        return instructionsInput;
    }

    public JTextArea getDataInput() {
        return dataInput;
    }

    public JTextArea getRegistersInput() {
        return registersInput;
    }

    public JPanel getButtonsPanel() {
        return buttonsPanel;
    }

    public void installActions(final AbstractAction[] abstractActions) {
        for (AbstractAction action : abstractActions) {
            getButtonsPanel().add(new JButton(action));
        }
    }
}

