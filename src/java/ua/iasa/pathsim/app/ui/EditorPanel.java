/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim.app.ui;

import javax.swing.*;
import com.bws.base.swing.GBC;
import ua.iasa.pathsim.app.*;
import java.awt.*;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: EditorPanel.java,v 1.1 2006/12/28 11:55:33 Anton S. Kraievoy Exp $
 */

public class EditorPanel extends JPanel {
    protected JTextArea instructionsInput = new JTextArea();
    protected JTextArea dataInput = new JTextArea();
    protected JTextArea registersInput = new JTextArea();

    public EditorPanel(Control control) {
        control.getLoadDataAction().setInput(dataInput);
        control.getLoadRegistersAction().setRegistersInput(registersInput);
        control.getAssembleAction().setTextInput(instructionsInput);

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

    protected JPanel createButtonsPanel(Control control) {
        final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));

        buttonsPanel.add(new JButton(control.getLoadDataAction()));
        buttonsPanel.add(new JButton(control.getAssembleAction()));
        buttonsPanel.add(new JButton(control.getLoadRegistersAction()));

        return buttonsPanel;
    }
}

