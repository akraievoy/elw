/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim.app;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.event.ActionEvent;
import ua.iasa.pathsim.*;

import java.util.logging.Logger;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: AssembleAction.java,v 1.1 2006/12/28 10:57:24 Anton S. Kraievoy Exp $
 */
public class AssembleAction extends AbstractAction {
    private static final Logger log = Logger.getLogger(AssembleAction.class.getName());

    protected final Assembler assembler = new MipsAssembler();
    protected final InstructionsModel instructionsModel;
    protected JTextComponent textInput;

    public AssembleAction(InstructionsModel instructionsModel) {
        super("Assemble");
        this.instructionsModel = instructionsModel;
    }

    public void load(java.util.List<String> newCode) {
        assembler.assembleLoad(newCode);
        reload();
    }

    public void reload() {
        instructionsModel.setInstructions(assembler.getInstructions(), assembler.getCodeLines());
    }

    public void actionPerformed(ActionEvent e) {
        if (textInput != null) {
            load(Data.extractCode(textInput.getText()));
        } else {
            log.warning("textInput == null, nothing to assemble");
        }
    }

    public JTextComponent getTextInput() {
        return textInput;
    }

    public void setTextInput(JTextComponent textInput) {
        this.textInput = textInput;
    }
}