/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.app;

import elw.dp.mips.asm.Data;
import elw.dp.mips.vis.Assembler;
import elw.dp.mips.vis.InstructionsModel;
import elw.dp.mips.vis.MipsAssembler;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.event.ActionEvent;
import java.util.logging.Logger;

public class AssembleAction extends AbstractAction {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(AssembleAction.class);

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
			log.warn("textInput == null, nothing to assemble");
		}
	}

	public JTextComponent getTextInput() {
		return textInput;
	}

	public void setTextInput(JTextComponent textInput) {
		this.textInput = textInput;
	}
}