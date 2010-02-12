/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.app;

import elw.dp.mips.asm.Data;
import elw.dp.mips.vis.InstructionsModel;
import elw.dp.mips.vis.RegistersModel;
import org.akraievoy.gear.G4Str;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.event.ActionEvent;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

public class LoadRegistersAction extends AbstractAction {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(LoadRegistersAction.class);

	protected final RegistersModel regModel;
	protected JTextComponent registersInput;
	protected final int[] lastLoaded = new int[RegistersModel.REGS];

	public LoadRegistersAction(RegistersModel regModel) {
		super("Load registers");
		this.regModel = regModel;
	}

	public void actionPerformed(ActionEvent e) {
		if (registersInput != null) {
			load(Data.extractCode(registersInput.getText()));
		} else {
			log.warn("registersInput == null, nothing to load");
		}
	}

	public boolean isRegisterNumber(String registerNumber) {
		return Data.isDecPositive(registerNumber) && Data.dec2int(registerNumber) < RegistersModel.REGS;
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
			final String register = tempStr.substring(0, index);
			final int lookupIndex = G4Str.indexOf(register, InstructionsModel.REGS);
			final int registerIndex = lookupIndex > 0 ? lookupIndex : Data.dec2int(register);
			if (registerIndex != 0) {
				String word = tempStr.substring(index + 1);
				this.lastLoaded[registerIndex] = Data.hex2int(word);
			}
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
			if (G4Str.indexOf(registerNumber, InstructionsModel.REGS) <= 0) {
				if (InstructionsModel.REGS[0].equalsIgnoreCase(registerNumber)) {
					log.warn("Register '" + InstructionsModel.REGS[0] + "' is Constant Holding 00000000. Its Value CANNOT be Changed!");
					return false;
				} else if (Data.dec2int(registerNumber) == 0) {
					log.warn("Register 0 is Constant Holding 00000000. Its Value CANNOT be Changed!");
					return false;
				} else if (!isRegisterNumber(registerNumber)) {
					log.warn("Register Number '" + registerNumber + "' must be a decimal number between 1 and 31.");
					return false;
				}
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

	public JTextComponent getRegistersInput() {
		return registersInput;
	}

	public void setRegistersInput(JTextComponent registersInput) {
		this.registersInput = registersInput;
	}

	public void listReload() {
		regModel.load(lastLoaded);
	}
}

