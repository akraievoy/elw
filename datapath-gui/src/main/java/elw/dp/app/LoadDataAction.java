/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.app;

import elw.dp.mips.asm.Data;
import elw.dp.mips.vis.MemoryModel;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.event.ActionEvent;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

public class LoadDataAction extends AbstractAction {
	private static final org.slf4j.Logger log = LoggerFactory.getLogger(LoadDataAction.class);

	protected final MemoryModel memModel;
	protected final int[] lastLoaded;
	protected JTextComponent input;

	public LoadDataAction(MemoryModel newMemModel) {
		super("Load Data");
		memModel = newMemModel;
		lastLoaded = new int[newMemModel.getSize()];
	}

	public void listReload() {
		memModel.setData(lastLoaded);
	}

	public void setDataValue(String value, int arrayIndex) {
		memModel.setDataForIndex(arrayIndex, Data.hex2int(value));
	}

	public void load(List<String> temp) {
		boolean validated = validateInput(temp);
		if (!validated) {
			log.info("Please correct validation errors. Data loading terminated.");
			return;
		}

		Arrays.fill(lastLoaded, 0);
		for (String tempStr : temp) {
			int index = tempStr.indexOf(":");
			String address = tempStr.substring(0, index);
			int word = Data.hex2int(tempStr.substring(index + 1));
			int memoryIndex = Data.hex2int(address) / 4;
			lastLoaded[memoryIndex] = word;
		}

		listReload();

		log.info("Data Successfully Loaded!");
	}

	public static boolean validateInput(List<String> temp) {
		for (String tempStr : temp) {
			int index = tempStr.indexOf(":");
			if (index == -1 || index == 0) {
				log.warn("Data item '" + tempStr + "' must be given with an address.");
				return false;
			}

			String address = tempStr.substring(0, index);
			if (!Data.isHexPositive(address)) {
				log.warn("Address '" + address + "' must be an hexadecimal number.");
				return false;
			}

			long addressValue = Data.hex2long(address);
			if (addressValue > 400 || addressValue % 4 != 0) {
				log.warn("Address '" + address + "' must be a multiple of 4 and no more than 400.");
				return false;
			}
		}

		for (String tempStr : temp) {
			int index = tempStr.indexOf(":");
			String word = tempStr.substring(index + 1);
			if (word.length() != 8) {
				log.warn("'" + word + "' must have 8 hex digits!");
				return false;
			}
			for (int j = 0; j < word.length(); j++) {
				int digit = word.charAt(j);
				if ((digit < '0' || digit > '9') &&
						(digit < 'A' || digit > 'F')) {
					log.warn("'" + word + "' has an ILLEGAL character (not a Hex digit).");
					return false;
				}
			}
		}

		return true;
	}

	public JTextComponent getInput() {
		return input;
	}

	public void setInput(JTextComponent input) {
		this.input = input;
	}

	public void actionPerformed(ActionEvent e) {
		if (input != null) {
			load(Data.extractCode(input.getText()));
		} else {
			log.warn("input == null, nothing to assemble");
		}

	}
}

