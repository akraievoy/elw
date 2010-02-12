/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;

import java.awt.*;

class SingleSourceDataLine extends DataLine {

	private DataLine parentLine;
	private String mask;
	private int firstHexDigit, lastHexDigit, rightShift;

	public SingleSourceDataLine(int size, Color c, String name, String label, String value, DataLine parentLine, String mask,
								int firstHexDigit, int lastHexDigit, int rightShift) {
		super(size, c, name, label, value);
		this.parentLine = parentLine;
		this.mask = mask;
		this.firstHexDigit = firstHexDigit;
		this.lastHexDigit = lastHexDigit;
		this.rightShift = rightShift;
	}

	public void execute() {
		long inputValue = Data.hex2long(parentLine.getValue());
		long mask = Data.hex2long(this.mask);
		int shiftFactor = 1;
		String result = "";
		for (int i = 1; i <= rightShift; i++) {
			shiftFactor *= 2;
		}

		String value = Data.long2hex((inputValue & mask) / (long) shiftFactor, 8);
		for (int i = firstHexDigit; i <= lastHexDigit; i++) {
			result = result + value.charAt(i);
		}

		setValue(result);
	}
}
