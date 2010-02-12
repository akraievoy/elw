/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import java.awt.*;

class DualSourceDataLine extends DataLine {

	private DataLine parentLine1, parentLine2;
	private String mask;
	private int firstHexDigit, lastHexDigit;

	public DualSourceDataLine(int size, Color c, String name, String label, String value, DataLine parentLine1, DataLine parentLine2,
							  String mask, int firstHexDigit, int lastHexDigit) {
		super(size, c, name, label, value);
		this.parentLine1 = parentLine1;
		this.parentLine2 = parentLine2;
		this.mask = mask;
		this.firstHexDigit = firstHexDigit;
		this.lastHexDigit = lastHexDigit;
	}

	public void execute() {
		String result = parentLine1.getValue() + parentLine2.getValue();
		setValue(result);
	}
}
