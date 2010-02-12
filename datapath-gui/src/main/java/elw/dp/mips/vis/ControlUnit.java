/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;

import java.awt.*;

class ControlUnit extends UtilityUnit {

	private DataLine input, signal1, signal2, signal3;
	private DataLine signal4, signal5, signal6, signal7;
	private DataLine signal8, signal9;

	public ControlUnit(double x, double y, double w, double h, Color c, String label1, String label2, String name, DataLine input, DataLine signal1,
					   DataLine signal2, DataLine signal3, DataLine signal4, DataLine signal5, DataLine signal6, DataLine signal7, DataLine signal8,
					   DataLine signal9) {
		super(x, y, w, h, c, label1, label2, name);
		this.input = input;
		this.signal1 = signal1;
		this.signal2 = signal2;
		this.signal3 = signal3;
		this.signal4 = signal4;
		this.signal5 = signal5;
		this.signal6 = signal6;
		this.signal7 = signal7;
		this.signal8 = signal8;
		this.signal9 = signal9;
	}

	public void executeAdd() {
	}

	public void executePart1() {
	}

	public void executePart2() {
	}

	public void execute() {
		signal1.setValue("0");
		signal2.setValue("0");
		signal3.setValue("0");
		signal4.setValue("0");
		signal5.setValue("0");
		signal6.setValue("0");
		signal7.setValue("0");
		signal8.setValue("0");
		signal9.setValue("0");
		int opCode = (int) Data.hex2long(input.getValue());
		switch (opCode) {
			case 0: // '\0'
				signal1.setValue("1");
				signal6.setValue("2");
				signal9.setValue("1");
				break;

			case 2: // '\002'
				signal2.setValue("1");
				break;

			case 4: // '\004'
				signal3.setValue("1");
				signal6.setValue("1");
				break;

			case 35: // '#'
				signal4.setValue("1");
				signal5.setValue("1");
				signal8.setValue("1");
				signal9.setValue("1");
				break;

			case 43: // '+'
				signal7.setValue("1");
				signal8.setValue("1");
				break;
		}
	}
}
