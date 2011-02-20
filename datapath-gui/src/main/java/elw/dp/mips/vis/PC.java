/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import java.awt.*;

class PC extends RegMem {

	private DataLine input;
	private DataLine output;

	public PC(double x1, double y1, double width, double height, Color color, String label1, String label2, String name, DataLine input, DataLine output) {
		super(x1, y1, width, height, color, label1, label2, name);
		this.input = input;
		this.output = output;
	}

	public void executeAdd() {
	}

	public void executePart1() {
	}

	public void executePart2() {
	}

	public void execute() {
		output.setValue(input.getValue());
	}
}
