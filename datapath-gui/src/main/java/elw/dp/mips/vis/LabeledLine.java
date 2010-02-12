/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import java.awt.*;

class LabeledLine extends DataLine {

	String label;

	public LabeledLine(int size, Color c, String name, String label, String value) {
		super(size, c, name, label, value);
		this.label = label;
	}

	public void draw(Graphics g, int w, int h) {
		Line line = (Line) lines.elementAt(0);
		int x1 = (int) Math.floor(line.getX1() * (double) w);
		int y1 = (int) Math.floor(line.getY1() * (double) h);
		super.draw(g, w, h);
		g.drawString(label, x1 - 15, y1);
	}
}
