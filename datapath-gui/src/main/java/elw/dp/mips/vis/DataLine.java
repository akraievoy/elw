/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;

import java.awt.*;
import java.util.Vector;

public class DataLine {

	protected Vector lines;
	private Color normalColor;
	private Color highLightColor;
	private String value;
	private String name;
	private String label;
	private boolean selected;

	public DataLine(int size, Color c, String name, String label, String value) {
		selected = false;
		lines = new Vector(size);
		normalColor = c;
		highLightColor = Color.red;
		this.name = name;
		this.label = label;
		this.value = value;
	}

	public void addLine(double x1, double y1, double x2, double y2, char orientation) {
		lines.addElement(new Line(x1, y1, x2, y2, orientation));
	}

	public boolean isOver(int w, int h, int x, int y) {
		int numberLines = lines.size();
		boolean found = false;
		int index = 0;
		do {
			Line line = (Line) lines.elementAt(index);
			found = line.isOver(w, h, x, y);
			index++;
		} while (!found && index < numberLines);
		return found;
	}

	public void draw(Graphics g, int w, int h) {
		int numberLines = lines.size();
		if (selected) {
			g.setColor(highLightColor);
		} else {
			g.setColor(normalColor);
		}
		for (int i = 0; i < numberLines; i++) {
			Line line = (Line) lines.elementAt(i);
			line.draw(g, w, h);
		}

	}

	public void execute() {
	}

	public void setSelected() {
		selected = true;
	}

	public void setDeselected() {
		selected = false;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public void setValue(long value) {
		this.value = Data.long2hex(value);
	}

	public void setColor(Color c) {
		normalColor = c;
	}

	public String getName() {
		return name;
	}

	public String getLabel() {
		return label;
	}

	public String getValue() {
		return value;
	}

	public long getValueLong() {
		return Data.hex2long(value);
	}

	public int getValueInt() {
		return Data.hex2int(value);
	}
}
