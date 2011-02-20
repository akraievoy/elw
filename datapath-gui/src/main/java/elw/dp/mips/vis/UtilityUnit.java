/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import java.awt.*;

abstract class UtilityUnit extends Device {

	private double x1;
	private double y1;
	private double width;
	private double height;
	private Color color;
	private String label1;
	private String label2;
	private String name;

	protected UtilityUnit(double x, double y, double w, double h, Color c, String label1, String label2, String name) {
		x1 = x;
		y1 = y;
		width = w;
		height = h;
		color = c;
		this.label1 = label1;
		this.label2 = label2;
		this.name = name;
	}

	public void draw(Graphics g, int w, int h) {
		int x = (int) Math.floor(x1 * (double) w);
		int y = (int) Math.floor(y1 * (double) h);
		int width = (int) Math.floor(this.width * (double) w);
		int height = (int) Math.floor(this.height * (double) h);
		g.setColor(color);
		g.fillOval(x, y, width, height);
		g.setColor(Color.black);
		g.drawString(label1, x + width / 10, y + height / 2);
		g.drawString(label2, x + width / 10, y + height / 2 + 12);
	}

	public String getName() {
		return name;
	}

	public boolean isOver(int w, int h, int x, int y) {
		int x1 = (int) Math.floor(this.x1 * (double) w);
		int y1 = (int) Math.floor(this.y1 * (double) h);
		int x2 = x1 + (int) Math.floor(width * (double) w);
		int y2 = y1 + (int) Math.floor(height * (double) h);
		return x1 <= x && x <= x2 && y1 <= y && y <= y2;
	}
}
