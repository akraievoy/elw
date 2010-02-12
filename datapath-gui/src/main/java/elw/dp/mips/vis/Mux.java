/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import java.awt.*;

class Mux extends Device {

    private double x1, y1, width, height;
    private double arcWidth, arcHeight;
    private Color color;
    private String label1, label2, name;
    private DataLine input0, input1, select, output;

    public Mux(double x, double y, double w, double h, double aW, double aH, Color c, String label1,
               String label2, String name, DataLine select, DataLine input0, DataLine input1, DataLine output) {
        x1 = x;
        y1 = y;
        width = w;
        height = h;
        arcWidth = aW;
        arcHeight = aH;
        color = c;
        this.label1 = label1;
        this.label2 = label2;
        this.name = name;
        this.input0 = input0;
        this.input1 = input1;
        this.select = select;
        this.output = output;
    }

    public String getName() {
        return name;
    }

    public void draw(Graphics g, int w, int h) {
        int x = (int) Math.floor(x1 * (double) w);
        int y = (int) Math.floor(y1 * (double) h);
        int width = (int) Math.floor(this.width * (double) w);
        int height = (int) Math.floor(this.height * (double) h);
        int arcWidth = (int) Math.floor(this.arcWidth * (double) w);
        int arcHeight = (int) Math.floor(this.arcHeight * (double) h);
        g.setColor(color);
        g.fillRoundRect(x, y, width, height, arcWidth, arcHeight);
        g.setColor(Color.black);
        g.drawString(label1, x + 2, y + 12);
        g.drawString(label2, x + 2, (y + height) - 2);
    }

    public boolean isOver(int w, int h, int x, int y) {
        int x1 = (int) Math.floor(this.x1 * (double) w);
        int y1 = (int) Math.floor(this.y1 * (double) h);
        int x2 = x1 + (int) Math.floor(width * (double) w);
        int y2 = y1 + (int) Math.floor(height * (double) h);
        return x1 <= x && x <= x2 && y1 <= y && y <= y2;
    }

    public void executeAdd() {
    }

    public void executePart1() {
    }

    public void executePart2() {
    }

    public void execute() {
        if (select.getValue() == "0") {
            output.setValue(input0.getValue());
        } else {
            output.setValue(input1.getValue());
        }
    }
}
