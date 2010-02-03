/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;

class AndGate extends Device {

    private double x1, y1, width, height;
    private Color color;
    private String label, name;
    private DataLine input1, input2, output;

    public AndGate(double x, double y, double w, double h, Color c, String label, String name, DataLine input1, DataLine input2, DataLine output) {
        x1 = x;
        y1 = y;
        width = w;
        height = h;
        color = c;
        this.label = label;
        this.name = name;
        this.input1 = input1;
        this.input2 = input2;
        this.output = output;
    }

    public void draw(Graphics g, int w, int h) {
        int x = (int) Math.floor(x1 * (double) w);
        int y = (int) Math.floor(y1 * (double) h);
        int width = (int) Math.floor(this.width * (double) w);
        int height = (int) Math.floor(this.height * (double) h);
        g.setColor(color);
        g.fillOval(x, y, width, height);
        g.fillRect(x, y, width / 2, height);
    }

    public String getName() {
        return name;
    }

    public void executeAdd() {
    }

    public void executePart1() {
    }

    public void executePart2() {
    }

    public void execute() {
        if (input1.getValue() == "1" && input2.getValue() == "1") {
            output.setValue("1");
        } else {
            output.setValue("0");
        }
    }

    public boolean isOver(int w, int h, int x, int y) {
        int x1 = (int) Math.floor(this.x1 * (double) w);
        int y1 = (int) Math.floor(this.y1 * (double) h);
        int x2 = x1 + (int) Math.floor(width * (double) w);
        int y2 = y1 + (int) Math.floor(height * (double) h);
        return x1 <= x && x <= x2 && y1 <= y && y <= y2;
    }
}
