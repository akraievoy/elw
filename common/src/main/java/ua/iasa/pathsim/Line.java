/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;

class Line {

    private int epsilon;
    private double x1, y1, x2, y2;
    private char orientation;

    public Line(double x1, double y1, double x2, double y2, char orientation) {
        epsilon = 4;
        if (x1 <= x2) {
            this.x1 = x1;
            this.y1 = y1;
            this.x2 = x2;
            this.y2 = y2;
        } else {
            this.x1 = x2;
            this.y1 = y2;
            this.x2 = x1;
            this.y2 = y1;
        }
        this.orientation = orientation;
    }

    public boolean isOver(int w, int h, int x, int y) {
        int x1 = (int) Math.floor(this.x1 * (double) w);
        int y1 = (int) Math.floor(this.y1 * (double) h);
        int x2 = (int) Math.floor(this.x2 * (double) w);
        int y2 = (int) Math.floor(this.y2 * (double) h);
        int smallerY;
        int largerY;
        if (orientation == 'h') {
            smallerY = y1 - epsilon;
            largerY = y1 + epsilon;
        } else {
            if (y1 <= y2) {
                smallerY = y1;
                largerY = y2;
            } else {
                smallerY = y2;
                largerY = y1;
            }
            x1 -= epsilon;
            x2 += epsilon;
        }
        return x1 <= x && x <= x2 && smallerY <= y && y <= largerY;
    }

    public double getX1() {
        return x1;
    }

    public double getY1() {
        return y1;
    }

    public void draw(Graphics g, int w, int h) {
        int x1 = (int) Math.floor(this.x1 * (double) w);
        int y1 = (int) Math.floor(this.y1 * (double) h);
        int x2 = (int) Math.floor(this.x2 * (double) w);
        int y2 = (int) Math.floor(this.y2 * (double) h);
        g.drawLine(x1, y1, x2, y2);
    }
}
