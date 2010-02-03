/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;

public abstract class Device {

    public Device() {
    }

    public abstract void execute();

    public abstract void executeAdd();

    public abstract void executePart1();

    public abstract void executePart2();

    public abstract void draw(Graphics g, int i, int j);

    public abstract String getName();

    public abstract boolean isOver(int i, int j, int k, int l);
}
