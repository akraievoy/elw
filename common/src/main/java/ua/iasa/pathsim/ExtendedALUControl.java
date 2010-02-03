/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;
import ua.iasa.pathsim.app.ui.DataPath;

class ExtendedALUControl extends ALUControl {

    private DataLine aluOp, func, aluControl;
    private DataPath parent;

    public ExtendedALUControl(double x, double y, double w, double h, Color c, String label1, String label2, String name, DataLine aluOp, DataLine func,
                              DataLine aluControl, DataPath parent) {
        super(x, y, w, h, c, label1, label2, name, aluOp, func, aluControl);
        this.func = func;
        this.aluOp = aluOp;
        this.aluControl = aluControl;
        this.parent = parent;
    }

    public void execute() {
        int function = (int) Data.hex2long(func.getValue());
        int operation = (int) Data.hex2long(aluOp.getValue());
        super.execute();
    }
}
