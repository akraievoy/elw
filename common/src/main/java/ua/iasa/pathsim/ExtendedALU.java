/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;
import ua.iasa.pathsim.app.ui.DataPathOld;

class ExtendedALU extends ALU {

    private DataLine aluControl, input1, input2, result;
    private DataLine zero;
    private DataPathOld parent;

    public ExtendedALU(double x[], double y[], int n, Color color, String label, String name, DataLine aluControl,
                       DataLine input1, DataLine input2, DataLine result, DataLine zero, DataPathOld parent) {
        super(x, y, n, color, label, name, aluControl, input1, input2, result, zero);
        this.input1 = input1;
        this.input2 = input2;
        this.aluControl = aluControl;
        this.result = result;
        this.zero = zero;
        this.parent = parent;
    }

    public void execute() {
        int operation = (int) Data.hex2long(aluControl.getValue());
        int signedNumber1 = Data.hex2int(input1.getValue());
        int signedNumber2 = Data.hex2int(input2.getValue());
        super.execute();
    }
}
