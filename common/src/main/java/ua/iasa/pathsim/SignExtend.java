/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;

class SignExtend extends UtilityUnit {

    private DataLine input, output;

    public SignExtend(double x, double y, double w, double h, Color c, String label1, String label2, String name, DataLine input, DataLine output) {
        super(x, y, w, h, c, label1, label2, name);
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
        String inValue = input.getValue();
        char leadingDigit = inValue.charAt(0);
        String outValue;
        if ('0' <= leadingDigit && leadingDigit <= '7') {
            outValue = "0000" + inValue;
        } else {
            outValue = "FFFF" + inValue;
        }
        output.setValue(outValue);
    }
}
