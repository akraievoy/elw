/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;

class ShiftLeft2 extends UtilityUnit {

    private DataLine input, output;
    private int numberOfDigits;

    public ShiftLeft2(double x, double y, double w, double h, Color c, String label1, String label2, String name, DataLine input, DataLine output,
                      int numberOfDigits) {
        super(x, y, w, h, c, label1, label2, name);
        this.input = input;
        this.output = output;
        this.numberOfDigits = numberOfDigits;
    }

    public void executeAdd() {
    }

    public void executePart1() {
    }

    public void executePart2() {
    }

    public void execute() {
        long inputValue = Data.hex2long(input.getValue());
        inputValue *= 4L;
        output.setValue(Data.long2hex(inputValue, numberOfDigits));
    }
}
