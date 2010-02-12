/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;

import java.awt.*;

class ALUControl extends UtilityUnit {

    private DataLine input, aluOp, output;

    public ALUControl(double x, double y, double w, double h, Color c, String label1, String label2, String name, DataLine aluOp, DataLine input,
                      DataLine output) {
        super(x, y, w, h, c, label1, label2, name);
        this.input = input;
        this.aluOp = aluOp;
        this.output = output;
    }

    public void executeAdd() {
    }

    public void executePart1() {
    }

    public void executePart2() {
    }

    public void execute() {
        int function = (int) Data.hex2long(input.getValue());
        int operation = (int) Data.hex2long(aluOp.getValue());
        switch (operation) {
            default:
                break;

            case 0: // '\0'
                output.setValue("2");
                break;

            case 1: // '\001'
                output.setValue("6");
                break;

            case 2: // '\002'
                switch (function) {
                    case 32: // ' '
                        output.setValue("2");
                        break;

                    case 34: // '"'
                        output.setValue("6");
                        break;

                    case 36: // '$'
                        output.setValue("0");
                        break;

                    case 37: // '%'
                        output.setValue("1");
                        break;

                    case 42: // '*'
                        output.setValue("7");
                        break;
                }
                break;
        }
    }
}
