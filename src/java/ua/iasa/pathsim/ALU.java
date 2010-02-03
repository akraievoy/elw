/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import java.awt.*;

class ALU extends Device {

    private double x[], y[];
    private int numberPoints;
    private Color color;
    private String label, name;
    private DataLine control, input1, input2, result;
    private DataLine zero;

    public ALU(double x[], double y[], int n, Color color, String label, String name, DataLine control,
               DataLine input1, DataLine input2, DataLine result, DataLine zero) {
        this.x = x;
        this.y = y;
        numberPoints = n;
        this.color = color;
        this.label = label;
        this.name = name;
        this.input1 = input1;
        this.input2 = input2;
        this.control = control;
        this.result = result;
        this.zero = zero;
    }

    public void draw(Graphics g, int w, int h) {
        int averageX = 0;
        int averageY = 0;
        int x[] = new int[numberPoints];
        int y[] = new int[numberPoints];
        for (int i = 0; i < numberPoints; i++) {
            x[i] = (int) Math.floor(this.x[i] * (double) w);
            y[i] = (int) Math.floor(this.y[i] * (double) h);
            averageX += x[i];
            averageY += y[i];
        }

        averageX /= numberPoints;
        averageY /= numberPoints;
        g.setColor(color);
        g.fillPolygon(x, y, numberPoints);
        g.setColor(Color.black);
        g.drawString(label, averageX, averageY);
    }

    public String getName() {
        return name;
    }

    public void executePart1() {
    }

    public void executePart2() {
    }

    public void executeAdd() {
        long number1 = Data.hex2long(input1.getValue());
        long number2 = Data.hex2long(input2.getValue());
        result.setValue(Data.long2hex(number1 + number2, 8));
    }

    public void execute() {
        int operation = (int) Data.hex2long(control.getValue());
        int signedNumber1 = Data.hex2int(input1.getValue());
        int signedNumber2 = Data.hex2int(input2.getValue());
        switch (operation) {
            case 3: // '\003'
            case 4: // '\004'
            case 5: // '\005'
            default:
                break;

            case 0: // '\0'
            {
                int anded = signedNumber1 & signedNumber2;
                result.setValue(Data.int2hex(anded, 8));
                if (anded == 0) {
                    zero.setValue("1");
                } else {
                    zero.setValue("0");
                }
                break;
            }

            case 1: // '\001'
            {
                int ored = signedNumber1 | signedNumber2;
                result.setValue(Data.int2hex(ored, 8));
                if (ored == 0) {
                    zero.setValue("1");
                } else {
                    zero.setValue("0");
                }
                break;
            }

            case 2: // '\002'
            {
                int sum = signedNumber1 + signedNumber2;
                result.setValue(Data.int2hex(sum, 8));
                if (sum == 0) {
                    zero.setValue("1");
                } else {
                    zero.setValue("0");
                }
                break;
            }

            case 6: // '\006'
            {
                int difference = signedNumber1 - signedNumber2;
                result.setValue(Data.int2hex(difference, 8));
                if (difference == 0) {
                    zero.setValue("1");
                } else {
                    zero.setValue("0");
                }
                break;
            }

            case 7: // '\007'
            {
                int difference = signedNumber1 - signedNumber2;
                if (difference == 0) {
                    result.setValue("00000000");
                    zero.setValue("1");
                    break;
                }
                if (difference < 0) {
                    result.setValue("00000001");
                    zero.setValue("0");
                } else {
                    result.setValue("00000000");
                    zero.setValue("1");
                }
                break;
            }
        }
    }

    public boolean isOver(int w, int h, int mouseX, int mouseY) {
        int x[] = new int[numberPoints];
        int y[] = new int[numberPoints];
        for (int i = 0; i < numberPoints; i++) {
            x[i] = (int) Math.floor(this.x[i] * (double) w);
            y[i] = (int) Math.floor(this.y[i] * (double) h);
        }

        return x[0] <= mouseX && mouseX <= x[1] && y[0] <= mouseY && mouseY <= y[3];
    }
}
