/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;


import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Vector;

import static elw.dp.mips.asm.Data.hex2int;

public class DataPathPanel extends JPanel
		implements MouseListener, MouseMotionListener {

	private int x1, y1, x2, y2;
	private Vector dataLines, devices;
	private InfoBox floatingInfoBox;
	private int selectedDataLine;
	private boolean overDataLine, overDevice, paintFloatingBox;

	public DataPathPanel() {
		setBackground(Color.lightGray);
		setForeground(Color.black);
		setPreferredSize(new Dimension(240, 120));
		defineDiagram();
		addMouseListener(this);
	}

	private void defineDiagram() {
		double w = 24D;
		double h = 15D;
		dataLines = new Vector(4);
		DataLine temp = new LabeledLine(1, Color.black, "4", "4", "00000004");
		temp.addLine(2.6000000000000001D / w, 4.7999999999999998D / h, 3.3999999999999999D / w, 4.7999999999999998D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(3, Color.black, "pcOut", "Read Address", "");
		temp.addLine(1.1000000000000001D / w, 8.4000000000000004D / h, 1.8999999999999999D / w, 8.4000000000000004D / h, 'h');
		temp.addLine(1.3D / w, 8.4000000000000004D / h, 1.3D / w, 3.2000000000000002D / h, 'v');
		temp.addLine(1.3D / w, 3.2000000000000002D / h, 3.3999999999999999D / w, 3.2000000000000002D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(5, Color.black, "pcIn", "", "00000000");
		temp.addLine(0.80000000000000004D / w, 8.4000000000000004D / h, 0.5D / w, 8.4000000000000004D / h, 'h');
		temp.addLine(0.5D / w, 8.4000000000000004D / h, 0.5D / w, 0.69999999999999996D / h, 'v');
		temp.addLine(0.5D / w, 0.69999999999999996D / h, 22.300000000000001D / w, 0.69999999999999996D / h, 'h');
		temp.addLine(22.300000000000001D / w, 0.69999999999999996D / h, 22.300000000000001D / w, 2.3999999999999999D / h, 'v');
		temp.addLine(22.300000000000001D / w, 2.3999999999999999D / h, 21.600000000000001D / w, 2.3999999999999999D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(4, Color.black, "pc+4", "", "");
		temp.addLine(4.2000000000000002D / w, 3.6000000000000001D / h, 13.699999999999999D / w, 3.6000000000000001D / h, 'h');
		temp.addLine(13.699999999999999D / w, 3.6000000000000001D / h, 13.699999999999999D / w, 1.8999999999999999D / h, 'v');
		temp.addLine(13.699999999999999D / w, 1.8999999999999999D / h, 19.600000000000001D / w, 1.8999999999999999D / h, 'h');
		temp.addLine(13.699999999999999D / w, 2.5D / h, 16D / w, 2.5D / h, 'h');
		dataLines.addElement(temp);
		temp = new SingleSourceDataLine(3, Color.black, "pc+4[31-28]", "pc+4[31-28]", "", findDataLine("pc+4"), "F0000000", 0, 0, 0);
		temp.addLine(6.7000000000000002D / w, 3.6000000000000001D / h, 6.7000000000000002D / w, 2.5D / h, 'v');
		temp.addLine(6.7000000000000002D / w, 2.5D / h, 10D / w, 2.5D / h, 'h');
		temp.addLine(10D / w, 2.5D / h, 10D / w, 1.5D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(1, Color.black, "shiftLeftAOut", "", "");
		temp.addLine(8.6999999999999993D / w, 1.5D / h, 10D / w, 1.5D / h, 'h');
		dataLines.addElement(temp);
		temp = new DualSourceDataLine(2, Color.black, "jumpAddress[31-0]", "jumpAddress[31-0]", "", findDataLine("pc+4[31-28]"), findDataLine("shiftLeftAOut"), "", 0, 7);
		temp.addLine(10D / w, 1.5D / h, 20.5D / w, 1.5D / h, 'h');
		temp.addLine(20.5D / w, 1.5D / h, 20.5D / w, 1.8999999999999999D / h, 'v');
		temp.addLine(20.5D / w, 1.8999999999999999D / h, 21.399999999999999D / w, 1.8999999999999999D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(3, Color.black, "aluAddressOut", "", "");
		temp.addLine(17.5D / w, 3.5D / h, 18.5D / w, 3.5D / h, 'h');
		temp.addLine(18.5D / w, 3.5D / h, 18.5D / w, 3D / h, 'v');
		temp.addLine(18.5D / w, 3D / h, 19.600000000000001D / w, 3D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(1, Color.black, "mux1ToMux2", "", "");
		temp.addLine(19.800000000000001D / w, 3D / h, 21.399999999999999D / w, 3D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(3, Color.black, "instructionOut", "instruction[31-0]", "");
		temp.addLine(4.2999999999999998D / w, 9.3000000000000007D / h, 5.5D / w, 9.3000000000000007D / h, 'h');
		temp.addLine(4.5999999999999996D / w, 9.3000000000000007D / h, 4.5999999999999996D / w, 1.5D / h, 'v');
		temp.addLine(5.5D / w, 12.199999999999999D / h, 5.5D / w, 5.7000000000000002D / h, 'v');
		dataLines.addElement(temp);
		temp = new SingleSourceDataLine(1, Color.black, "instruction[25-0]", "instruction[25-0]", "", findDataLine("instructionOut"), "03FFFFFF", 1, 7, 0);
		temp.addLine(4.5999999999999996D / w, 1.5D / h, 8.1999999999999993D / w, 1.5D / h, 'h');
		dataLines.addElement(temp);

		temp = new SingleSourceDataLine(1, Color.black, "instruction[31-26]", "instruction[31-26]", "", findDataLine("instructionOut"), "FC000000", 0, 1, 2);

		temp.addLine(5.5D / w, 5.7000000000000002D / h, 8.9000000000000004D / w, 5.7000000000000002D / h, 'h');
		dataLines.addElement(temp);

		temp = new SingleSourceDataLine(1, Color.black, "instruction[25-21]", "instruction[25-21]", "", findDataLine("instructionOut"), "03E00000", 1, 2, 1);

		temp.addLine(5.5D / w, 8.1999999999999993D / h, 10.300000000000001D / w, 8.1999999999999993D / h, 'h');
		dataLines.addElement(temp);

		temp = new SingleSourceDataLine(3, Color.black, "instruction[20-16]", "instruction[20-16]", "", findDataLine("instructionOut"), "001F0000", 2, 3, 0);

		temp.addLine(5.5D / w, 8.9000000000000004D / h, 10.300000000000001D / w, 8.9000000000000004D / h, 'h');

		temp.addLine(7.9000000000000004D / w, 8.9000000000000004D / h, 7.9000000000000004D / w, 9.5D / h, 'v');
		temp.addLine(7.9000000000000004D / w, 9.5D / h, 9D / w, 9.5D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(1, Color.black, "mux3Out", "Write Register", "");

		temp.addLine(9.1999999999999993D / w, 9.9000000000000004D / h, 10.300000000000001D / w, 9.9000000000000004D / h, 'h');
		dataLines.addElement(temp);

		temp = new SingleSourceDataLine(1, Color.black, "instruction[15-11]", "instruction[15-11]", "", findDataLine("instructionOut"), "0000F800", 4, 5, 3);
		temp.addLine(5.5D / w, 10.4D / h, 9D / w, 10.4D / h, 'h');
		dataLines.addElement(temp);

		temp = new SingleSourceDataLine(1, Color.black, "instruction[15-0]", "instruction[15-0]", "", findDataLine("instructionOut"), "0000FFFF", 4, 7, 0);
		temp.addLine(5.5D / w, 12.1D / h, 12.300000000000001D / w, 12.1D / h, 'h');
		dataLines.addElement(temp);

		temp = new SingleSourceDataLine(4, Color.black, "instruction[5-0]", "instruction[5-0]", "", findDataLine("instructionOut"), "0000003F", 6, 7, 0);
		temp.addLine(11D / w, 12.1D / h, 11D / w, 13.5D / h, 'v');
		temp.addLine(11D / w, 13.5D / h, 14.5D / w, 13.5D / h, 'h');
		temp.addLine(14.5D / w, 13.5D / h, 14.5D / w, 12.5D / h, 'v');
		temp.addLine(14.5D / w, 12.5D / h, 15D / w, 12.5D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(2, Color.black, "aluControl", "ua.iasa.ALUControl", "");
		temp.addLine(15.800000000000001D / w, 12.5D / h, 16.199999999999999D / w, 12.5D / h, 'h');
		temp.addLine(16.199999999999999D / w, 12.5D / h, 16.199999999999999D / w, 10.5D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(1, Color.black, "registerData1", "Read Data1", "");

		temp.addLine(12.699999999999999D / w, 8.1999999999999993D / h, 16D / w, 8.1999999999999993D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(3, Color.black, "registerData2", "Read Data2", "");

		temp.addLine(12.699999999999999D / w, 9.5999999999999996D / h, 14.9D / w, 9.5999999999999996D / h, 'h');

		temp.addLine(13.199999999999999D / w, 9.5999999999999996D / h, 13.199999999999999D / w, 11.300000000000001D / h, 'v');

		temp.addLine(13.199999999999999D / w, 11.300000000000001D / h, 18.800000000000001D / w, 11.300000000000001D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(4, Color.black, "signExtendOut", "", "");
		temp.addLine(13D / w, 12.1D / h, 14D / w, 12.1D / h, 'h');
		temp.addLine(14D / w, 12.1D / h, 14D / w, 4.4000000000000004D / h, 'v');

		temp.addLine(14D / w, 4.4000000000000004D / h, 14.300000000000001D / w, 4.4000000000000004D / h, 'h');
		temp.addLine(14D / w, 10.5D / h, 14.9D / w, 10.5D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(1, Color.black, "mux4Out", "", "");
		temp.addLine(15.1D / w, 10D / h, 16D / w, 10D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(5, Color.black, "aluResult", "ua.iasa.ALU Result", "");
		temp.addLine(17.5D / w, 9.5D / h, 18.800000000000001D / w, 9.5D / h, 'h');

		temp.addLine(18.199999999999999D / w, 9.5D / h, 18.199999999999999D / w, 12.199999999999999D / h, 'v');

		temp.addLine(18.199999999999999D / w, 12.199999999999999D / h, 21.800000000000001D / w, 12.199999999999999D / h, 'h');

		temp.addLine(21.800000000000001D / w, 12.199999999999999D / h, 21.800000000000001D / w, 10.6D / h, 'v');
		temp.addLine(21.800000000000001D / w, 10.6D / h, 22.5D / w, 10.6D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(3, Color.black, "aluZero", "Zero", "0");
		temp.addLine(17.5D / w, 9D / h, 17.800000000000001D / w, 9D / h, 'h');
		temp.addLine(17.800000000000001D / w, 9D / h, 17.800000000000001D / w, 5D / h, 'v');
		temp.addLine(17.800000000000001D / w, 5D / h, 18.199999999999999D / w, 5D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(2, Color.black, "andOut", "", "");
		temp.addLine(18.899999999999999D / w, 4.5D / h, 19.699999999999999D / w, 4.5D / h, 'h');

		temp.addLine(19.699999999999999D / w, 4.5D / h, 19.699999999999999D / w, 3.2999999999999998D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(1, Color.black, "memoryReadData", "Read Data", "");

		temp.addLine(21.199999999999999D / w, 9.8000000000000007D / h, 22.5D / w, 9.8000000000000007D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(5, Color.black, "registerWriteData", "Write Data", "");
		temp.addLine(22.5D / w, 10.199999999999999D / h, 23D / w, 10.199999999999999D / h, 'h');
		temp.addLine(23D / w, 10.199999999999999D / h, 23D / w, 14.199999999999999D / h, 'v');

		temp.addLine(23D / w, 14.199999999999999D / h, 9.8000000000000007D / w, 14.199999999999999D / h, 'h');

		temp.addLine(9.8000000000000007D / w, 14.199999999999999D / h, 9.8000000000000007D / w, 10.6D / h, 'v');
		temp.addLine(9.8000000000000007D / w, 10.6D / h, 10.300000000000001D / w, 10.6D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(1, Color.black, "shiftLeftBOut", "", "");

		temp.addLine(14.800000000000001D / w, 4.4000000000000004D / h, 16D / w, 4.4000000000000004D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(5, Color.green, "controlRegDst", "RegDst", "");
		temp.addLine(9.5D / w, 3.8999999999999999D / h, 9.5D / w, 3.7000000000000002D / h, 'v');
		temp.addLine(9.5D / w, 3.7000000000000002D / h, 7D / w, 3.7000000000000002D / h, 'h');
		temp.addLine(7D / w, 3.7000000000000002D / h, 7D / w, 11D / h, 'v');
		temp.addLine(7D / w, 11D / h, 9.0500000000000007D / w, 11D / h, 'h');

		temp.addLine(9.0500000000000007D / w, 11D / h, 9.0500000000000007D / w, 10.699999999999999D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(4, Color.green, "controlJump", "Jump", "");

		temp.addLine(9.6999999999999993D / w, 4.0999999999999996D / h, 11.5D / w, 4.0999999999999996D / h, 'h');
		temp.addLine(11.5D / w, 4.0999999999999996D / h, 11.5D / w, 1.1000000000000001D / h, 'v');
		temp.addLine(11.5D / w, 1.1000000000000001D / h, 21.5D / w, 1.1000000000000001D / h, 'h');
		temp.addLine(21.5D / w, 1.1000000000000001D / h, 21.5D / w, 1.7D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(3, Color.green, "controlBranch", "Branch", "");

		temp.addLine(10D / w, 5.2000000000000002D / h, 17.600000000000001D / w, 5.2000000000000002D / h, 'h');

		temp.addLine(17.600000000000001D / w, 5.2000000000000002D / h, 17.600000000000001D / w, 4.2999999999999998D / h, 'v');

		temp.addLine(17.600000000000001D / w, 4.2999999999999998D / h, 18.199999999999999D / w, 4.2999999999999998D / h, 'h');
		dataLines.addElement(temp);
		temp = new DataLine(4, Color.green, "controlMemRead", "MemRead", "");

		temp.addLine(10.1D / w, 5.4000000000000004D / h, 23.399999999999999D / w, 5.4000000000000004D / h, 'h');

		temp.addLine(23.399999999999999D / w, 5.4000000000000004D / h, 23.399999999999999D / w, 12.699999999999999D / h, 'v');

		temp.addLine(23.399999999999999D / w, 12.699999999999999D / h, 20D / w, 12.699999999999999D / h, 'h');
		temp.addLine(20D / w, 12.699999999999999D / h, 20D / w, 11.699999999999999D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(2, Color.green, "controlMemToReg", "MemToReg", "");

		temp.addLine(10.1D / w, 5.7999999999999998D / h, 22.600000000000001D / w, 5.7999999999999998D / h, 'h');

		temp.addLine(22.600000000000001D / w, 5.7999999999999998D / h, 22.600000000000001D / w, 9.4000000000000004D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(4, Color.green, "controlALUOp", "ALUOp", "");
		temp.addLine(10D / w, 6.2000000000000002D / h, 13.6D / w, 6.2000000000000002D / h, 'h');
		temp.addLine(13.6D / w, 6.2000000000000002D / h, 13.6D / w, 14.6D / h, 'v');
		temp.addLine(13.6D / w, 14.6D / h, 15.4D / w, 14.6D / h, 'h');
		temp.addLine(15.4D / w, 14.6D / h, 15.4D / w, 13.199999999999999D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(2, Color.green, "controlMemWrite", "MemWrite", "");

		temp.addLine(9.9000000000000004D / w, 6.5999999999999996D / h, 19.899999999999999D / w, 6.5999999999999996D / h, 'h');

		temp.addLine(19.899999999999999D / w, 6.5999999999999996D / h, 19.899999999999999D / w, 8.5999999999999996D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(2, Color.green, "controlALUSrc", "ALUSrc", "");
		temp.addLine(9.8000000000000007D / w, 7D / h, 15D / w, 7D / h, 'h');
		temp.addLine(15D / w, 7D / h, 15D / w, 9.3000000000000007D / h, 'v');
		dataLines.addElement(temp);
		temp = new DataLine(2, Color.green, "controlRegWrite", "RegWrite", "");

		temp.addLine(9.6999999999999993D / w, 7.4000000000000004D / h, 11.5D / w, 7.4000000000000004D / h, 'h');
		temp.addLine(11.5D / w, 7.4000000000000004D / h, 11.5D / w, 7.7999999999999998D / h, 'v');
		dataLines.addElement(temp);
		devices = new Vector(4);

		devices.addElement(new PC(0.80000000000000004D / w, 7.7000000000000002D / h, 0.25D / w, 1.3999999999999999D / h, Color.cyan, "P", "C", "pc", findDataLine("pcIn"), findDataLine("pcOut")));

		devices.addElement(new InstructionMemory(1.8999999999999999D / w, 7.7999999999999998D / h, 2.3999999999999999D / w, 3.1000000000000001D / h, Color.cyan, "Instruction", "Memory", "instructionMemory", findDataLine("pcOut"), findDataLine("instructionOut"), this));

		devices.addElement(new Registers(10.300000000000001D / w, 7.7999999999999998D / h, 2.3999999999999999D / w, 3.1000000000000001D / h, Color.cyan, "Registers", "", "registers", findDataLine("controlRegWrite"), findDataLine("instruction[25-21]"), findDataLine("instruction[20-16]"), findDataLine("mux3Out"), findDataLine("registerWriteData"), findDataLine("registerData1"), findDataLine("registerData2"), this));

		devices.addElement(new DataMemory(18.800000000000001D / w, 8.5999999999999996D / h, 2.3999999999999999D / w, 3.1000000000000001D / h, Color.cyan, "Data", "Memory", "dataMemory", MemoryModel.MEM_SIZE, findDataLine("controlMemRead"), findDataLine("controlMemWrite"), findDataLine("aluResult"), findDataLine("registerData2"), findDataLine("memoryReadData")));

		devices.addElement(new ShiftLeft2(8.1999999999999993D / w, 0.90000000000000002D / h, 0.59999999999999998D / w, 1.3999999999999999D / h, Color.magenta, "Shift", "Left", "shiftLeftA", findDataLine("instruction[25-0]"), findDataLine("shiftLeftAOut"), 7));

		devices.addElement(new ShiftLeft2(14.300000000000001D / w, 3.6000000000000001D / h, 0.59999999999999998D / w, 1.3999999999999999D / h, Color.magenta, "Shift", "Left", "shiftLeftB", findDataLine("signExtendOut"), findDataLine("shiftLeftBOut"), 8));

		devices.addElement(new ControlUnit(8.9000000000000004D / w, 3.8999999999999999D / h, 1.2D / w, 3.6000000000000001D / h, Color.red, "Control", "", "control", findDataLine("instruction[31-26]"), findDataLine("controlRegDst"), findDataLine("controlJump"), findDataLine("controlBranch"), findDataLine("controlMemRead"), findDataLine("controlMemToReg"), findDataLine("controlALUOp"), findDataLine("controlMemWrite"), findDataLine("controlALUSrc"), findDataLine("controlRegWrite")));

		devices.addElement(new SignExtend(12.300000000000001D / w, 11.1D / h, 0.80000000000000004D / w, 1.8D / h, Color.magenta, "Sign", "Ext", "signExtend", findDataLine("instruction[15-0]"), findDataLine("signExtendOut")));

		devices.addElement(new ExtendedALUControl(15D / w, 11.5D / h, 0.80000000000000004D / w, 1.8D / h, Color.magenta, "Alu", "Cont", "aluControl", findDataLine("controlALUOp"), findDataLine("instruction[5-0]"), findDataLine("aluControl")));

		devices.addElement(new AndGate(18.199999999999999D / w, 4D / h, 0.80000000000000004D / w, 1.2D / h, Color.blue, "And Gate", "andGate", findDataLine("controlBranch"), findDataLine("aluZero"), findDataLine("andOut")));

		devices.addElement(new Mux(19.600000000000001D / w, 1.7D / h, 0.20000000000000001D / w, 1.6000000000000001D / h, 0.14999999999999999D / w, 0.14999999999999999D / h, Color.orange, "0", "1", "mux1", findDataLine("andOut"), findDataLine("pc+4"), findDataLine("aluAddressOut"), findDataLine("mux1ToMux2")));

		devices.addElement(new Mux(21.399999999999999D / w, 1.7D / h, 0.20000000000000001D / w, 1.6000000000000001D / h, 0.14999999999999999D / w, 0.14999999999999999D / h, Color.orange, "1", "0", "mux2", findDataLine("controlJump"), findDataLine("mux1ToMux2"), findDataLine("jumpAddress[31-0]"), findDataLine("pcIn")));

		devices.addElement(new Mux(9D / w, 9.0999999999999996D / h, 0.20000000000000001D / w, 1.6000000000000001D / h, 0.14999999999999999D / w, 0.14999999999999999D / h, Color.orange, "0", "1", "mux3", findDataLine("controlRegDst"), findDataLine("instruction[20-16]"), findDataLine("instruction[15-11]"), findDataLine("mux3Out")));

		devices.addElement(new Mux(14.9D / w, 9.3000000000000007D / h, 0.20000000000000001D / w, 1.6000000000000001D / h, 0.14999999999999999D / w, 0.14999999999999999D / h, Color.orange, "0", "1", "mux4", findDataLine("controlALUSrc"), findDataLine("registerData2"), findDataLine("signExtendOut"), findDataLine("mux4Out")));

		devices.addElement(new Mux(22.5D / w, 9.4000000000000004D / h, 0.20000000000000001D / w, 1.6000000000000001D / h, 0.14999999999999999D / w, 0.14999999999999999D / h, Color.orange, "1", "0", "mux5", findDataLine("controlMemToReg"), findDataLine("aluResult"), findDataLine("memoryReadData"), findDataLine("registerWriteData")));
		double x1[] = {
				3.3999999999999999D / w, 4.2000000000000002D / w, 4.2000000000000002D / w, 3.3999999999999999D / w, 3.3999999999999999D / w, 3.6000000000000001D / w, 3.3999999999999999D / w
		};
		double y1[] = {
				2.6000000000000001D / h, 3.5D / h, 4.5999999999999996D / h, 5.4000000000000004D / h, 4.2999999999999998D / h, 4D / h, 3.7000000000000002D / h
		};

		devices.addElement(new ALU(x1, y1, 7, Color.green, "Add", "aluPC", null, findDataLine("pcOut"), findDataLine("4"), findDataLine("pc+4"), null));
		double x2[] = {
				16D / w, 17.5D / w, 17.5D / w, 16D / w, 16D / w, 16.199999999999999D / w, 16D / w
		};
		double y2[] = {
				2D / h, 3D / h, 4D / h, 5D / h, 3.7999999999999998D / h, 3.5D / h, 3.2000000000000002D / h
		};

		devices.addElement(new ALU(x2, y2, 7, Color.green, "Add", "aluAddress", null, findDataLine("pc+4"), findDataLine("shiftLeftBOut"), findDataLine("aluAddressOut"), null));
		double x3[] = {
				16D / w, 17.5D / w, 17.5D / w, 16D / w, 16D / w, 16.199999999999999D / w, 16D / w
		};
		double y3[] = {
				7.7000000000000002D / h, 8.6999999999999993D / h, 9.6999999999999993D / h, 10.699999999999999D / h, 9.5D / h, 9.1999999999999993D / h, 8.9000000000000004D / h
		};

		devices.addElement(new ExtendedALU(x3, y3, 7, Color.green, "ua.iasa.ALU", "aluGeneral", findDataLine("aluControl"), findDataLine("registerData1"), findDataLine("mux4Out"), findDataLine("aluResult"), findDataLine("aluZero")));
		floatingInfoBox = new InfoBox(5, 5, 35, Color.white);
	}

	private DataLine findDataLine(String name) {
		int dataLineIndex = 0;
		boolean found = false;
		if (name.length() > 0) {
			do {
				DataLine dataLine = (DataLine) dataLines.elementAt(dataLineIndex);
				found = name == dataLine.getName();
				if (found || dataLineIndex >= dataLines.size()) {
					break;
				}
				dataLineIndex++;
			} while (true);
			if (found) {
				return (DataLine) dataLines.elementAt(dataLineIndex);
			} else {
				return null;
			}
		} else {
			return null;
		}
	}

	public Device findDevice(String name) {
		int deviceIndex = 0;
		boolean found = false;
		do {
			Device device = (Device) devices.elementAt(deviceIndex);
			found = device.getName().equals(name);
			if (found || deviceIndex >= devices.size()) {
				break;
			}
			deviceIndex++;
		} while (true);
		if (found) {
			return (Device) devices.elementAt(deviceIndex);
		} else {
			return null;
		}
	}

	public void initInstructions() {
		InstructionMemory instructionMemory = (InstructionMemory) findDevice("instructionMemory");
		instructionMemory.initInstructions();
	}

	public void setInstruction(String instruction, String address, String code) {
		InstructionMemory instructionMemory = (InstructionMemory) findDevice("instructionMemory");
		instructionMemory.setInstructionAtAddress(hex2int(instruction), hex2int(address), code);
	}

	public void resetDataPath() {
		DataLine dataLine;
		for (int i = 0; i < dataLines.size(); i++) {
			dataLine = (DataLine) dataLines.get(i);
			dataLine.setValue("");
		}

		dataLine = findDataLine("pcIn");
		dataLine.setValue("00000000");
		dataLine = findDataLine("4");
		dataLine.setValue("00000004");
	}

	public void step() {
		executeOneInstruction();
	}

	public void executeOneInstruction() {
		findDevice("pc").execute();
		findDevice("aluPC").executeAdd();
		findDevice("instructionMemory").execute();
		findDataLine("instruction[25-0]").execute();
		findDataLine("pc+4[31-28]").execute();
		findDataLine("instruction[31-26]").execute();
		findDataLine("instruction[25-21]").execute();
		findDataLine("instruction[20-16]").execute();
		findDataLine("instruction[15-11]").execute();
		findDataLine("instruction[15-0]").execute();
		findDataLine("instruction[5-0]").execute();
		findDevice("shiftLeftA").execute();
		findDataLine("jumpAddress[31-0]").execute();
		findDevice("signExtend").execute();
		findDevice("shiftLeftB").execute();
		findDevice("aluAddress").executeAdd();
		findDevice("control").execute();
		findDevice("mux3").execute();
		findDevice("registers").executePart1();
		findDevice("mux4").execute();
		findDevice("aluControl").execute();
		findDevice("aluGeneral").execute();
		findDevice("andGate").execute();
		findDevice("mux1").execute();
		findDevice("mux2").execute();
		findDevice("dataMemory").execute();
		findDevice("mux5").execute();
		findDevice("registers").executePart2();
	}

	public void mousePressed(MouseEvent e) {
		e.consume();
		int width = getWidth();
		int height = getHeight();
		int X = e.getX();
		int Y = e.getY();
		int numberDataLines = dataLines.size();
		int numberDevices = devices.size();
		int indexDataLine = 0;
		int indexDevice = 0;
		int length = 0;
		overDataLine = false;
		overDevice = false;
		paintFloatingBox = false;
		do {
			DataLine dataLine = (DataLine) dataLines.elementAt(indexDataLine);
			overDataLine = dataLine.isOver(width, height, X, Y);
			if (overDataLine) {
				String name = dataLine.getName();
				String label = dataLine.getLabel();
				String value = dataLine.getValue();
				if (label.length() > value.length()) {
					length = label.length();
				} else {
					length = value.length();
				}
				floatingInfoBox.setWidth(length * 10);
				floatingInfoBox.setInitPoint(X, Y);
				floatingInfoBox.setLine1(label);
				floatingInfoBox.setLine2(value);
				dataLine.setSelected();
				selectedDataLine = indexDataLine;
			}
			indexDataLine++;
		} while (!overDataLine && indexDataLine < numberDataLines);
		do {
			Device device = (Device) devices.elementAt(indexDevice);
			String name = device.getName();
			if (name == "pc") {
				overDevice = device.isOver(width, height, X, Y);
				if (overDevice) {
					floatingInfoBox.setWidth(120);
					floatingInfoBox.setInitPoint(X, Y);
					floatingInfoBox.setLine1("");
					floatingInfoBox.setLine2("Execute Next");
					executeOneInstruction();
				}
			}
			indexDevice++;
		} while (!overDevice && indexDevice < numberDevices);
		if (overDataLine || overDevice) {
			paintFloatingBox = true;
			repaint();
		}
	}

	public void mouseReleased(MouseEvent e) {
		if (overDataLine) {
			DataLine dataLine = (DataLine) dataLines.elementAt(selectedDataLine);
			dataLine.setDeselected();
			repaint();
			paintFloatingBox = false;
			overDataLine = false;
		} else if (overDevice) {
			repaint();
			paintFloatingBox = false;
			overDevice = false;
		}
	}

	public void mouseDragged(MouseEvent mouseevent) {
	}

	public void mouseMoved(MouseEvent mouseevent) {
	}

	public void mouseEntered(MouseEvent mouseevent) {
	}

	public void mouseExited(MouseEvent mouseevent) {
	}

	public void mouseClicked(MouseEvent mouseevent) {
	}

	public void paint(Graphics g) {
		int width = getWidth();
		int height = getHeight();
		int maxIterations = 10000;
		super.paint(g);
		for (int i = 0; i < dataLines.size(); i++) {
			DataLine dataLine = (DataLine) dataLines.elementAt(i);
			dataLine.draw(g, width, height);
		}

		for (int i = 0; i < devices.size(); i++) {
			Device device = (Device) devices.elementAt(i);
			device.draw(g, width, height);
		}

		if (paintFloatingBox) {
			floatingInfoBox.draw(g);
		}
	}
}
