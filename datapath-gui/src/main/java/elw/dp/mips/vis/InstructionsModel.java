/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import javax.swing.table.AbstractTableModel;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: InstructionsModel.java,v 1.1 2006/12/28 10:38:57 Anton S. Kraievoy Exp $
 */

public interface InstructionsModel {
	int SIZE = 100;
	String[] REGS = {
			"$ZERO",
			"$AT",
			"$V0", "$V1",
			"$A0", "$A1", "$A2", "$A3",
			"$T0", "$T1", "$T2", "$T3", "$T4", "$T5", "$T6", "$T7",
			"$S0", "$S1", "$S2", "$S3", "$S4", "$S5", "$S6", "$S7",
			"$T8", "$T9",
			"$K0", "$K1",
			"$GP",
			"$SP",
			"$FP",
			"$RA"
	};

	void initInstructions();

	void setInstructionAtAddress(int instruction, int address, String code);

	void setInstructions(int[] instructions, String[] newCode);

	int getSize();

	int getValue(int memoryIndex);

	String getCode(int memoryIndex);

	int getAccessIndex();

	AbstractTableModel getTableModel();
}

