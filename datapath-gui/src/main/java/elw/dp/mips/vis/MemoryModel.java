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
 * @version $Id: MemoryModel.java,v 1.2 2006/12/28 10:38:57 Anton S. Kraievoy Exp $
 */

public interface MemoryModel {
	int MEM_SIZE = 100;

	void setData(String dataItem, String address);

	void resetData();

	void setData(int[] newValues);

	int getReadIndex();

	int getWriteIndex();

	int getSize();

	int getValue(int memoryIndex);

	AbstractTableModel getTableModel();

	void setDataForIndex(int value, int memoryIndex);
}

