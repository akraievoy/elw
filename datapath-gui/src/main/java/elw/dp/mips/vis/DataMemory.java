/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;

import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.util.Arrays;

import static elw.dp.mips.asm.Data.hex2int;
import static elw.dp.mips.asm.Data.hex2long;

class DataMemory extends RegMem implements MemoryModel {
	private DataLine memReadControl, memWriteControl, address, writeData, readData;
	private int memory[];

	protected int readIndex;
	protected int writeIndex;
	protected MemoryTableModel memoryTableModel;

	public DataMemory(double x1, double y1, double width, double height, Color color, String label1, String label2, String name, int size, DataLine memReadControl,
					  DataLine memWriteControl, DataLine address, DataLine writeData, DataLine readData) {
		super(x1, y1, width, height, color, label1, label2, name);
		this.address = address;
		this.writeData = writeData;
		this.memReadControl = memReadControl;
		this.memWriteControl = memWriteControl;
		this.readData = readData;
		memory = new int[size];
		Arrays.fill(memory, 0);
	}

	public void setData(String dataItem, String address) {
		long addressValue = hex2long(address);
		int memoryIndex = (int) addressValue / 4;
		int value = hex2int(dataItem);
		setDataForIndex(value, memoryIndex);
	}

	public void setDataForIndex(int value, int memoryIndex) {
		memory[memoryIndex] = value;
		getTableModel().fireTableDataChanged();
	}

	public void setData(int[] newValues) {
		System.arraycopy(newValues, 0, memory, 0, memory.length);
		readIndex = -1;
		writeIndex = -1;
		getTableModel().fireTableDataChanged();
	}

	public void resetData() {
		Arrays.fill(memory, 0);
		readIndex = -1;
		writeIndex = -1;
		getTableModel().fireTableDataChanged();
	}

	public void executeAdd() {
	}

	public void executePart1() {
	}

	public void executePart2() {
	}

	public void execute() {
		final long byteAddress = address.getValueInt();
		final int memoryIndex = (int) (byteAddress / 4L);

		if (hex2long(memReadControl.getValue()) == 1L) {
			readIndex = memoryIndex;
			readData.setValue(memory[memoryIndex]);
			getTableModel().fireTableDataChanged();
		}

		if (hex2long(memWriteControl.getValue()) == 1L) {
			writeIndex = memoryIndex;
			setDataForIndex(writeData.getValueInt(), memoryIndex);
		}
	}

	public int getReadIndex() {
		return readIndex;
	}

	public int getWriteIndex() {
		return writeIndex;
	}

	public int getSize() {
		return memory.length;
	}

	public int getValue(final int memoryIndex) {
		return memory[memoryIndex];
	}

	public synchronized AbstractTableModel getTableModel() {
		if (memoryTableModel == null) {
			memoryTableModel = new MemoryTableModel(this);
		}

		return memoryTableModel;
	}

	protected static class MemoryTableModel extends AbstractTableModel {
		public static final String COL_NUMBER = "Addr";
		public static final String COL_HEX = "Hex";
		public static final String COL_DEC = "Dec";
		public static final String COL_ACC = "Acc";

		protected final String[] columns = new String[]{COL_NUMBER, COL_HEX, COL_DEC, COL_ACC};
		protected final MemoryModel memory;

		public MemoryTableModel(MemoryModel memory) {
			this.memory = memory;
		}

		public String getColumnName(int column) {
			return columns[column];
		}

		public int getColumnCount() {
			return columns.length;
		}

		public int getRowCount() {
			return memory.getSize();
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			final String colName = columns[columnIndex];
			if (COL_NUMBER.equals(colName)) {
				return Data.int2dec(rowIndex, 2);
			} else if (COL_HEX.equals(colName)) {
				return Data.int2hex(memory.getValue(rowIndex), 1);
			} else if (COL_DEC.equals(colName)) {
				return Data.int2dec(memory.getValue(rowIndex), 1);
			} else if (COL_ACC.equals(colName)) {
				return getAccessMod(rowIndex);
			}

			return "";
		}

		protected String getAccessMod(int rowIndex) {
			StringBuffer accessMod = new StringBuffer();

			if (rowIndex == memory.getReadIndex()) {
				accessMod.append("r");
			}

			if (rowIndex == memory.getWriteIndex()) {
				accessMod.append("w");
			}

			if (accessMod.length() > 0) {
				accessMod.insert(0, "(");
				accessMod.append(")");
			}

			return accessMod.toString();
		}
	}
}
