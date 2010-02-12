package elw.dp.app;

import elw.dp.mips.Memory;
import elw.dp.mips.asm.Data;

import javax.swing.table.AbstractTableModel;

public class MemoryTableModel extends AbstractTableModel {
	public static final String COL_ADDR = "Addr";
	public static final String COL_HEX = "Hex8";
	public static final String COL_HEX_WORD = "Hex32";
	public static final String COL_DEC_WORD = "Dec32";
	public static final String COL_ACC = "Access";

	protected final String[] columns = new String[]{COL_ADDR, COL_ACC, COL_HEX_WORD, COL_DEC_WORD, COL_HEX};
	protected final Memory memory;

	public MemoryTableModel(Memory memory) {
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
		final int address = memory.getAddressAt(rowIndex);

		if (COL_ADDR.equals(colName)) {

			return Data.int2hex(address, 4);

		} else if (COL_HEX.equals(colName)) {

			return Data.int2hex(memory.getByteInternal(address), 2);

		} else if (COL_HEX_WORD.equals(colName)) {

			if (!memory.hasWord(address)) {
				return "?";
			}

			return Data.int2hex(memory.getWordInternal(address), 8);

		} else if (COL_DEC_WORD.equals(colName)) {

			if (!memory.hasWord(address)) {
				return "?";
			}

			return Data.int2dec(memory.getWordInternal(address));

		} else if (COL_ACC.equals(colName)) {

			return getAccessMod(address);

		}

		return "";
	}

	protected String getAccessMod(int address) {
		StringBuffer accessMod = new StringBuffer();

		if (memory.getReadAdresses().contains(address)) {
			accessMod.append("r");
		}

		if (memory.getWriteAdresses().contains(address)) {
			accessMod.append("w");
		}

		return accessMod.toString();
	}
}
