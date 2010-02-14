package elw.dp.app;

import elw.dp.mips.Reg;
import elw.dp.mips.Registers;
import elw.dp.mips.asm.Data;

import javax.swing.table.AbstractTableModel;

public class RegistersTableModel extends AbstractTableModel {
	public static final String COL_NUMBER = "#";
	public static final String COL_NAME = "Name";
	public static final String COL_HEX = "Hex";
	public static final String COL_DEC = "Dec";
	public static final String COL_ACC = "Access";

	protected final String[] columns = new String[]{COL_NUMBER, COL_NAME, COL_HEX, COL_DEC, COL_ACC};
	protected final Registers registers;

	public RegistersTableModel(Registers registers) {
		this.registers = registers;
	}

	public String getColumnName(int column) {
		return columns[column];
	}

	public int getColumnCount() {
		return columns.length;
	}

	public int getRowCount() {
		return Reg.values().length;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		final Reg reg = Reg.values()[rowIndex];
		final String colName = columns[columnIndex];

		if (COL_NUMBER.equals(colName)) {

			return Data.int2dec(rowIndex, 2);

		} else if (COL_NAME.equals(colName)) {

			return reg.toString();

		} else if (COL_HEX.equals(colName)) {

			return Data.int2hex(registers.getRegInternal(reg), 8);

		} else if (COL_DEC.equals(colName)) {

			return Data.int2dec(registers.getRegInternal(reg));

		} else if (COL_ACC.equals(colName)) {

			return getAccessMod(reg);

		}

		return "";
	}

	protected String getAccessMod(Reg rowIndex) {
		StringBuffer accessMod = new StringBuffer();

		if (registers.getReadRegs().contains(rowIndex.ordinal())) {
			accessMod.append("r");
		}

		if (registers.getWriteRegs().contains(rowIndex.ordinal())) {
			accessMod.append("w");
		}

		return accessMod.toString();
	}
}
