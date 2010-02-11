package ua.iasa.pathsim.app;

import com.bws.base.utils.Str;
import javax.swing.table.AbstractTableModel;
import ua.iasa.pathsim.domain.Instructions;
import ua.iasa.pathsim.Data;

public class InstructionsTableModel extends AbstractTableModel {
    public static final String COL_ADDR = "Addr";
    public static final String COL_BIN = "Hex";
    public static final String COL_CODE = "Code";
    public static final String COL_ACC = "Acc";

    protected final String[] columns = new String[]{COL_ADDR, COL_BIN, COL_ACC, COL_CODE};
    protected final Instructions instructions;

    public InstructionsTableModel(Instructions instructions) {
        this.instructions = instructions;
    }

    public String getColumnName(int column) {
        return columns[column];
    }

    public int getColumnCount() {
        return columns.length;
    }

    public int getRowCount() {
        return instructions.getSize();
    }

    public Object getValueAt(int rowIndex, int columnIndex) {
        final String colName = columns[columnIndex];
        final int address = instructions.getAddressAt(rowIndex);

        if (COL_ADDR.equals(colName)) {

            return Data.int2dec(address, 2);

        } else if (COL_BIN.equals(colName)) {

            final String code = instructions.getInternal(address).getBinaryCode();
            
            return groupBy(code, 4);

        } else if (COL_CODE.equals(colName)) {

            return instructions.getInternal(address).getCodeLine();

        } else if (COL_ACC.equals(colName)) {

            return getAccessMod(address);

        }

        return Str.EMPTY;
    }

    private String groupBy(final String code, final int group) {
        if (code.length() > group) {
            return code.substring(0, group) + " " + groupBy(code.substring(group), group);
        }

        return code;
    }

    protected String getAccessMod(int rowIndex) {
        StringBuffer accessMod = new StringBuffer();

        if (instructions.getReadAddresses().contains(rowIndex)) {
            accessMod.append("r");
        }

        return accessMod.toString();
    }
}