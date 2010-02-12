/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;

import java.awt.*;
import javax.swing.table.AbstractTableModel;
import static elw.dp.mips.asm.Data.hex2int;
import static elw.dp.mips.asm.Data.int2long;

import java.util.Arrays;

class InstructionMemory extends RegMem implements InstructionsModel {

    DataLine input, output;
    int memory[];
    String code[];
    DataPathPanel parent;
    protected int accessIndex;
    private AbstractTableModel instructionsTableModel;

    public InstructionMemory(double x1, double y1, double width, double height, Color color, String label1, String label2, String name, DataLine input, DataLine output, DataPathPanel parent) {
        super(x1, y1, width, height, color, label1, label2, name);
        this.input = input;
        this.output = output;
        this.parent = parent;
        memory = new int[SIZE];
        code = new String[SIZE];
    }

    public void initInstructions() {
        Arrays.fill(memory, 0);
        Arrays.fill(code, "");
        accessIndex = -1;
        getTableModel().fireTableDataChanged();
    }

    public void setInstructionAtAddress(int instruction, int address, String code) {
        int memoryIndex = (int) (int2long(address) / 4);
        memory[memoryIndex] = instruction;
        getTableModel().fireTableRowsUpdated(memoryIndex, memoryIndex);
    }

    public void setInstructions(int[] instructions, String[] newCode) {
        System.arraycopy(instructions, 0, memory, 0, instructions.length);
        System.arraycopy(newCode, 0, code, 0, newCode.length);
        accessIndex = -1;
        getTableModel().fireTableDataChanged();
    }

    public int getSize() {
        return memory.length;
    }

    public int getValue(int memoryIndex) {
        return memory[memoryIndex];
    }

    public String getCode(int memoryIndex) {
        return code[memoryIndex];
    }

    public int getAccessIndex() {
        return accessIndex;
    }

    public void executeAdd() {
    }

    public void executePart1() {
    }

    public void executePart2() {
    }

    public void execute() {
        String addressString = input.getValue();
        int address = hex2int(addressString);
        accessIndex = (int) (int2long(address) / 4);
        output.setValue(memory[accessIndex]);
        getTableModel().fireTableDataChanged();
    }

    public synchronized AbstractTableModel getTableModel() {
        if (instructionsTableModel == null) {
            instructionsTableModel = new InstructionsTableModel(this);
        }

        return instructionsTableModel;
    }

    protected static class InstructionsTableModel extends AbstractTableModel {
        public static final String COL_NUMBER = "Addr";
        public static final String COL_HEX = "Hex";
        public static final String COL_CODE = "Code";
        public static final String COL_ACC = "Acc";

        protected final String[] columns = new String[]{COL_NUMBER, COL_HEX, COL_ACC, COL_CODE};
        protected final InstructionsModel instructions;

        public InstructionsTableModel(InstructionsModel instructions) {
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
            if (COL_NUMBER.equals(colName)) {
                return Data.int2dec(rowIndex, 2);
            } else if (COL_HEX.equals(colName)) {
                return Data.int2hex(instructions.getValue(rowIndex), 8);
            } else if (COL_CODE.equals(colName)) {
                return instructions.getCode(rowIndex);
            } else if (COL_ACC.equals(colName)) {
                return getAccessMod(rowIndex);
            }

            return "";
        }

        protected String getAccessMod(int rowIndex) {
            StringBuffer accessMod = new StringBuffer();

            if (rowIndex == instructions.getAccessIndex()) {
                accessMod.append("r");
            }

            if (accessMod.length() > 0) {
                accessMod.insert(0, "(");
                accessMod.append(")");
            }

            return accessMod.toString();
        }
    }}
