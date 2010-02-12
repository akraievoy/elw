/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;

import java.awt.*;
import javax.swing.table.*;

import java.util.Arrays;

class Registers extends RegMem implements RegistersModel {
    final int[] registers = new int[REGS];

    DataLine control, readData1, readData2, readRegister1;
    DataLine readRegister2, writeRegister, writeData;
    DataPathPanel parent;
    int read1RegisterIndex, read2RegisterIndex;
    protected RegistersTableModel registersTableModel;
    protected int writeRegisterIndex;

    public Registers(double x1, double y1, double width, double height, Color color, String label1, String label2, String name, DataLine control, DataLine readRegister1,
                     DataLine readRegister2, DataLine writeRegister, DataLine writeData, DataLine readData1, DataLine readData2, DataPathPanel parent) {
        super(x1, y1, width, height, color, label1, label2, name);
        this.readRegister1 = readRegister1;
        this.readRegister2 = readRegister2;
        this.writeRegister = writeRegister;
        this.writeData = writeData;
        this.control = control;
        this.readData1 = readData1;
        this.readData2 = readData2;
        zeroRegisters();
    }

    public void zeroRegisters() {
        Arrays.fill(registers, 0);
    }

    public void setRegister(int word, int registerIndex) {
        if (registerIndex > 0) {
            registers[registerIndex] = word;
        }
    }

    public void execute() {
    }

    public void executeAdd() {
    }

    public void executePart1() {
        read1RegisterIndex = Data.hex2int(readRegister1.getValue());
        read2RegisterIndex = Data.hex2int(readRegister2.getValue());
        readData1.setValue(registers[read1RegisterIndex]);
        readData2.setValue(registers[read2RegisterIndex]);

        getTableModel().fireTableDataChanged();
    }

    public void executePart2() {
        if (Data.hex2long(control.getValue()) == 1L) {
            writeRegisterIndex = (int) Data.hex2long(writeRegister.getValue());
            registers[writeRegisterIndex] = (int) writeData.getValueLong();
        } else {
            writeRegisterIndex = -1;
        }

        getTableModel().fireTableDataChanged();
    }

    public int getRead1RegisterIndex() {
        return read1RegisterIndex;
    }

    public int getRead2RegisterIndex() {
        return read2RegisterIndex;
    }

    public boolean isWriteEnabled() {
        return control.getValueLong() == 1;
    }

    public int getWriteRegisterIndex() {
        return writeRegisterIndex;
    }

    public int getWriteDataValue() {
        return (int) (isWriteEnabled() ? Data.hex2long(writeData.getValue()) : -1);
    }

    public int getRegisterValue(int index) {
        return registers[index];
    }

    public void load(int[] lastLoaded) {
        read1RegisterIndex = -1;
        read2RegisterIndex = -1;
        writeRegisterIndex = -1;
        System.arraycopy(lastLoaded, 0, registers, 0, REGS);
        getTableModel().fireTableDataChanged();
    }

    public synchronized AbstractTableModel getTableModel() {
        if (registersTableModel == null) {
            registersTableModel = new RegistersTableModel(this);
        }

        return registersTableModel;
    }

    protected static class RegistersTableModel extends AbstractTableModel {
        public static final String COL_NUMBER = "#";
        public static final String COL_NAME = "Name";
        public static final String COL_HEX = "Hex";
        public static final String COL_DEC = "Dec";
        public static final String COL_ACC = "Acc";

        protected final String[] columns = new String[]{COL_NUMBER, COL_NAME, COL_HEX, COL_DEC, COL_ACC};
        protected final RegistersModel registers;

        public RegistersTableModel(RegistersModel registers) {
            this.registers = registers;
        }

        public String getColumnName(int column) {
            return columns[column];
        }

        public int getColumnCount() {
            return columns.length;
        }

        public int getRowCount() {
            return REGS;
        }

        public Object getValueAt(int rowIndex, int columnIndex) {
            final String colName = columns[columnIndex];
            if (COL_NUMBER.equals(colName)) {
                return Data.int2dec(rowIndex, 2);
            } else if (COL_NAME.equals(colName)) {
                return InstructionsModel.REGS[rowIndex];
            } else if (COL_HEX.equals(colName)) {
                return Data.int2hex(registers.getRegisterValue(rowIndex), 1);
            } else if (COL_DEC.equals(colName)) {
                return Data.int2dec(registers.getRegisterValue(rowIndex), 1);
            } else if (COL_ACC.equals(colName)) {
                return getAccessMod(rowIndex);
            }

            return "";
        }

        protected String getAccessMod(int rowIndex) {
            StringBuffer accessMod = new StringBuffer();
            
            if (rowIndex == registers.getRead1RegisterIndex() || rowIndex == registers.getRead2RegisterIndex()) {
                accessMod.append("r");
            }

            if (rowIndex == registers.getWriteRegisterIndex()) {
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
