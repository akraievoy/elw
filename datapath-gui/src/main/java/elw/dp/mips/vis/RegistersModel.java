/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import javax.swing.table.TableModel;

/**
 * DataModel interface for Register Memory device.
 *
 * @author Anton Kraievoy
 * @version $Id: RegistersModel.java,v 1.4 2006/12/27 20:22:44 Anton S. Kraievoy Exp $
 */
public interface RegistersModel {
    int REGS = 32;

    int getRead1RegisterIndex();

    int getRead2RegisterIndex();

    boolean isWriteEnabled();

    int getWriteRegisterIndex();

    int getWriteDataValue();

    int getRegisterValue(int index);

    void zeroRegisters();

    void setRegister(int word, int registerIndex);

    void load(int[] lastLoaded);

    TableModel getTableModel();
}

