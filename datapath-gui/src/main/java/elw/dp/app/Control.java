package elw.dp.app;

import elw.dp.mips.vis.InstructionsModel;
import elw.dp.mips.vis.MemoryModel;
import elw.dp.mips.vis.RegistersModel;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: Control.java,v 1.2 2006/12/28 11:55:39 Anton S. Kraievoy Exp $
 */

public interface Control {
    void resetMachine();

    RegistersModel getRegisters();

    MemoryModel getData();

    InstructionsModel getInstructions();

    AssembleAction getAssembleAction();

    LoadDataAction getLoadDataAction();

    LoadRegistersAction getLoadRegistersAction();

    ResetAction getResetAction();
}

