package ua.iasa.pathsim.app;

import ua.iasa.pathsim.*;

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

