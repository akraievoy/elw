/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import java.util.*;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: Assembler.java,v 1.1 2006/12/28 10:38:48 Anton S. Kraievoy Exp $
 */
public interface Assembler {
    void assembleLoad(List<String> codeLines);

    String[] getCodeLines();

    int[] getInstructions();
}

