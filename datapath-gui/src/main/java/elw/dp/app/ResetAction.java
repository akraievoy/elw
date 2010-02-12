/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.app;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: ResetAction.java,v 1.1 2006/12/28 10:57:27 Anton S. Kraievoy Exp $
 */
public class ResetAction extends AbstractAction {
	protected final Control control;

	public ResetAction(Control control) {
		super("Reset");
		this.control = control;
	}

	public void actionPerformed(ActionEvent e) {
		control.resetMachine();
	}
}

