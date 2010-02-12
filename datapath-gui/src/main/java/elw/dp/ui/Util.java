package elw.dp.ui;

import javax.swing.*;
import java.awt.*;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: Util.java,v 1.1 2006/12/28 11:55:39 Anton S. Kraievoy Exp $
 */

public class Util {
	public static JComponent wrap(final Component textArea, String title) {
		return wrapSized(textArea, title, new Dimension(120, 120));
	}

	private static JComponent wrapSized(Component textArea, String title, Dimension preferredSize) {
		final JScrollPane scrollPane = new JScrollPane();

		scrollPane.setViewportView(textArea);
		scrollPane.setPreferredSize(preferredSize);
		scrollPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), title));

		return scrollPane;
	}
}

