package elw.dp.swing;

import org.akraievoy.gear.G;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Swing {
	Swing() {
	}

	public static JPanel flow(Component[] components, final int interval) {
		final JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));

		for (Component component : components) {
			result.add(Box.createHorizontalStrut(interval));
			result.add(component);
		}
		result.add(Box.createHorizontalStrut(interval));

		return result;
	}

	public static JPanel flowVert(Component[] components, final int interval) {
		final JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));

		for (Component component : components) {
			result.add(Box.createVerticalStrut(interval));
			result.add(component);
		}
		result.add(Box.createVerticalStrut(interval));

		return result;
	}

	public static JScrollPane scroll(final JComponent viewPort, int prefWidth, int prefHeight) {
		JScrollPane result = new JScrollPane();

		result.setViewportView(viewPort);
		result.setPreferredSize(new Dimension(prefWidth, prefHeight));

		return result;
	}

	public static JComponent border(JComponent component, final String title) {
		component.setBorder(BorderFactory.createTitledBorder(title));

		return component;
	}

	public static Font lookupMonospaceFont() {
		final Font[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts();

		Font courierNew = null;
		Font courier = null;
		Font monospace = null;

		for (Font font : fonts) {
			if ("Courier New".equalsIgnoreCase(font.getName())) {
				courierNew = font;
			}
			if (font.getName().toLowerCase().contains("courier")) {
				courier = font;
			}
			if (font.getFamily().toLowerCase().contains("monospace")) {
				monospace = font;
			}
		}

		final Font toReturn = courierNew != null ? courierNew : courier != null ? courier : monospace;

		return toReturn != null ? toReturn.deriveFont(Font.PLAIN, 12) : Font.decode(null);
	}
}
