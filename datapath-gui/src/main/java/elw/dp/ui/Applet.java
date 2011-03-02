package elw.dp.ui;

import elw.dp.app.Controller;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;

public class Applet extends JApplet{
	private static final Logger log = LoggerFactory.getLogger(Applet.class);

	private Controller instance;

	public void init() {
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			System.out.println("Error setting native LAF: " + e);
		}

		final Controller controller = new Controller();
		controller.setBaseUrl(getCodeBase().toString());
		controller.setUploadHeader(getParameter("upHeader"));
		controller.setElwCtx(getParameter("elw_ctx"));

		instance = controller;
	}

	public void start() {
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(instance.getPanelView(), BorderLayout.CENTER);
		getContentPane().add(instance.getLabelStatus(), BorderLayout.SOUTH);

		instance.start();
	}
}
