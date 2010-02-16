package elw.dp.ui;

import elw.dp.app.Controller;
import elw.vo.Course;
import elw.vo.Version;
import org.apache.log4j.BasicConfigurator;
import org.codehaus.jackson.map.ObjectMapper;

import javax.swing.*;
import java.io.IOException;
import java.io.InputStream;

public class Standalone {
	public static void main(String[] args) throws IOException {
		BasicConfigurator.configure();

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			System.out.println("Error setting native LAF: " + e);
		}

		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				final Controller instance = new Controller();
				Version version;
				try {
					final InputStream modelStream = Controller.class.getResourceAsStream("/aos-s10.json");
					final ObjectMapper mapper = new ObjectMapper();
					final Course course = mapper.readValue(modelStream, Course.class);

					version = course.getAssBundles()[0].getAssignments()[0].getVersions()[0];
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
				instance.setSelectedTask(version);
				instance.init();

				final JFrame frame = new JFrame();
				frame.setTitle("DataPath");
				frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

				frame.getContentPane().add(instance.getView().getRootPanel());
				frame.pack();
				frame.setSize(600, 400);
				frame.setLocation(20, 20);
				frame.setVisible(true);
			}
		});
	}
}
