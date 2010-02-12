package elw.dp.ui;

import elw.dp.swing.JTextAreaLogHandler;
import java.awt.*;
import javax.swing.*;

import java.util.logging.*;

public class MainFrame extends JFrame {
    JTabbedPane tabs;

    EditorPanel editorPanel;
    RunnerPanel runnerPanel;

    JTextArea logger;

    public void init() {
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(getTabs(), BorderLayout.CENTER);
        getContentPane().add(new JScrollPane(getLogger()), BorderLayout.SOUTH);

        pack();
    }

    public JTabbedPane getTabs() {
        if (tabs == null) {
            tabs = new JTabbedPane();

            tabs.addTab("Edit", null, getEditorPanel(), "Editor");
            tabs.addTab("Run", null, getRunnerPanel(), "Runner");
        }

        return tabs;
    }

    public EditorPanel getEditorPanel() {
        if (editorPanel == null) {
            editorPanel = new EditorPanel();
        }

        return editorPanel;
    }

    public RunnerPanel getRunnerPanel() {
        if (runnerPanel == null) {
            runnerPanel = new RunnerPanel();
        }

        return runnerPanel;
    }

    public JTextArea getLogger() {
        if (logger == null) {
            logger = new JTextArea();

            final JTextAreaLogHandler logHandler = new JTextAreaLogHandler(logger, 65536 * 4);
            logHandler.setLevel(Level.FINEST);
            Logger.getLogger("ua.iasa.pathsim").addHandler(logHandler);
            Logger.getLogger("com.bws").addHandler(logHandler);
        }

        return logger;
    }
}
