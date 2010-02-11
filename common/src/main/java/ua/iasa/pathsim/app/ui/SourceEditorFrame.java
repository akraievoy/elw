package ua.iasa.pathsim.app.ui;

import com.bws.base.swing.*;
import java.awt.*;
import javax.swing.*;

public class SourceEditorFrame extends JFrame {
    JTextPane definition;
    JTextArea solution;
    JButton submit;
    JButton close;
    JButton run;
    JButton step;
    JComboBox testingCase;

    JPanel contentPanel;

    public void init() {
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(getContentPanel(), BorderLayout.CENTER);
        pack();
    }

    public JTextPane getDefinition() {
        if (definition == null) {
            definition = new JTextPane();

            definition.setContentType("text/html");
            definition.setEditable(false);
        }

        return definition;
    }

    public JTextArea getSolution() {
        if (solution == null) {
            solution = new JTextArea();
        }

        return solution;
    }

    public JButton getSubmit() {
        if (submit == null) {
            submit = new JButton("Submit");
        }

        return submit;
    }

    public JButton getClose() {
        if (close == null) {
            close = new JButton("Close");
        }

        return close;
    }

    public JButton getRun() {
        if (run == null) {
            run = new JButton("Run");
        }

        return run;
    }

    public JButton getStep() {
        if (step == null) {
            step = new JButton("Step");
        }

        return step;
    }

    public JComboBox getTestingCase() {
        if (testingCase == null) {
            testingCase = new JComboBox();

            testingCase.setEditable(false);
        }

        return testingCase;
    }

    public JPanel getContentPanel() {
        if (contentPanel == null) {
            contentPanel = new JPanel(new GridBagLayout());

            GBC gbc = new GBC();

            gbc.moveTo(0, 0).span(2, 1).weight(1, 1).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

            contentPanel.add(Swing.border(Swing.scroll(getDefinition(), 400, 120), "Definition"), gbc.getVal());

            gbc.moveTo(0, 1).span(2, 1).weight(1, 3).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

            contentPanel.add(Swing.border(Swing.scroll(getSolution(), 400, 180), "Solution"), gbc.getVal());

            gbc.moveTo(0, 2).span(1, 1).weight(1, 0).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.NONE);

            contentPanel.add(Swing.flow(new JComponent[]{getTestingCase(), getStep(), getRun()}, 3), gbc.getVal());

            gbc.moveTo(1, 2).span(1, 1).weight(0, 0).insets(2, 2).align(GridBagConstraints.EAST, GridBagConstraints.NONE);

            contentPanel.add(Swing.flow(new JComponent[]{getSubmit(), getClose()}, 3), gbc.getVal());
        }

        return contentPanel;
    }

    public static void main(String[] args) {
        final SourceEditorFrame frame = new SourceEditorFrame();
        frame.init();
        frame.setVisible(true);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
}
