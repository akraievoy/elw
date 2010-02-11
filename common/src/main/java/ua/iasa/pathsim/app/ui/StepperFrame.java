package ua.iasa.pathsim.app.ui;

import javax.swing.*;
import java.awt.*;
import com.bws.base.swing.*;

public class StepperFrame extends JFrame {
    JPanel contentPanel;

    JTable code;
    JTable data;
    JTable regs;

    JButton run;
    JButton stepInto;
    JButton stepOver;
    JButton reset;

    JButton close;

    JLabel status;

    public void init() {
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(getContentPanel(), BorderLayout.CENTER);
        pack();
    }

    public JTable getCode() {
        if (code == null) {
            code = new JTable();
        }
        return code;
    }

    public JTable getData() {
        if (data == null) {
            data = new JTable();
        }
        return data;
    }

    public JTable getRegs() {
        if (regs == null) {
            regs = new JTable();
        }
        return regs;
    }

    public JButton getRun() {
        if (run == null) {
            run = new JButton("Run");
        }
        return run;
    }

    public JButton getStepOver() {
        if (stepOver == null) {
            stepOver = new JButton("Step Over");
        }
        return stepOver;
    }

    public JButton getStepInto() {
        if (stepInto == null) {
            stepInto = new JButton("Step Into");
        }
        return stepInto;
    }

    public JButton getReset() {
        if (reset == null) {
            reset = new JButton("Reset");
        }
        return reset;
    }

    public JButton getClose() {
        if (close == null) {
            close = new JButton("Close");
        }

        return close;
    }

    public JLabel getStatus() {
        if (status == null) {
            status = new JLabel("Status");
        }
        return status;
    }

    public JPanel getContentPanel() {
        if (contentPanel == null) {
            contentPanel = new JPanel(new GridBagLayout());

            GBC gbc = new GBC();

            gbc.moveTo(0, 0).span(2, 1).weight(2, 1).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

            contentPanel.add(Swing.border(Swing.scroll(getCode(), 400, 180), "Instructions"), gbc.getVal());

            gbc.moveTo(2, 0).span(1, 1).weight(0, 1).insets(2, 2).align(GridBagConstraints.SOUTH, GridBagConstraints.NONE);

            contentPanel.add(Swing.flowVert(new Component[]{getRun(), getStepOver(), getStepInto(), Box.createVerticalStrut(16), getReset()}, 3), gbc.getVal());

            gbc.moveTo(0, 1).span(1, 1).weight(1,1).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

            contentPanel.add(Swing.border(Swing.scroll(getData(), 200, 180), "Memory"), gbc.getVal());

            gbc.moveTo(1, 1).span(2, 1).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

            contentPanel.add(Swing.border(Swing.scroll(getRegs(), 200, 180), "Registers"), gbc.getVal());

            gbc.moveTo(0, 2).span(2, 1).weight(2, 0).insets(2, 2).align(GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL);

            contentPanel.add(Swing.flow(new JComponent[]{getStatus()}, 3), gbc.getVal());

            gbc.moveTo(2, 2).span(1, 1).weight(0, 0).insets(2, 2).align(GridBagConstraints.EAST, GridBagConstraints.NONE);

            contentPanel.add(Swing.flow(new JComponent[]{getClose()}, 3), gbc.getVal());

            gbc.moveTo(1, 3).span(1, 1).weight(1, 0).insets(2, 2).align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

            contentPanel.add(Box.createHorizontalGlue(), gbc.getVal());
        }

        return contentPanel;
    }

    public static void main(String[] args) {
        final StepperFrame frame = new StepperFrame();
        frame.init();
        frame.setVisible(true);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
}
