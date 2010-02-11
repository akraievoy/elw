/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package com.bws.base.swing;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import org.jdesktop.swinghelper.transformer.JXTransformer;

import java.util.ArrayList;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: JButtonBar.java,v 1.3 2006/12/27 13:58:06 Anton S. Kraievoy Exp $
 */
public class JButtonBar extends JPanel {
    protected GBC gbc = new GBC();

    protected final JSidebarPane parent;
    protected String axis;

    protected java.util.List<JSidebarPane.SidebarInfo> infos = new ArrayList<JSidebarPane.SidebarInfo>();
    protected Dimension buttonPadding = new Dimension(1, 1);
    protected Component filler = new JPanel();
    protected ButtonGroup buttonGroup = new ButtonGroup();
    protected JToggleButton noneButton = new JToggleButton();

    protected ButtonModel prevSelection = noneButton.getModel();
    protected boolean expand = true;

    public JButtonBar(JSidebarPane parent, final String newAxis) {
        this.parent = parent;
        setLayout(new GridBagLayout());

        this.axis = newAxis;

        gbc.defInsets(getMainAxis(buttonPadding), getSecondaryAxis(buttonPadding));
        gbc.moveTo(0, 0);
        gbc.weight(1, 1);

        add(filler, gbc.getVal());

        buttonGroup.add(noneButton);
    }

    public void addSidebar(final JSidebarPane.SidebarInfo sbInfo) {
        remove(filler);

        final Dimension newButtonCell = new Dimension(getButtonCount(), 0);
        final Dimension newFillerCell = new Dimension(getButtonCount() + 1, 0);

        gbc.moveTo(getMainAxis(newButtonCell), getSecondaryAxis(newButtonCell));
        sbInfo.toggler = createToggler(sbInfo); 
        buttonGroup.add(sbInfo.toggler);
        sbInfo.toggler.getModel().addActionListener(new TogglerActionListener(this));
        add(transform(sbInfo.toggler), gbc.getVal());

        gbc.moveTo(getMainAxis(newFillerCell), getSecondaryAxis(newFillerCell));
        gbc.weight(1, 1);
        add(filler, gbc.getVal());

        infos.add(sbInfo);
    }

    private JToggleButton createToggler(JSidebarPane.SidebarInfo sbInfo) {
        final JToggleButton jToggleButton = new JToggleButton(sbInfo.title, sbInfo.icon);
        final Font font = jToggleButton.getFont().deriveFont(8);
        jToggleButton.setFont(font);
        jToggleButton.setMargin(new Insets(1,1,1,1));
        jToggleButton.setIconTextGap(2);
        jToggleButton.setActionCommand(sbInfo.key);
        return jToggleButton;
    }

    private JComponent transform(JToggleButton toggler) {
        if (isHorizontal()) {
            return toggler;
        }

        final JXTransformer transformer = new JXTransformer(toggler);
        final int sign = BorderLayout.WEST.equals(axis) ? -1 : 1;
        transformer.rotate(sign * Math.PI / 2);
        return transformer;
    }

    public boolean isHorizontal() {
        return BorderLayout.SOUTH.equals(axis) || BorderLayout.NORTH.equals(axis);
    }

    public double getMainAxis(final Dimension d) {
        return isHorizontal() ? d.getWidth() : d.getHeight();
    }

    public double getSecondaryAxis(final Dimension d) {
        return isHorizontal() ? d.getHeight() : d.getWidth();
    }

    public int getButtonCount() {
        return infos.size();
    }

    public Dimension getButtonPadding() {
        return buttonPadding;
    }

    public void setButtonPadding(Dimension buttonPadding) {
        this.buttonPadding = buttonPadding;
    }

    protected static class TogglerActionListener implements ActionListener {
        protected JButtonBar jButtonBar;

        public TogglerActionListener(JButtonBar jButtonBar) {
            this.jButtonBar = jButtonBar;
        }

        public void actionPerformed(ActionEvent e) {
            final ButtonModel source = (ButtonModel) e.getSource();
            final JSidebarPane.SidebarInfo sbInfo1 = jButtonBar.parent.findByKey(source.getActionCommand());
            if (source.isSelected() && source == jButtonBar.prevSelection) {
                if (jButtonBar.expand && sbInfo1.expandable) {
                    jButtonBar.parent.expand(jButtonBar.axis);
                    jButtonBar.expand = false;
                } else {
                    jButtonBar.buttonGroup.setSelected(jButtonBar.noneButton.getModel(), true);
                    jButtonBar.prevSelection = jButtonBar.noneButton.getModel();
                    jButtonBar.parent.deactivate(jButtonBar.axis);
                    jButtonBar.expand = true;
                }
            } else {
                jButtonBar.prevSelection = source;
                jButtonBar.parent.activate(sbInfo1.axis, sbInfo1);
                jButtonBar.expand = true;
            }
        }
    }
}

