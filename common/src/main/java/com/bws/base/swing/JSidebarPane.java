/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package com.bws.base.swing;

import java.awt.*;
import javax.swing.*;

import java.util.*;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: JSidebarPane.java,v 1.4 2006/12/27 15:41:31 Anton S. Kraievoy Exp $
 */
public class JSidebarPane extends JPanel {
    protected Map<String, SidebarInfo> sbMap;
    protected GBC gbc = new GBC();
    protected JButtonBar southButtonBar;
    protected JButtonBar eastButtonBar;
    protected JButtonBar westButtonBar;
    protected JSplitPane westSplit;
    protected JSplitPane eastSplit;
    protected JSplitPane mainSplit;

    protected JSidebarPane() {
        sbMap = new HashMap<String, SidebarInfo>();
    }

    protected void init() {
        southButtonBar = new JButtonBar(this, BorderLayout.SOUTH);
        westButtonBar = new JButtonBar(this, BorderLayout.WEST);
        eastButtonBar = new JButtonBar(this, BorderLayout.EAST);

        mainSplit = createSplitter(JSplitPane.VERTICAL_SPLIT, 1.0);
        westSplit = createSplitter(JSplitPane.HORIZONTAL_SPLIT, 0);
        eastSplit = createSplitter(JSplitPane.HORIZONTAL_SPLIT, 1.0);

        setLayout(new GridBagLayout());

        gbc.moveTo(1, 0);
        gbc.weight(1, 1);
        gbc.align(GridBagConstraints.CENTER, GridBagConstraints.BOTH);

        add(mainSplit, gbc.getVal());

        mainSplit.setTopComponent(westSplit);
        westSplit.setRightComponent(eastSplit);
        setContent(new JButton("Content"));
    }

    public void addSidebar(SidebarInfo sbInfo) {
        if (BorderLayout.WEST.equals(sbInfo.axis)) {
            westButtonBar.addSidebar(sbInfo);
            if (westButtonBar.getButtonCount() == 1) {
                gbc.moveTo(0, 0);
                gbc.align(GridBagConstraints.NORTH, GridBagConstraints.BOTH);
                add(wrap(westButtonBar), gbc.getVal());
                gbc.moveTo(0, 1);
                add(Box.createHorizontalStrut(18), gbc.getVal());
            }
        } else if (BorderLayout.EAST.equals(sbInfo.axis)) {
            eastButtonBar.addSidebar(sbInfo);
            if (eastButtonBar.getButtonCount() == 1) {
                gbc.moveTo(2, 0);
                gbc.align(GridBagConstraints.NORTH, GridBagConstraints.BOTH);
                add(wrap(eastButtonBar), gbc.getVal());
                gbc.moveTo(2, 1);
                add(Box.createHorizontalStrut(18), gbc.getVal());
            }
        } else if (BorderLayout.SOUTH.equals(sbInfo.axis)) {
            southButtonBar.addSidebar(sbInfo);
            if (southButtonBar.getButtonCount() == 1) {
                gbc.moveTo(1, 1);
                gbc.align(GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL);
                add(southButtonBar, gbc.getVal());
            }
        }
        sbMap.put(sbInfo.key, sbInfo);
    }

    public JComponent wrap(final JComponent wrappee) {
        final JScrollPane wrapper = new JScrollPane();
        wrapper.setViewportView(wrappee);
        wrapper.setAutoscrolls(false);
        wrapper.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        wrapper.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        wrapper.setBorder(null);
        return wrapper;
    }

    protected JSplitPane createSplitter(int split, double resizeWeight) {
        final JSplitPane jSplitPane = new JSplitPane(split);
        jSplitPane.setOneTouchExpandable(true);
        jSplitPane.setResizeWeight(resizeWeight);
        jSplitPane.setDividerSize(1);
        jSplitPane.setBorder(null);
        return jSplitPane;
    }

    public static JSidebarPane createPane() {
        final JSidebarPane jSidebarPane = new JSidebarPane();
        jSidebarPane.init();
        return jSidebarPane;
    }

    public void activate(String axis, SidebarInfo sbInfo) {
        if (BorderLayout.SOUTH.equals(axis)) {
            eastSplit.setDividerLocation(-1);
            mainSplit.setBottomComponent(sbInfo.comp);
            westSplit.setDividerLocation(-1);
        } else if (BorderLayout.EAST.equals(axis)) {
            eastSplit.setRightComponent(sbInfo.comp);
            mainSplit.setDividerLocation(-1);
            westSplit.setDividerLocation(-1);
        } else if (BorderLayout.WEST.equals(axis)) {
            eastSplit.setDividerLocation(-1);
            mainSplit.setDividerLocation(-1);
            westSplit.setLeftComponent(sbInfo.comp);
        }
    }

    public void deactivate(String axis) {
        if (BorderLayout.SOUTH.equals(axis)) {
            mainSplit.setBottomComponent(null);
        } else if (BorderLayout.EAST.equals(axis)) {
            eastSplit.setRightComponent(null);
        } else if (BorderLayout.WEST.equals(axis)) {
            westSplit.setLeftComponent(null);
        }
    }

    public void expand(String axis) {
        if (BorderLayout.SOUTH.equals(axis)) {
            eastSplit.setDividerLocation(-1);
            mainSplit.setDividerLocation(0.0);
            westSplit.setDividerLocation(-1);
        } else if (BorderLayout.EAST.equals(axis)) {
            eastSplit.setDividerLocation(0.0);
            mainSplit.setDividerLocation(-1);
            westSplit.setDividerLocation(-1);
        } else if (BorderLayout.WEST.equals(axis)) {
            eastSplit.setDividerLocation(-1);
            mainSplit.setDividerLocation(-1);
            westSplit.setDividerLocation(1.0);
        }
    }

    public SidebarInfo findByKey(final String key) {
        return sbMap.get(key);
    }

    public void setContent(final JComponent comp) {
        eastSplit.setLeftComponent(comp);
    }

    public static class SidebarInfo {
        public JComponent comp;
        public String key;
        public String title;
        public Icon icon;
        public boolean enabled;
        public boolean expandable = true;
        public String axis;
        public JToggleButton toggler;

        public SidebarInfo(String axis, boolean enabled, Icon icon, String key, JComponent component, String title) {
            this.axis = axis;
            this.enabled = enabled;
            this.icon = icon;
            this.key = key;
            this.comp = component;
            this.title = title;
        }
    }

    public static void main(String[] args) {
        JFrame test = new JFrame();
        test.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        JSidebarPane comp = createPane();
        test.getContentPane().add(comp, BorderLayout.CENTER);

        comp.addSidebar(new SidebarInfo(
                BorderLayout.SOUTH,
                true,
                null,
                "sb1",
                createSBPanel("sb one (south)"),
                "sb one (south)"
        ));
        comp.addSidebar(new SidebarInfo(
                BorderLayout.SOUTH,
                true,
                null,
                "sb112",
                createSBPanel("sb oneone (south)"),
                "sb oneone (south)"
        ));
        comp.addSidebar(new SidebarInfo(
                BorderLayout.SOUTH,
                true,
                null,
                "sb111",
                createSBPanel("sb oneoneone (south)"),
                "sb oneoneone (south)"
        ));
        comp.addSidebar(new SidebarInfo(
                BorderLayout.EAST,
                true,
                null,
                "sb2",
                createSBPanel("sb two (east)"),
                "sb two (east)"
        ));
        comp.addSidebar(new SidebarInfo(
                BorderLayout.EAST,
                true,
                null,
                "sb five",
                createSBPanel("sb five (east)"),
                "sb five (east)"
        ));
        comp.addSidebar(new SidebarInfo(
                BorderLayout.WEST,
                true,
                null,
                "sb3",
                createSBPanel("sb three (west)"),
                "sb three (west)"
        ));
        comp.addSidebar(new SidebarInfo(
                BorderLayout.WEST,
                true,
                null,
                "sb4",
                createSBPanel("sb four (west)"),
                "sb four (west)"
        ));

        test.pack();
        test.setVisible(true);
    }

    private static JPanel createSBPanel(String title) {
        final JPanel jPanel = new JPanel(new BorderLayout());
        jPanel.add(new JButton(title));
        return jPanel;
    }
}


