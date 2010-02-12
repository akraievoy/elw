/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.swing;

import java.awt.*;

/**
 * Simple class to manipulate GridBagConstraints instance.
 *
 * @author Anton Kraievoy
 * @version $Id: GBC.java,v 1.2 2006/12/27 15:40:57 Anton S. Kraievoy Exp $
 */

public class GBC {
    protected GridBagConstraints val = new GridBagConstraints();
    protected GridBagConstraints def = new GridBagConstraints();
    protected boolean resetOnMove = true;

    public GBC() {
        this(GridBagConstraints.CENTER, GridBagConstraints.BOTH);
    }

    public GBC(int defaultAnchor, int defaultFill) {
        gbcAlign(def, defaultAnchor, defaultFill);
        gbcMoveTo(val, 0, 0);
    }

    public GridBagConstraints getDef() {
        return def;
    }

    public GridBagConstraints getVal() {
        return val;
    }

    public boolean isResetOnMove() {
        return resetOnMove;
    }

    public void setResetOnMove(boolean resetOnMove) {
        this.resetOnMove = resetOnMove;
    }

    public GBC reset() {
        getVal().insets = getDef().insets;
        getVal().ipadx = getDef().ipadx;
        getVal().ipady = getDef().ipady;
        getVal().weightx = getDef().weightx;
        getVal().weighty = getDef().weighty;
        getVal().gridheight = getDef().gridheight;
        getVal().gridwidth = getDef().gridwidth;
        getVal().anchor = getDef().anchor;
        getVal().fill = getDef().fill;
        return this;
    }

    public GBC insets(double w, double h) {
        gbcInsets(getVal(), w, h);
        return this;
    }

    public GBC pad(double w, double h) {
        gbcPad(getVal(), w, h);
        return this;
    }

    public GBC weight(double w, double h) {
        gbcWeight(getVal(), w, h);
        return this;
    }

    public GBC span(double w, double h) {
        gbcSpan(getVal(), w, h);
        return this;
    }

    public GBC align(int anchor, int fill) {
        gbcAlign(getVal(), anchor, fill);
        return this;
    }

    public GBC moveTo(int x, int y) {
        gbcMoveTo(getVal(), x, y);
        if (resetOnMove) {
            reset();
        }
        return this;
    }

    public GBC moveTo(double x, double y) {
        moveTo((int) x, (int) y);
        return this;
    }

    public void defInsets(double w, double h) {
        gbcInsets(getDef(), w, h);
    }

    public void defPad(double w, double h) {
        gbcPad(getDef(), w, h);
    }

    public void defWeight(double w, double h) {
        gbcWeight(getDef(), w, h);
    }

    public void defSpan(double w, double h) {
        gbcSpan(getDef(), w, h);
    }

    public void defAlign(int anchor, int fill) {
        gbcAlign(getDef(), anchor, fill);
    }

    public void defMoveTo(int x, int y) {
        gbcMoveTo(getDef(), x, y);
    }

    public void defMoveTo(double x, double y) {
        defMoveTo((int) x, (int) y);
    }

    public static void gbcInsets(GridBagConstraints gbc, double w, double h) {
        gbc.insets = new Insets((int) h, (int) w, (int) h, (int) w);
    }

    public static void gbcPad(GridBagConstraints gbc, double w, double h) {
        gbc.ipadx = (int) w;
        gbc.ipady = (int) h;
    }

    public static void gbcWeight(GridBagConstraints gbc, double w, double h) {
        gbc.weightx = (int) w;
        gbc.weighty = (int) h;
    }

    public static void gbcSpan(GridBagConstraints gbc, double w, double h) {
        gbc.gridwidth = (int) w;
        gbc.gridheight = (int) h;
    }

    public static void gbcAlign(GridBagConstraints gbc, int anchor, int fill) {
        gbc.anchor = anchor;
        gbc.fill = fill;
    }

    public static void gbcMoveTo(GridBagConstraints gbc, int x, int y) {
        gbc.gridx = x;
        gbc.gridy = y;
    }

    public static void gbcMoveTo(GridBagConstraints gbc, double x, double y) {
        gbcMoveTo(gbc, (int) x, (int) y);
    }
}

