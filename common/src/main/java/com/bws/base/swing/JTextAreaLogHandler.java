/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package com.bws.base.swing;

import javax.swing.*;

import java.util.logging.*;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: JTextAreaLogHandler.java,v 1.1 2006/12/27 21:04:24 Anton S. Kraievoy Exp $
 */
public class JTextAreaLogHandler extends ConsoleHandler {
    protected final JTextArea logger;
    protected final int maxLength;

    public JTextAreaLogHandler(JTextArea logger) {
        this(logger, 65536 * 4);
    }

    public JTextAreaLogHandler(JTextArea logger, int maxLength) {
        this.logger = logger;
        this.maxLength = maxLength;
        logger.setEditable(false);
    }

    public void publish(final LogRecord record) {
        if (SwingUtilities.isEventDispatchThread()) {
            publishInternal(record);
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    publishInternal(record);
                }
            });
        }
    }

    private void publishInternal(LogRecord record) {
        logger.append(getFormatter().format(record));
        flush();
    }

    public void flush() {
        if (SwingUtilities.isEventDispatchThread()) {
            flushInternal();
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    flushInternal();
                }
            });
        }
    }

    private void flushInternal() {
        if (maxLength > 0) {
            while (logger.getText().length() > maxLength) {
                int cut = logger.getText().indexOf("\n");
                logger.replaceRange("", 0, cut + 1);
            }
        }
        logger.setCaretPosition(logger.getText().length());
    }

    public static class LogFormatter extends SimpleFormatter {
        public synchronized String format(LogRecord record) {
            return record.getMessage() + "\n";
        }
    }
}

