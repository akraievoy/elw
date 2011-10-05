package elw.dp.ui;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import java.awt.*;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public class FeedbackHandler extends Handler {
    private final static SimpleDateFormat dateFormat = new SimpleDateFormat("HH:mm:ss.SSS");
    private final JTextPane outputPane;
    private Map<Level, Style> styles = new HashMap<Level, Style>();

    //	please access this value only from Swing worker thread...
    private int traceStart = -1;

    public FeedbackHandler(JTextPane outputPane) {
        super();
        this.outputPane = outputPane;
        init();
    }

    private void init() {
        final Style fatal = outputPane.addStyle("fatal", null);
        StyleConstants.setForeground(fatal, Color.RED.darker().darker());
        StyleConstants.setBold(fatal, true);
        styles.put(Level.SEVERE, fatal);

        final Style error = outputPane.addStyle("error", null);
        StyleConstants.setForeground(error, Color.RED.darker());
        StyleConstants.setBold(fatal, true);
        styles.put(Level.WARNING, error);

        final Style warn = outputPane.addStyle("warn", null);
        StyleConstants.setForeground(warn, Color.ORANGE.darker());
        StyleConstants.setBold(warn, true);
        styles.put(Level.CONFIG, warn);

        final Style info = outputPane.addStyle("info", null);
        StyleConstants.setForeground(info, Color.DARK_GRAY.darker());
        styles.put(Level.INFO, info);

        final Style debug = outputPane.addStyle("debug", null);
        StyleConstants.setForeground(debug, Color.GRAY);
        styles.put(Level.FINE, debug);

        final Style finer = outputPane.addStyle("finer", null);
        StyleConstants.setForeground(finer, Color.GRAY.brighter());
        styles.put(Level.FINER, finer);

        final Style finest = outputPane.addStyle("finest", null);
        StyleConstants.setForeground(finest, Color.GRAY.brighter().brighter());
        styles.put(Level.FINEST, finest);
    }

    @Override
    public void publish(final LogRecord record) {
        final Style style = styles.get(record.getLevel());
//		private static final String PATTERN_DEFAULT = "%d{HH:mm:ss.SSS} %c{1} %m";

        final String message =
                dateFormat.format(new Date(record.getMillis())) + " " +
                        record.getSourceClassName() + "." +
                        record.getSourceMethodName() + "() " + record.getMessage();

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                final Document document = outputPane.getDocument();

                try {
                    final int lastTraceStart = traceStart;
                    if (lastTraceStart > 0) {
                        document.remove(lastTraceStart, document.getLength() - lastTraceStart);
                        traceStart = -1;
                    }
                } catch (BadLocationException e) {
                    //	ignoring: appender should not use logging itself
                }

                try {
                    final int prevLength = document.getLength();

                    document.insertString(
                            prevLength,
                            message + "\n",
                            style.copyAttributes()
                    );

                    if (Level.INFO.intValue() > record.getLevel().intValue()) {
                        traceStart = prevLength;
                    }
                } catch (BadLocationException e) {
                    //	ignoring: appender should not use logging itself
                }
            }
        });
    }

    @Override
    public void flush() {
        //	nothing to do here
    }

    @Override
    public void close() throws SecurityException {
        //	nothing to do
    }
}
