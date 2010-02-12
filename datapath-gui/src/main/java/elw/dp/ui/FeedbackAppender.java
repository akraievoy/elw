package elw.dp.ui;

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.spi.LoggingEvent;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class FeedbackAppender extends AppenderSkeleton {
	protected static final String PATTERN_DEFAULT = "%d{HH:mm:ss.SSS} %c{1} %m";

	protected final JTextPane outputPane;
	protected Map<Level, Style> styles = new HashMap<Level, Style>();

	//	please access this value only from Swing worker thread...
	protected int traceStart = -1;

	public FeedbackAppender(JTextPane outputPane) {
		this(outputPane, PATTERN_DEFAULT);
	}

	public FeedbackAppender(JTextPane outputPane, final String patternDefault) {
		this.outputPane = outputPane;

		setLayout(new PatternLayout(patternDefault));

		init();
	}

	protected void init() {
		final Style fatal = outputPane.addStyle("fatal", null);
		StyleConstants.setForeground(fatal, Color.RED.darker().darker());
		StyleConstants.setBold(fatal, true);
		styles.put(Level.FATAL, fatal);

		final Style error = outputPane.addStyle("error", null);
		StyleConstants.setForeground(error, Color.RED.darker());
		StyleConstants.setBold(fatal, true);
		styles.put(Level.ERROR, error);

		final Style warn = outputPane.addStyle("warn", null);
		StyleConstants.setForeground(warn, Color.ORANGE.darker());
		StyleConstants.setBold(warn, true);
		styles.put(Level.WARN, warn);

		final Style info = outputPane.addStyle("info", null);
		StyleConstants.setForeground(info, Color.DARK_GRAY.darker());
		styles.put(Level.INFO, info);

		final Style debug = outputPane.addStyle("debug", null);
		StyleConstants.setForeground(debug, Color.GRAY);
		styles.put(Level.DEBUG, debug);

		final Style trace = outputPane.addStyle("trace", null);
		StyleConstants.setForeground(trace, Color.GRAY.brighter());
		styles.put(Level.TRACE, trace);
	}

	protected void append(final LoggingEvent event) {
		final Style style = styles.get(event.getLevel());
		final String message = getLayout().format(event);

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

					if (Level.DEBUG.isGreaterOrEqual(event.getLevel())) {
						traceStart = prevLength;
					}
				} catch (BadLocationException e) {
					//	ignoring: appender should not use logging itself
				}
			}
		});
	}

	public boolean requiresLayout() {
		return true;
	}

	public void close() {
		//	nothing to do
	}
}
