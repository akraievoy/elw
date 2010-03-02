package elw.miniweb;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Message {
	public static final String TYPE_INFO = "info";
	public static final String TYPE_WARN = "warn";
	public static final String TYPE_ERR = "err";

	protected String message;
	protected String type;

	public Message(String message, String type) {
		this.message = message;
		this.type = type;
	}

	public String getMessage() {
		return message;
	}

	public String getType() {
		return type;
	}

	public static List<Message> getMessages(HttpServletRequest req) {
		final HttpSession session = req.getSession(true);

		List<Message> messages;
		synchronized (Message.class) {
			messages = (List<Message>) session.getAttribute("elw_messages");

			if (messages == null) {
				messages = Collections.synchronizedList(new ArrayList<Message>());
				session.setAttribute("elw_messages", messages);
			}
		}

		return messages;
	}

	public static Message[] drainMessages(HttpServletRequest req) {
		final List<Message> messages = getMessages(req);

		final Message[] result = messages.toArray(new Message[messages.size()]);

		messages.clear();

		return result;
	}

	public static void addMessage(final HttpServletRequest req, final String type, final String message) {
		getMessages(req).add(new Message(message, type));
	}

	public static void addInfo(final HttpServletRequest req, final String message) {
		addMessage(req, TYPE_INFO, message);
	}

	public static void addWarn(final HttpServletRequest req, final String message) {
		addMessage(req, TYPE_WARN, message);
	}

	public static void addErr(final HttpServletRequest req, final String message) {
		addMessage(req, TYPE_ERR, message);
	}
}
