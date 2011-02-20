/*
 * ELW : e-learning workspace
 * Copyright (C) 2010  Anton Kraievoy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package elw.miniweb;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Message {
	private static final String TYPE_INFO = "info";
	private static final String TYPE_WARN = "warn";
	private static final String TYPE_ERR = "err";

	private String message;
	private String type;

	private Message(String message, String type) {
		this.message = message;
		this.type = type;
	}

	public String getMessage() {
		return message;
	}

	public String getType() {
		return type;
	}

	private static List<Message> getMessages(HttpServletRequest req) {
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

	private static void addMessage(final HttpServletRequest req, final String type, final String message) {
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
