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

import base.pattern.Result;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Message {
    private static final String TYPE_INFO = "info";
    private static final String TYPE_WARN = "warn";
    private static final String TYPE_ERR = "err";

    private static long lastUsedStamp = 0;

    private final long stamp;
    private final String text;
    private final String severity;

    private Message(String text, String severity) {
        this.text = text;
        this.severity = severity;
        synchronized (Message.class) {
            final long currentMillis = System.currentTimeMillis();
            lastUsedStamp =
                    stamp =
                            Math.max(currentMillis, lastUsedStamp + 1);
        }
    }

    public static Message info(String text) { return new Message(text, TYPE_INFO); }
    public static Message warn(String text) { return new Message(text, TYPE_WARN); }
    public static Message err(String text) { return new Message(text, TYPE_ERR); }

    public String getText() {
        return text;
    }

    public String getSeverity() {
        return severity;
    }

    public long getStamp() {
        return stamp;
    }

    private static List<Message> getOrCreateMessages(HttpServletRequest req) {
        final HttpSession session = req.getSession(true);

        @SuppressWarnings("unchecked")
        List<Message> messages =
                (List<Message>) session.getAttribute("elw_messages");

        if (messages == null) {
            messages = new ArrayList<Message>();
            session.setAttribute("elw_messages", messages);
        }

        return messages;
    }

    public static List<Message> getMessages(HttpServletRequest req) {
        synchronized (Message.class) {
            final List<Message> messages = getOrCreateMessages(req);
            return new ArrayList<Message>(messages);
        }
    }

    public static void delete(
            final HttpServletRequest req, final String stamp
    ) {
        synchronized (Message.class) {
            final List<Message> messageList = getOrCreateMessages(req);

            for (Iterator<Message> iterator = messageList.iterator(); iterator.hasNext(); ) {
                Message message = iterator.next();
                if (String.valueOf(message.getStamp()).equals(stamp)) {
                    iterator.remove();
                }
            }
        }
    }


    private static void addMessage(final HttpServletRequest req, final String type, final String message) {
        synchronized (Message.class) {
            getOrCreateMessages(req).add(new Message(message, type));
        }
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

    public static void addResult(final HttpServletRequest req, Result result) {
        if (result.isSuccess()) {
            addInfo(req, result.getMessage());
        } else {
            addErr(req, result.getMessage());
        }
    }
}
