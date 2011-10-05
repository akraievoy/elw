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

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.springframework.web.servlet.View;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Map;

public class ViewJackson implements View {
    public final static ObjectMapper MAPPER = createMapper();
    private String contentType = "text/json";

    private static ObjectMapper createMapper() {
        final ObjectMapper mapper = new ObjectMapper();

        mapper.getSerializationConfig().enable(SerializationConfig.Feature.INDENT_OUTPUT);

        return mapper;
    }

    private final State state;

    private ViewJackson(State state) {
        this.state = state;
    }

    @Deprecated
    private ViewJackson(Object data) {
        this(new State(true, null, data));
    }

    @Deprecated
    private ViewJackson(String message) {
        this(new State(false, message, null));
    }

    public String getContentType() {
        return contentType;
    }

    public ViewJackson toContentType(String contentType) {
        this.contentType = contentType;
        return this;
    }

    public void render(Map model, HttpServletRequest request, HttpServletResponse resp) throws Exception {
        resp.setContentType(getContentType() + "; charset=UTF-8");
        resp.setCharacterEncoding("UTF-8");

        resp.setHeader("Pragma", "no-cache");
        resp.setHeader("Cache-Control", "no-cache");
        resp.setDateHeader("Expires", System.currentTimeMillis());

        MAPPER.writeValue(resp.getWriter(), state);
    }

    public static ViewJackson success(Object data) {
        return new ViewJackson(data);
    }

    public static ViewJackson failure(String message) {
        return new ViewJackson(message);
    }

    public static class State {
        protected final boolean success;
        protected final String message;
        protected final Object data;

        private State(boolean success, String message, Object data) {
            this.success = success;
            this.message = message;
            this.data = data;
        }

        public boolean isSuccess() {
            return success;
        }

        public String getMessage() {
            return message;
        }

        public Object getData() {
            return data;
        }
    }
}
