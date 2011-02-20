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

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

public class HtmlContentTypeFilter implements Filter {
	public void destroy() {
		// nothing to do
	}

	public void init(FilterConfig filterConfig) throws ServletException {
		//	nothing to do
	}

	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		if (isActionUri(request)) {
			response.setContentType("text/html");
		}
		chain.doFilter(request, response);
	}

	private static boolean isActionUri(ServletRequest request) {
		if (!(request instanceof HttpServletRequest)) {
			return false;
		}

		final HttpServletRequest req = (HttpServletRequest) request;
		final String uri = req.getRequestURI();
		final String resource = uri.contains("/") ? uri.substring(uri.lastIndexOf("/") + 1) : uri;

		return !resource.contains(".");
	}
}
