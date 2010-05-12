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

	protected static boolean isActionUri(ServletRequest request) {
		if (!(request instanceof HttpServletRequest)) {
			return false;
		}

		final HttpServletRequest req = (HttpServletRequest) request;
		final String uri = req.getRequestURI();
		final String resource = uri.contains("/") ? uri.substring(uri.lastIndexOf("/") + 1) : uri;

		return !resource.contains(".");
	}
}
