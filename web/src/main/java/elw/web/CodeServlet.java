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

package elw.web;

import org.akraievoy.gear.G4Io;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

public class CodeServlet extends HttpServlet {
	private static final Logger log = LoggerFactory.getLogger(CodeServlet.class);

	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		doGetOrHead(req, resp, true);
	}

	protected void doHead(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		doGetOrHead(req, resp, false);
	}

	protected void doGetOrHead(HttpServletRequest req, HttpServletResponse resp, final boolean doCopy) throws IOException {
		final String pathInfo = req.getPathInfo();

		if (pathInfo == null || !pathInfo.endsWith(".jar")) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}

		final int lastSlashIdx = pathInfo.lastIndexOf("/");
		final String slashFileName = pathInfo.substring(lastSlashIdx);

		final String filePath = getServletContext().getRealPath(req.getServletPath() + slashFileName);
		final File codeFile = new File(filePath);
		if (!codeFile.exists()) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}

		resp.setContentType("application/java-archive");
		resp.setHeader("Cache-Control", "public");
		resp.setDateHeader("Expires", System.currentTimeMillis() + 1000 * 60 * 60 * 24);
		resp.setDateHeader("Last-Modified", codeFile.lastModified());
		resp.setHeader("Content-Length", String.valueOf(codeFile.length()));

		if (doCopy) {
			FileInputStream fis = null;
			try {
				fis = new FileInputStream(codeFile);

				G4Io.pumpData(fis, resp.getOutputStream());
			} finally {
				if (fis != null) {
					try {
						fis.close();
					} catch (IOException e) {
						log.debug("failed on close", e);
					}
				}
			}
		}
	}
}
