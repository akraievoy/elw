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

package elw.web.core;

import elw.vo.*;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

public class W {
	public static void storeFilter(HttpServletRequest req, Map<String, Object> model) {
		final Map params = req.getParameterMap();
		for (Object o : params.keySet()) {
			String paramName = (String) o;
			if (paramName.startsWith("f_")) {
				final String value = req.getParameter(paramName);
				if (value != null && value.length() > 0) {
					model.put(paramName, value);
				}
			}
		}
	}

	public static String resolveRemoteAddress(HttpServletRequest req) {
		final String remoteAddr = req.getRemoteAddr();

		final String xff = req.getHeader("X-Forwarded-For");
		if (xff != null && xff.trim().length() > 0) {
			return xff.replaceAll("\\s+", "").split(",")[0];
		}

		return remoteAddr;
	}

	public static boolean excluded(Object filterValue, String actualValue) {
		if (!(filterValue instanceof String)) {
			return false;
		}
		return ((String) filterValue).trim().length() > 0 && !filterValue.equals(actualValue);
	}

	public static void filterDefault(Map<String, Object> model, String fKey, String fDefault) {
		if (model.get(fKey) == null || model.get(fKey).toString().trim().length() == 0) {
			model.put(fKey, fDefault);
		}
	}

	public static boolean excluded(String aTypeSlotFilter, String typeId, String slotId) {
		if (aTypeSlotFilter == null) {
			return false;
		}

		final String typeSlotExpr = typeId + "--" + slotId + "--";
		return !typeSlotExpr.startsWith(aTypeSlotFilter);
	}

	public static Stamp parseStamp(HttpServletRequest req, final String paramName) {
		final String paramVal = req.getParameter(paramName);
		return paramVal != null ? Stamp.fromString(paramVal) : null;
	}

	public static LogFilter parseFilter(HttpServletRequest req) {
		final String due = req.getParameter("f_due");
		final boolean latest = "true".equals(req.getParameter("f_latest"));
		final String slotId = req.getParameter("f_slotId");
		final String verId = req.getParameter("f_verId");
		final String studId = req.getParameter("f_studId");
		final String mode = req.getParameter("f_mode");
		final String scope = req.getParameter("f_scope");
		final LogFilter logFilter = new LogFilter(slotId, studId, verId, due == null ? "any" : due, mode == null ? "s" : mode, scope == null ? "s--opd--" : scope, latest);
		return logFilter;
	}
}
