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

public class ElwUri {
	public static final String MODEL_KEY = "elw_uri";

	public String logPending(final String enrId) {
		return "log?elw_ctx=e--" + enrId + "&f_scope=s--p--&f_due=today&f_mode=dd";
	}

	public String logOpen(final String enrId) {
		return "log?elw_ctx=e--" + enrId + "&f_scope=s--o--&f_due=twoweeks&f_mode=dd";
	}

	public String logCourse(final String enrId) {
		return "log?elw_ctx=e--" + enrId + "&f_scope=c--av--&f_due=any";
	}

	public String tasks(final String enrId) {
		return "tasks?elw_ctx=e--" + enrId;
	}

	public String logPending(final String enrId, final String assId) {
		return "log?elw_ctx=e--" + enrId + "&f_verId=" + assId + "--&f_scope=s--p--&f_due=today&f_mode=dd";
	}

	public String logOpen(final String enrId, final String assId) {
		return "log?elw_ctx=e--" + enrId + "&f_verId=" + assId + "--&f_scope=s--o--&f_due=twoweeks&f_mode=dd";
	}

	public String logCourse(final String enrId, final String assId) {
		return "log?elw_ctx=e--" + enrId + "&f_verId=" + assId + "--&f_scope=c--av--&f_due=any";
	}

	public String summary(final String enrId) {
		return "summary?elw_ctx=e--"+ enrId;
	}
}
