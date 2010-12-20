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

import elw.dao.Ctx;
import elw.vo.Entry;
import elw.vo.FileMeta;

public class ElwUri {
	public static final String MODEL_KEY = "elw_uri";

	public String logPendingE(final String enrId) {
		return "log?elw_ctx=e--" + enrId + "&f_scope=s--p--&f_due=twoweeks&f_mode=dd";
	}

	public String logOpenE(final String enrId) {
		return "log?elw_ctx=e--" + enrId + "&f_scope=s--o--&f_due=twoweeks&f_mode=dd";
	}

	public String logCourseE(final String enrId) {
		return "log?elw_ctx=e--" + enrId + "&f_scope=c--av--&f_due=any";
	}

	public String tasks(final String enrId) {
		return "tasks?elw_ctx=e--" + enrId;
	}

	public String logPendingEA(final Ctx ctx) {
		return "log?elw_ctx=e--" + ctx.getEnr().getId() + "&f_verId=" + ctx.getAss().getId() + "--&f_scope=s--p--&f_due=twoweeks&f_mode=dd";
	}

	public String logOpenEA(final Ctx ctx) {
		return "log?elw_ctx=e--" + ctx.getEnr().getId() + "&f_verId=" + ctx.getAss().getId() + "--&f_scope=s--o--&f_due=twoweeks&f_mode=dd";
	}

	public String logCourseEA(final Ctx ctx) {
		return "log?elw_ctx=e--" + ctx.getEnr().getId() + "&f_verId=" + ctx.getAss().getId() + "--&f_scope=c--av--&f_due=any";
	}

	public String logPendingEAV(final Ctx ctx) {
		return "log?elw_ctx=e--" + ctx.getEnr().getId() + "&f_verId=" + ctx.getAss().getId() + "--" + ctx.getVer().getId() + "--&f_scope=s--p--&f_due=twoweeks&f_mode=dd";
	}

	public String logOpenEAV(final Ctx ctx) {
		return "log?elw_ctx=e--" + ctx.getEnr().getId() + "&f_verId=" + ctx.getAss().getId() + "--" + ctx.getVer().getId() + "--&f_scope=s--o--&f_due=twoweeks&f_mode=dd";
	}

	public String logCourseEAV(final Ctx ctx) {
		return "log?elw_ctx=e--" + ctx.getEnr().getId() + "&f_verId=" + ctx.getAss().getId() + "--" + ctx.getVer().getId() + "--&f_scope=c--av--&f_due=any";
	}

	public String summary(final String enrId) {
		return "summary?elw_ctx=e--" + enrId;
	}

	public String fileQuery(final Ctx ctx, final String scope, final String slotId, FileMeta file) {
		final String xferQuery = "?elw_ctx=" + ctx.toString() + "&s=" + scope + "&sId=" + slotId;

		if (file != null) {
			return xferQuery + "&fId=" + file.getId();
		}
		return xferQuery;
	}

	public String upload(final Ctx ctx, final String scope, final String slotId, Entry<FileMeta> e) {
		return "ul" + fileQuery(ctx, scope, slotId, e == null ? null : e.getMeta());
	}

	public String download(final Ctx ctx, final String scope, final String slotId, Entry<FileMeta> e) {
		if (e == null) {
			return null;
		}
		return "dl" + fileQuery(ctx, scope, slotId, e == null ? null : e.getMeta());
	}

	public String approve(final Ctx ctx, final String scope, final String slotId, Entry<FileMeta> e) {
		if (e == null) {
			return null;
		}
		return "approve" + fileQuery(ctx, scope, slotId, e.getMeta());
	}
}
