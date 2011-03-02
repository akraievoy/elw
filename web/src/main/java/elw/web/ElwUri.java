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

import com.google.common.base.Strings;
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
		return logEAV(ctx, "s--p--", "set") +"&f_mode=dd";
	}

	public String logAnyEAV(final Ctx ctx) {
		return logEAV(ctx, "s--", "set") +"&f_mode=dd";
	}

	public String logOpenEAV(final Ctx ctx) {
		return logEAV(ctx, "s--o--", "set") +"&f_mode=dd";
	}

	public String logCourseEAV(final Ctx ctx) {
		return logEAV(ctx, "c--av--", "any");
	}

	public String logEAV(Ctx ctx, String scope, String due) {
		return "log?elw_ctx=e--" + ctx.getEnr().getId() + "&f_verId=" + ctx.getAss().getId() + "--" + ctx.getVer().getId() + "--&f_scope="+ scope +"&f_due="+ due;
	}

	public String summary(final String enrId) {
		return "summary?elw_ctx=e--" + enrId;
	}

	private String fileQuery(final Ctx ctx, final String scope, final String slotId, FileMeta file) {
		final String xferQuery = "?elw_ctx=" + ctx.toString() + "&s=" + scope + "&sId=" + slotId;

		if (file != null && !Strings.isNullOrEmpty(file.getId())) {
			return xferQuery + "&fId=" + file.getId();
		}

		return xferQuery;
	}

	public String upload(final Ctx ctx, final String scope, final String slotId) {
		return "ul" + fileQuery(ctx, scope, slotId, null);
	}

	public String download(final Ctx ctx, final String scope, final String slotId, Entry<FileMeta> e, String nameNorm) {
		if (e == null) {
			return null;
		}

		final String name = nameNorm == null ? e.getMeta().getName() : nameNorm;
		return "dl/" + name + fileQuery(ctx, scope, slotId, e.getMeta());
	}

	public String approve(final Ctx ctx, final String scope, final String slotId, Entry<FileMeta> e) {
		if (e == null) {
			return null;
		}
		return "approve" + fileQuery(ctx, scope, slotId, e.getMeta());
	}

	public String edit(final Ctx ctxVer, final String scope, final String slotId, final String fileId) {
		return "edit?elw_ctx=" + ctxVer.toString() + "&s=" + scope + "&sId=" + slotId + "&fId=" + fileId;
	}
}
