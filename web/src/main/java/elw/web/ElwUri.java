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
import elw.vo.FileBase;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

public class ElwUri {
    public static final String MODEL_KEY = "elw_uri";

    public String logAnyE(final String enrId) {
        return "log?elw_ctx=e--" + enrId + "&f_scope=s--&f_due=twoweeks&f_mode=s";
    }

    public String logCourseE(final String enrId) {
        return "log?elw_ctx=e--" + enrId + "&f_scope=c--av--&f_due=any";
    }

    public String tasks(final String enrId) {
        return "tasks?elw_ctx=e--" + enrId;
    }

    public String logOpenPendingEAV(final Ctx ctx) {
        return logEAV(ctx, "s--op--", "any") + "&f_mode=dd";
    }

    public String logApprovedDeclinedEAV(final Ctx ctx) {
        return logEAV(ctx, "s--ad--", "any") + "&f_mode=s";
    }

    public String logCourseEAV(final Ctx ctx) {
        return logEAV(ctx, "c--av--", "any");
    }

    public String logEAV(Ctx ctx, String scope, String due) {
        final StringBuilder builder = new StringBuilder("log?");

        builder.append("elw_ctx=e--").append(ctx.getEnr().getId());
        builder.append("&").append("f_verId").append("=").append(ctx.getAss().getId()).append("--");
        if (ctx.resolved(Ctx.STATE_CIV)) {
            builder.append(ctx.getVer().getId()).append("--");
        }
        builder.append("&").append("f_scope").append("=").append(scope);
        builder.append("&").append("f_due").append("=").append(due);

        return builder.toString();
    }

    public String summary(final String enrId) {
        return "summary?elw_ctx=e--" + enrId;
    }

    private String fileQueryForObj(
            final Ctx ctx,
            final String scope,
            final String slotId,
            final FileBase file
    ) {
        return fileQueryForId(
                ctx,
                scope,
                slotId,
                file == null ? null : file.getId()
        );
    }

    private String fileQueryForId(
            final Ctx ctx,
            final String scope,
            final String slotId,
            final String fileId
    ) {
        final String xferQuery =
                "?elw_ctx=" + ctx.toString() + "&s=" + scope + "&sId=" + slotId;

        if (Strings.isNullOrEmpty(fileId)) {
            return xferQuery;
        }

        return xferQuery + "&fId=" + urlEncode(fileId);
    }

    public static String urlEncode(String fileId) {
        try {
            return URLEncoder.encode(fileId, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException("UTF-8 not supported", e);
        }
    }

    public String upload(final Ctx ctx, final String scope, final String slotId) {
        return "ul" + fileQueryForObj(ctx, scope, slotId, null);
    }

    public String download(final Ctx ctx, final String scope, final String slotId, FileBase e, String nameNorm) {
        if (e == null) {
            return null;
        }

        final String name = nameNorm == null ? e.getName() : nameNorm;
        return "dl/" + name + fileQueryForObj(ctx, scope, slotId, e);
    }

    public String approve(final Ctx ctx, final String scope, final String slotId, FileBase e) {
        if (e == null) {
            return null;
        }
        return "approve" + fileQueryForObj(ctx, scope, slotId, e);
    }

    public String edit(final Ctx ctxVer, final String scope, final String slotId, final String fileId) {
        return "edit" + fileQueryForId(ctxVer, scope, slotId, fileId);
    }
}
