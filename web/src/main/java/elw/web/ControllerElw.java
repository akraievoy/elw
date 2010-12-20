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
import elw.dao.FileDao;
import elw.miniweb.Message;
import elw.vo.Entry;
import elw.vo.FileMeta;
import elw.vo.FileSlot;
import elw.vo.Stamp;
import elw.web.core.Core;
import elw.web.core.W;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;
import org.springframework.web.servlet.support.RequestContextUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public abstract class ControllerElw extends MultiActionController implements WebSymbols {
	private static final Logger log = LoggerFactory.getLogger(ControllerElw.class);

	protected final Core core;

	public ControllerElw(Core core) {
		this.core = core;
	}

	protected HashMap<String, Object> prepareDefaultModel(HttpServletRequest req) {
		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put(S_MESSAGES, Message.drainMessages(req));
		model.put(FormatTool.MODEL_KEY, FormatTool.forLocale(RequestContextUtils.getLocale(req)));
		model.put(VelocityTemplates.MODEL_KEY, core.getTemplates());
		model.put(ElwUri.MODEL_KEY, core.getUri());

		return model;
	}

	protected abstract HashMap<String, Object> auth(
			final HttpServletRequest req,
			final HttpServletResponse resp,
			final String pathToRoot
	) throws IOException;

	protected static interface WebMethodScore {
		ModelAndView handleScore(
				HttpServletRequest req, HttpServletResponse resp, Ctx ctx, FileSlot slot, Entry<FileMeta> file, Stamp stamp, Map<String, Object> model
		) throws IOException;
	}

	protected ModelAndView wmScore(
			HttpServletRequest req, HttpServletResponse resp,
			final String pathToRoot, final WebMethodScore wm
	) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, pathToRoot);
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
			return null;
		}

		final String slotId = req.getParameter("sId");
		if ((slotId == null || slotId.trim().length() == 0)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
			return null;
		}

		final FileSlot slot = ctx.getAssType().findSlotById(slotId);
		if (slot == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "slot for id " + slotId + " not found");
			return null;
		}

		final String fileId = req.getParameter("fId");
		if ((fileId == null || fileId.trim().length() == 0)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no fileId (fId) defined");
			return null;
		}

		final Entry<FileMeta> file = core.getFileDao().findFileFor(FileDao.SCOPE_STUD, ctx, slotId, fileId);
		if (file == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "file for id " + fileId + " not found");
			return null;
		}

		final Stamp stamp = W.parseStamp(req, "stamp");

		return wm.handleScore(req, resp, ctx, slot, file, stamp, model);
	}

	protected static abstract class WebMethodCtx {
		protected HttpServletRequest req;
		protected HttpServletResponse resp;
		protected Ctx ctx;
		protected Map<String, Object> model;

		protected void init(HttpServletRequest req, HttpServletResponse resp, Ctx ctx, Map<String, Object> model) {
			this.ctx = ctx;
			this.model = model;
			this.req = req;
			this.resp = resp;
		}

		public abstract ModelAndView handleCtx() throws IOException;
	}

	protected ModelAndView wmECGS(
			HttpServletRequest req, HttpServletResponse resp,
			final String pathToRoot, final WebMethodCtx wm
	) throws IOException {
		return wm(req, resp, pathToRoot, Ctx.STATE_ECGS, wm);
	}

	protected ModelAndView wmECG(
			HttpServletRequest req, HttpServletResponse resp,
			final String pathToRoot, final WebMethodCtx wm
	) throws IOException {
		return wm(req, resp, pathToRoot, Ctx.STATE_ECG, wm);
	}

	protected ModelAndView wm(
			HttpServletRequest req, HttpServletResponse resp,
			final String pathToRoot, final String ctxResolveState, final WebMethodCtx wm
	) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, pathToRoot);
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(ctxResolveState)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
			return null;
		}

		wm.init(req, resp, ctx, model);
		return wm.handleCtx();
	}


	protected ModelAndView wmFile(
			HttpServletRequest req, HttpServletResponse resp, final String pathToRoot,
			final String scopeForced, final WebMethodFile wm
	) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, pathToRoot);
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);

		wm.init(req, resp, ctx, model);
		wm.setScopeForced(scopeForced);

		return wm.handleCtx();
	}

	protected static abstract class WebMethodFile extends WebMethodCtx {
		protected String scopeForced = null;

		public void setScopeForced(String scopeForced) {
			this.scopeForced = scopeForced;
		}

		public ModelAndView handleCtx() throws IOException {
			final String scope = scopeForced == null ? req.getParameter("s") : scopeForced;
			if (scope == null) {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "scope not set");
				return null;
			}

			if (FileDao.SCOPE_ASS_TYPE.equals(scope)) {
				if (!ctx.resolved(Ctx.STATE_CT)) {
					resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
					return null;
				}
			} else if (FileDao.SCOPE_ASS.equals(scope)) {
				if (!ctx.resolved(Ctx.STATE_CTA)) {
					resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
					return null;
				}
			} else if (FileDao.SCOPE_VER.equals(scope)) {
				if (!ctx.resolved(Ctx.STATE_CTAV)) {
					resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
					return null;
				}
			} else if (FileDao.SCOPE_STUD.equals(scope)) {
				if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
					resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
					return null;
				}
			} else {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "bad scope: " + scope);
				return null;
			}

			final String slotId = req.getParameter("sId");
			if (slotId == null || slotId.trim().length() == 0) {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
				return null;
			}

			final FileSlot slot = ctx.getAssType().findSlotById(slotId);
			if (slot == null) {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "slot '" + slotId + "' not found");
				return null;
			}

			return handleFile(scope, slot);
		}

		protected abstract ModelAndView handleFile(String scope, FileSlot slot) throws IOException;
	}
}
