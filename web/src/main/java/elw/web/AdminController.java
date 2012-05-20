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
import elw.dao.Auth;
import elw.dao.Ctx;
import elw.dao.Queries;
import elw.dao.ctx.CtxSolution;
import elw.miniweb.ViewJackson;
import elw.vo.*;
import elw.web.core.Core;
import elw.web.core.IndexRow;
import elw.web.core.LogFilter;
import elw.web.core.W;
import org.akraievoy.base.Parse;
import org.apache.commons.fileupload.FileUploadException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

@Controller
@RequestMapping("/a/**/*")
public class AdminController extends ControllerElw {
    private static final Logger log = LoggerFactory.getLogger(AdminController.class);

    private final Queries queries;

    public AdminController(
            Queries queries,
            Core core,
            ElwServerConfig elwServerConfig
    ) {
        super(core, elwServerConfig);
        this.queries = queries;
    }

    protected HashMap<String, Object> auth(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            final boolean page,
            final boolean verified
    ) throws IOException {
        final HashMap<String, Object> model =
                super.auth(req, resp, page, verified);

        if (model == null) {
            return null;
        }

        model.put(
                R_CTX,
                Ctx.fromString(req.getParameter(R_CTX)).resolve(queries)
        );

        return model;
    }

    @Override
    protected String extraAuthValidations(Auth auth) {
        if (!auth.isAdm()) {
            return "Admin Auth Required";
        }

        return null;
    }

    @RequestMapping(value = "logout", method = RequestMethod.GET)
    public ModelAndView do_logout(
            final HttpServletRequest req,
            final HttpServletResponse resp
    ) throws IOException {
        req.getSession(true).invalidate();
        resp.sendRedirect("index");
        return null;
    }

    @RequestMapping(value = "index", method = RequestMethod.GET)
    public ModelAndView do_index(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, true, false);
        if (model == null) {
            return null;
        }

        return new ModelAndView("a/index", model);
    }

    @RequestMapping(value = "rest/index", method = RequestMethod.GET)
    public ModelAndView do_restIndex(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, false, false);
        if (model == null) {
            return null;
        }

        final List<Enrollment> enrolls = core.getQueries().enrollments();
        final List<IndexRow> indexData = core.index(enrolls);
        return new ModelAndView(ViewJackson.success(indexData));
    }

    @RequestMapping(value = "summary", method = RequestMethod.GET)
    public ModelAndView do_summary(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmECG(req, resp, true, false, new WebMethodCtx() {
            @Override
            public ModelAndView handleCtx() throws IOException {
                W.storeFilter(req, model);
                W.filterDefault(model, "f_mode", "a");
                final LogFilter filter = W.parseFilter(req);

                final SortedMap<String, Map<String, List<Solution>>> fileMetas =
                        new TreeMap<String, Map<String, List<Solution>>>();
                final Map<String, Double> ctxToScore = new HashMap<String, Double>();

                core.tasksData(ctx, filter, true, fileMetas, ctxToScore, null);

                model.put("fileMetas", fileMetas);
                model.put("ctxToScore", ctxToScore);
                model.put("totalBudget", ctx.getEnr().cmpTotalBudget());

                return new ModelAndView("a/summary", model);
            }
        });
    }

    @RequestMapping(value = "log", method = RequestMethod.GET)
    public ModelAndView do_log(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmECG(req, resp, true, false, new WebMethodCtx() {
            public ModelAndView handleCtx() throws IOException {
                W.storeFilter(req, model);
                return new ModelAndView("a/log", model);
            }
        });
    }

    @RequestMapping(value = "rest/log", method = RequestMethod.GET)
    public ModelAndView do_restLog(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmECG(req, resp, false, false, new WebMethodCtx() {
            public ModelAndView handleCtx() throws IOException {
                final Format format = (Format) model.get(FormatTool.MODEL_KEY);
                final List<Object[]> logData = core.log(
                        ctx, format, W.parseFilter(req), true
                );

                return new ModelAndView(ViewJackson.success(logData));
            }
        });
    }

    @RequestMapping(value = "tasks", method = RequestMethod.GET)
    public ModelAndView do_tasks(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmECG(req, resp, true, false, new WebMethodCtx() {
            public ModelAndView handleCtx() throws IOException {
                W.storeFilter(req, model);
                return new ModelAndView("a/tasks", model);
            }
        });
    }

    @RequestMapping(value = "rest/tasks", method = RequestMethod.GET)
    public ModelAndView do_restTasks(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmECG(req, resp, false, false, new WebMethodCtx() {
            public ModelAndView handleCtx() throws IOException {
                final Format format = (Format) model.get(FormatTool.MODEL_KEY);

                final List<Object[]> logData = core.tasks(
                        ctx, W.parseFilter(req), format, true
                );

                return new ModelAndView(ViewJackson.success(logData));
            }
        });
    }

    @RequestMapping(value = "approve", method = RequestMethod.GET)
    public ModelAndView do_approve(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmScore(req, resp, true, false, new WebMethodScore() {
            public ModelAndView handleScore(
                    HttpServletRequest req,
                    HttpServletResponse resp,
                    Ctx ctx,
                    FileSlot slot,
                    Solution file,
                    Long stamp,
                    Map<String, Object> model
            ) {
                final SortedMap<Long, Score> allScores =
                        core.getQueries().scores(ctx, slot, file);

                final long stampEff =
                        stamp == null ? allScores.lastKey() : stamp;
                final Score score =                          allScores.get(stampEff);

                model.put("stamp", stamp);
                model.put("score", score);
                model.put("slot", slot);
                model.put("file", file);

                return new ModelAndView("a/approve", model);
            }
        });
    }

    @RequestMapping(value = "rest/scoreLog", method = RequestMethod.GET)
    public ModelAndView do_restScoreLog(
            final HttpServletRequest req,
            final HttpServletResponse resp
    ) throws IOException {
        return wmScore(req, resp, false, false, new WebMethodScore() {
            public ModelAndView handleScore(
                    HttpServletRequest req,
                    HttpServletResponse resp,
                    Ctx ctx,
                    FileSlot slot,
                    Solution file,
                    Long stamp,
                    Map<String, Object> model
            ) {
                final Format f = (Format) model.get(FormatTool.MODEL_KEY);

                final SortedMap<Long, Score> allScores =
                        core.getQueries().scores(ctx, slot, file);

                final String mode = req.getParameter("f_mode");

                final List<Object[]> logData =
                        core.logScore(allScores, ctx, slot, file, f, mode, stamp);

                return new ModelAndView(ViewJackson.success(logData));
            }
        });
    }

    @RequestMapping(value = "approve", method = RequestMethod.POST)
    public ModelAndView do_approvePost(
            final HttpServletRequest req,
            final HttpServletResponse resp
    ) throws IOException {
        return wmScore(req, resp, false, true, new WebMethodScore() {
            public ModelAndView handleScore(
                    HttpServletRequest req,
                    HttpServletResponse resp,
                    Ctx ctx, FileSlot slot,
                    Solution file, Long stamp,
                    Map<String, Object> model
            ) throws IOException {
                final CtxSolution ctxSolution =
                        ctx.ctxSlot(slot).solution(file);

                final long stampEff =
                        stamp == null ? Long.MAX_VALUE : stamp;

                final Score score = core.getQueries().score(
                        ctxSolution, stampEff
                );

                final String action =
                        Strings.nullToEmpty(req.getParameter("action"));
                final List<String> validActions =
                        Arrays.asList("next", "approve", "decline");
                if (!validActions.contains(action.toLowerCase())) {
                    resp.sendError(
                            HttpServletResponse.SC_BAD_REQUEST,
                            "Bad action: " + action
                    );
                    return null;
                }

                if ("approve".equalsIgnoreCase(action)
                        || "decline".equalsIgnoreCase(action)) {
                    score.setApproved("approve".equalsIgnoreCase(action));

                    final Map<String, Integer> pows =
                            new TreeMap<String, Integer>(score.getPows());
                    final Map<String, Double> ratios =
                            new TreeMap<String, Double>(score.getRatios());

                    for (Criteria cri : slot.getCriterias().values()) {
                        final int powDef = score.getPow(slot, cri);
                        final double ratioDef = score.getRatio(slot, cri);

                        final String idFor = Score.idFor(slot, cri);
                        final String powReq = req.getParameter(idFor);
                        final String ratioReq = req.getParameter(idFor + "--ratio");

                        pows.put(idFor, Parse.oneInt(powReq, powDef));
                        ratios.put(idFor, Parse.oneDouble(ratioReq, ratioDef));
                    }

                    score.setPows(pows);
                    score.setRatios(ratios);
                    score.setComment(req.getParameter("comment"));
                    queries.createScore(ctxSolution, score);
                }

                resp.sendRedirect(
                        core.cmpForwardToEarliestPendingSince(
                                ctx, slot, file.getStamp()
                        )
                );

                return null;
            }
        });
    }

    @RequestMapping(value = "groups", method = RequestMethod.GET)
    public ModelAndView do_groups(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, true, false);
        if (model == null) {
            return null;
        }

        final List<Group> groups = core.getQueries().groups();

        model.put("groups", groups);

        return new ModelAndView("a/groups", model);
    }

    @RequestMapping(value = "ul", method = {RequestMethod.GET})
    public ModelAndView do_ul(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
        return wmFile(req, resp, null, true, false, new WebMethodFile() {
            @Override
            protected ModelAndView handleFile(String scope, FileSlot slot) throws IOException {
                model.put("slot", slot);
                final Ctx ctx = (Ctx) model.get("elw_ctx");
                model.put(
                        "elw_ctx_type",
                        Ctx.forEnr(ctx.getEnr())
                                .extCourse(ctx.getCourse())
                                .extIndexEntry(ctx.getIndexEntry())
                );
                return new ModelAndView("a/ul", model);
            }
        });
    }

    @RequestMapping(value = "ul", method = RequestMethod.POST)
    public ModelAndView do_ulPost(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
        return wmFile(req, resp, null, false, true, new WebMethodFile() {
            @Override
            protected ModelAndView handleFile(String scope, FileSlot slot) throws IOException {
                final String failureUri = core.getUri().upload(ctx, scope, slot.getId());
                final String refreshUri = core.getUri().logCourseE(ctx.getEnr().getId());
                final String authorName = auth(model).getAdmin().getName();

                return storeFile(
                        slot, refreshUri, failureUri, authorName,
                        core.getQueries(), new Attachment()
                );
            }
        });
    }

    @RequestMapping(value = "dl/*.*", method = RequestMethod.GET)
    public ModelAndView do_dl(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmFile(req, resp, null, false, true, new WebMethodFile() {
            @Override
            protected ModelAndView handleFile(String scope, FileSlot slot) throws IOException {
                final String fileId = req.getParameter("fId");
                if (fileId == null || fileId.trim().length() == 0) {
                    resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no fileId (fId) defined");
                    return null;
                }

                final FileBase entry = core.getQueries().file(scope, ctx, slot, fileId);
                if (entry == null) {
                    resp.sendError(HttpServletResponse.SC_NOT_FOUND, "no file found");
                    return null;
                }

                retrieveFile(entry, slot, core.getQueries());

                return null;
            }
        });
    }
}

