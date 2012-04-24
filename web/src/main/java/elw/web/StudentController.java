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
import elw.dao.Queries;
import elw.miniweb.Message;
import elw.miniweb.ViewJackson;
import elw.vo.*;
import elw.web.core.Core;
import elw.web.core.LogFilter;
import elw.web.core.W;
import org.apache.commons.fileupload.FileUploadException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.support.RequestContextUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.lang.Class;
import java.util.*;

@Controller
@RequestMapping("/s/**/*")
public class StudentController extends ControllerElw {
    private static final Logger log = LoggerFactory.getLogger(StudentController.class);

    private Queries queries;

    public StudentController(
            Queries queries,
            Core core
    ) {
        super(core);
        this.queries = queries;
    }

    protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final String pathToRoot) throws IOException {
        final HttpSession session = req.getSession(true);
        Ctx ctx = Ctx.fromString(req.getParameter(R_CTX));

        //	admin impersonation
        final Admin admin = (Admin) session.getAttribute(S_ADMIN);
        if (admin != null) {
            final Group group = (Group) session.getAttribute(S_GROUP);
            final Student student = (Student) session.getAttribute(S_STUD);
            if (ctx.getGroup() == null) {
                ctx = ctx.extendGroup(group);
            }
            if (ctx.getStudent() == null) {
                ctx = ctx.extendStudent(student);
            }

            if (!ctx.resolve(queries).resolved(Ctx.STATE_GS)) {
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
                return null;
            }

            final HashMap<String, Object> model = prepareDefaultModel(req);

            model.put(R_CTX, ctx);
            model.put("elw_admin", admin);

            return model;
        }

        final Group group = (Group) session.getAttribute(S_GROUP);
        final Student student = (Student) session.getAttribute(S_STUD);
        if (group != null && student != null) {
            ctx.resolve(queries);
            if (ctx.getGroup() == null) {
                ctx = ctx.extendGroup(group);
            }
            if (ctx.getStudent() == null) {
                ctx = ctx.extendStudent(student);
            }
            if (!ctx.resolved(Ctx.STATE_GS)) {
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
                return null;
            }
            if (!ctx.getGroup().getId().equals(group.getId())) {
                resp.sendError(HttpServletResponse.SC_FORBIDDEN, "context path refers to another group");
                return null;
            }
            if (!ctx.getStudent().getId().equals(student.getId())) {
                resp.sendError(HttpServletResponse.SC_FORBIDDEN, "context path refers to another student");
                return null;
            }

            final HashMap<String, Object> model = prepareDefaultModel(req);
            model.put(R_CTX, ctx);

            return model;
        }

        if (pathToRoot != null) {
            Message.addWarn(req, "Login required");
            resp.sendRedirect(pathToRoot + "login");
        } else {
            resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Login required");
        }

        return null;
    }

    protected HashMap<String, Object> prepareDefaultModel(HttpServletRequest req) {
        final HashMap<String, Object> model = new HashMap<String, Object>();

        model.put(S_MESSAGES, Message.drainMessages(req));
        model.put(FormatTool.MODEL_KEY, FormatTool.forLocale(RequestContextUtils.getLocale(req)));
        model.put(VelocityTemplates.MODEL_KEY, VelocityTemplates.INSTANCE);
        model.put("expandTriggers", req.getSession().getAttribute("viewToExpandTriggers"));

        return model;
    }

    @RequestMapping(value = "Index", method = RequestMethod.GET)
    public void do_Index(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        resp.sendRedirect("index");
    }

    @RequestMapping(value = "courses", method = RequestMethod.GET)
    public void do_courses(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        resp.sendRedirect("index");
    }

    @RequestMapping(value = "course", method = RequestMethod.GET)
    public void do_course(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        resp.sendRedirect("course");
    }

    @RequestMapping(value = "login", method = RequestMethod.GET)
    public ModelAndView do_login(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = prepareDefaultModel(req);

        model.put("groups", queries.groups());
        model.put("groupName", req.getSession(true).getAttribute("groupName"));
        model.put("studentName", req.getSession(true).getAttribute("studentName"));

        return new ModelAndView("s/login", model);
    }

    @RequestMapping(value = "loginPost", method = RequestMethod.POST)
    public ModelAndView do_loginPost(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final String groupName = req.getParameter("groupName");
        final String studentName = req.getParameter("studentName");

        final HttpSession session = req.getSession(true);

        if (
                groupName == null || studentName == null ||
                        groupName.trim().length() == 0 || studentName.trim().length() == 0
                ) {
            Message.addErr(req, "fields NOT set");
        } else {
            final List<Group> groups = queries.groups();
            final Group group = IdNamed._.findByName(groups, groupName.trim(), true);

            if (group != null) {
                final Collection<Student> students = group.getStudents().values();
                final Student student = IdNamed._.findByName(students, studentName.trim(), true);

                if (student != null) {
                    session.setAttribute(S_GROUP, group);
                    session.setAttribute(S_STUD, student);

                    Message.addInfo(req, "logged on");
                    session.removeAttribute("groupName");
                    session.removeAttribute("studentName");

                    List<Enrollment> enrs = queries.enrollmentsForGroup(group.getId());
                    if (enrs.size() == 1) {
                        resp.sendRedirect("tasks?elw_ctx=e--" + enrs.get(0).getId());
                    } else {
                        resp.sendRedirect("index");
                    }
                    return null;
                } else {
                    Message.addErr(req, "no such student");
                }
            } else {
                Message.addErr(req, "no such group");
            }
        }

        session.setAttribute("groupName", groupName);
        session.setAttribute("studentName", studentName);

        resp.sendRedirect("login");
        return null;
    }

    @RequestMapping(value = "logout", method = RequestMethod.GET)
    public ModelAndView do_logout(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        req.getSession(true).invalidate();
        resp.sendRedirect("index");
        return null;
    }

    @RequestMapping(value = "index", method = RequestMethod.GET)
    public ModelAndView do_index(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, "");
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);

        return new ModelAndView("s/index", model);
    }

    @RequestMapping(value = "rest/index", method = RequestMethod.GET)
    public ModelAndView do_restIndex(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);
        final List<Enrollment> enrolls = queries.enrollmentsForGroup(ctx.getGroup().getId());
        final List<Object[]> indexData = core.index(enrolls);

        return new ModelAndView(ViewJackson.success(indexData));
    }

    @RequestMapping(value = "tasks", method = RequestMethod.GET)
    public ModelAndView do_tasks(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmECG(req, resp, "", new WebMethodCtx() {
            public ModelAndView handleCtx() throws IOException {
                W.storeFilter(req, model);
                return new ModelAndView("s/tasks", model);
            }
        });
    }

    @RequestMapping(value = "rest/tasks", method = RequestMethod.GET)
    public ModelAndView do_restTasks(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmECG(req, resp, null, new WebMethodCtx() {
            public ModelAndView handleCtx() throws IOException {
                final Format format = (Format) model.get(FormatTool.MODEL_KEY);

                final List<Object[]> logData = core.tasks(ctx, new LogFilter(), format, false);

                return new ModelAndView(ViewJackson.success(logData));
            }
        });
    }

    @RequestMapping(value = "log", method = RequestMethod.GET)
    public ModelAndView do_log(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, "");
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);
        if (!ctx.resolved(Ctx.STATE_ECG)) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
            return null;
        }

        W.storeFilter(req, model);

        return new ModelAndView("s/log", model);
    }

    @RequestMapping(value = "rest/log", method = RequestMethod.GET)
    public ModelAndView do_restLog(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);
        if (!ctx.resolved(Ctx.STATE_ECG)) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
            return null;
        }

        final Format format = (Format) model.get(FormatTool.MODEL_KEY);

        final List<Object[]> logData = core.log(
                ctx, format, W.parseFilter(req), false
        );

        return new ModelAndView(ViewJackson.success(logData));
    }

    @RequestMapping(value = "list", method = RequestMethod.GET)
    public ModelAndView do_list(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, "");
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);
        if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
            return null;
        }

        final Map<String, Map<String, String[]>> slotToScopeToFileId =
                new HashMap<String, Map<String, String[]>>();

        for (FileSlot slot : ctx.getAssType().getFileSlots().values()) {
            final HashMap<String, String[]> scopeToFileId = new HashMap<String, String[]>();
            slotToScopeToFileId.put(slot.getId(), scopeToFileId);

            for (String scope : FileBase.SCOPES) {
                final List<? extends FileBase> fileEntries =
                        queries.files(scope, ctx, slot);
                final String[] fileIds = new String[fileEntries.size()];

                for (int i = 0, filesLength = fileEntries.size(); i < filesLength; i++) {
                    fileIds[i] = fileEntries.get(i).getId();
                }

                scopeToFileId.put(scope, fileIds);
            }
        }
        return new ModelAndView(ViewJackson.success(slotToScopeToFileId));
    }

    @RequestMapping(value = "dl/*.*", method = RequestMethod.GET)
    public ModelAndView do_dl(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmFile(req, resp, "../", null, new WebMethodFile() {
            @Override
            protected ModelAndView handleFile(String scope, FileSlot slot) throws IOException {
                if (accessDenied(resp, ctx, scope, slot, false)) {
                    return null;
                }

                final String fileId = req.getParameter("fId");
                if (fileId == null || fileId.trim().length() == 0) {
                    resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no fileId (fId) defined");
                    return null;
                }

                final FileBase entry = queries.file(scope, ctx, slot, fileId);
                if (entry == null) {
                    resp.sendError(HttpServletResponse.SC_NOT_FOUND, "no file found");
                    return null;
                }

                retrieveFile(entry, slot, queries);

                return null;
            }
        });
    }

    @RequestMapping(value = "edit", method = RequestMethod.GET)
    public ModelAndView do_edit(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
        return wmFile(req, resp, "", Solution.SCOPE, new WebMethodFile() {
            @Override
            protected ModelAndView handleFile(String scope, FileSlot slot) throws IOException {
                if (accessDenied(resp, ctx, scope, slot, true)) {
                    return null;
                }

                final String customEditName = IdNamed._.one(slot.getFileTypes()).getEditor();
                if (customEditName == null || customEditName.trim().length() == 0) {
                    resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "editor not set for slot " + slot.getId());
                    return null;
                }

                Editor editor;
                try {
                    editor = (Editor) Class.forName(customEditName).newInstance();
                } catch (InstantiationException e) {
                    log.error("failed for ctx " + ctx + " and slotId " + slot.getId(), e);
                    resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, String.valueOf(e));
                    return null;
                } catch (IllegalAccessException e) {
                    log.error("failed for ctx " + ctx + " and slotId " + slot.getId(), e);
                    resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, String.valueOf(e));
                    return null;
                } catch (ClassNotFoundException e) {
                    log.error("failed for ctx " + ctx + " and slotId " + slot, e);
                    resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, String.valueOf(e));
                    return null;
                }

                model.put("slot", slot);
                model.put("editor", editor.render(req, resp, ctx, slot));

                return new ModelAndView("s/edit", model);
            }
        });
    }

    @RequestMapping(value = "ul", method = {RequestMethod.GET})
    public ModelAndView do_ul(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
        return wmFile(req, resp, "", Solution.SCOPE, new WebMethodFile() {
            @Override
            protected ModelAndView handleFile(String scope, FileSlot slot) throws IOException {
                model.put("slot", slot);
                return new ModelAndView("s/ul", model);
            }
        });
    }

    @RequestMapping(value = "ul", method = {RequestMethod.POST, RequestMethod.PUT})
    public ModelAndView do_ulPost(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
        return wmFile(req, resp, "", Solution.SCOPE, new WebMethodFile() {
            @Override
            protected ModelAndView handleFile(String scope, FileSlot slot) throws IOException {
                if (accessDenied(resp, ctx, scope, slot, true)) {
                    return null;
                }

                final String refreshUri = core.getUri().logPendingEAV(ctx);
                final String failureUri = core.getUri().upload(ctx, scope, slot.getId());

                return storeFile(
                        slot, refreshUri, failureUri, ctx.getStudent().getName(),
                        queries, new Solution()
                );
            }
        });
    }

    private boolean accessDenied(
            HttpServletResponse resp, Ctx ctx, String scope, FileSlot slot, boolean checkWrite
    ) throws IOException {
        if (!ctx.cFrom().isStarted()) {
            resp.sendError(HttpServletResponse.SC_FORBIDDEN, "task not yet open");
            return true;
        }

        if (Solution.SCOPE.equals(scope)) {
            final SortedMap<String, List<Solution>> filesStud = queries.solutions(ctx);
            if (!ctx.checkRead(slot, filesStud)) {
                resp.sendError(HttpServletResponse.SC_FORBIDDEN, "not readable yet");
                return true;
            }
            if (checkWrite && !ctx.checkWrite(slot, filesStud)) {
                resp.sendError(HttpServletResponse.SC_FORBIDDEN, "not writable yet");
                return true;
            }
        }

        return false;
    }

}