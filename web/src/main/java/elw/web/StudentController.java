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

import elw.dao.*;
import elw.miniweb.Message;
import elw.miniweb.ViewJackson;
import elw.vo.*;
import org.akraievoy.gear.G4Io;
import org.apache.commons.fileupload.FileUploadException;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;
import org.springframework.web.servlet.support.RequestContextUtils;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.*;
import java.lang.Class;
import java.util.*;

@Controller
@RequestMapping("/s/**/*")
public class StudentController extends MultiActionController implements WebSymbols {
	private static final Logger log = LoggerFactory.getLogger(StudentController.class);

	protected final CourseDao courseDao;
	protected final GroupDao groupDao;
	protected final EnrollDao enrollDao;
	protected final ScoreDao scoreDao;
	protected final FileDao fileDao;
	private final AdminController adminController;

	protected final ObjectMapper mapper = new ObjectMapper();
	protected final long cacheBustingToken = System.currentTimeMillis();

	public StudentController(
			CourseDao courseDao, GroupDao groupDao, EnrollDao enrollDao,
			ScoreDao scoreDao, FileDao fileDao,
			AdminController adminController
	) {
		this.courseDao = courseDao;
		this.groupDao = groupDao;
		this.enrollDao = enrollDao;
		this.scoreDao = scoreDao;
		this.fileDao = fileDao;
		this.adminController = adminController;
	}

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final String pathToRoot) throws IOException {
		final HttpSession session = req.getSession(true);
		Ctx ctx = Ctx.fromString(req.getParameter(R_CTX));

		//	admin impersonation
		final Boolean admin = (Boolean) session.getAttribute(S_ADMIN);
		if (Boolean.TRUE.equals(admin)) {
			if (!ctx.resolve(enrollDao, groupDao, courseDao).resolved(Ctx.STATE_GS)) {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
				return null;
			}

			final HashMap<String, Object> model = prepareDefaultModel(req);

			model.put(R_CTX, ctx);

			return model;
		}

		final Group group = (Group) session.getAttribute(S_GROUP);
		final Student student = (Student) session.getAttribute(S_STUD);
		if (group != null && student != null) {
			ctx.resolve(enrollDao, groupDao, courseDao);
			if (!ctx.resolved(Ctx.ELEM_GROUP)) {
				ctx = ctx.extendGroup(group);
			}
			if (!ctx.resolved(Ctx.ELEM_STUD)) {
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
		model.put(VelocityUtils.MODEL_KEY, VelocityUtils.INSTANCE);
		model.put("expandTriggers", req.getSession().getAttribute("viewToExpandTriggers"));

		return model;
	}

	@RequestMapping(value = "login", method = RequestMethod.GET)
	public ModelAndView do_login(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = prepareDefaultModel(req);

		model.put("groups", groupDao.findAllGroups());
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
			final Group[] groups = groupDao.findAllGroups();
			final Group group = IdName.findByName(groups, groupName.trim(), true);

			if (group != null) {
				final Student[] students = group.getStudents();
				final Student student = IdName.findByName(students, studentName.trim(), true);

				if (student != null) {
					session.setAttribute(S_GROUP, group);
					session.setAttribute(S_STUD, student);

					Message.addInfo(req, "logged on");
					session.removeAttribute("groupName");
					session.removeAttribute("studentName");

					Enrollment[] enrs = enrollDao.findEnrollmentsForGroupId(group.getId());
					if (enrs.length == 1) {
						resp.sendRedirect("course?elw_ctx=e--" + enrs[0].getId());
					} else {
						resp.sendRedirect("courses");
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
		resp.sendRedirect("courses");
		return null;
	}

	@RequestMapping(value = "courses", method = RequestMethod.GET)
	public ModelAndView do_courses(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		final Enrollment[] enrollments = enrollDao.findEnrollmentsForGroupId(ctx.getGroup().getId());

		final Map<String, Course> courses = new TreeMap<String, Course>();
		for (Enrollment enrollment : enrollments) {
			final String courseId = enrollment.getCourseId();
			courses.put(courseId, courseDao.findCourse(courseId));
		}

		model.put("enrolls", enrollments);
		model.put("courses", courses);

		return new ModelAndView("s/courses", model);
	}

	@RequestMapping(value = "course", method = RequestMethod.GET)
	public ModelAndView do_course(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_ECGS)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final HashMap<String, CodeMeta> codeMetas = new HashMap<String, CodeMeta>();
		final HashMap<String, ReportMeta> reportMetas = new HashMap<String, ReportMeta>();
		final HashMap<String, Score> scores = new HashMap<String, Score>();
		final TreeMap<String, Map<String, List<Entry<FileMeta>>>> fileMetas =
				new TreeMap<String, Map<String, List<Entry<FileMeta>>>>();
		final int[] grossScore = new int[1];
		storeMetas(ctx, null, fileDao, fileMetas, grossScore);
		model.put("fileMetas", fileMetas);
		model.put("codeMetas", codeMetas);
		model.put("reportMetas", reportMetas);
		model.put("scores", scores);
		model.put("grossScore", grossScore[0]);

		return new ModelAndView("s/course", model);
	}

	public static void storeMetas(
			Ctx ctx, String aTypeSlotId,
			FileDao fileDao,
			Map<String, Map<String, List<Entry<FileMeta>>>> fileMetas,
			int[] grossScore
	) {
		double gsFuzzy = 0;

		for (int index = 0; index < ctx.getEnr().getIndex().size(); index++) {
			final Ctx ctxVer = ctx.extendIndex(index);
			final AssignmentType assType = ctxVer.getAssType();
			final String assPath = ctxVer.toString();

			final SortedMap<String, List<Entry<FileMeta>>> slotIdToFiles = fileDao.loadFilesStud(ctxVer);

			if (fileMetas != null) {
				fileMetas.put(assPath, slotIdToFiles);
			}

			for (String slotId : slotIdToFiles.keySet()) {
				if (AdminController.W.excluded(aTypeSlotId, assType.getId(), slotId)) {
					continue;
				}

				final List<Entry<FileMeta>> filesForSlot = slotIdToFiles.get(slotId);
				if (filesForSlot != null && !filesForSlot.isEmpty()) {
					for (Entry<FileMeta> e : filesForSlot) {
						if (e.getMeta().getScore() != null && e.getMeta().getScore().isApproved() != null) {
							continue;	//	don't alter any scores once they're approved
						}
						final Score autoScore = AdminController.W.updateAutos(
								ctxVer, slotId, e, e.getMeta().getScore()
						);
						e.getMeta().setScore(autoScore);
					}

					Entry<FileMeta> usedEntry = null;
					double maxScore = 0;
					for (Entry<FileMeta> e : filesForSlot) {
						final double eScore = ctxVer.getIndexEntry().getTotal(ctxVer.getAssType(), e.getMeta().getScore());
						if (usedEntry == null || maxScore < eScore) {
							maxScore = eScore;
							usedEntry = e;
						}
					}

					if (usedEntry != null) {
						final Score s = usedEntry.getMeta().getScore();
						if (s != null) {
							s.setBest(true);
							if (Boolean.TRUE.equals(s.isApproved())) {
								gsFuzzy += maxScore;
							}
						}
					}
				}
			}
		}

		grossScore[0] = (int) Math.floor(gsFuzzy + 0.1);
	}

	@RequestMapping(value = "ul", method = {RequestMethod.POST, RequestMethod.PUT})
	public ModelAndView do_ul(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		final String refreshUri = "course?" + WebSymbols.R_CTX + "=" + Ctx.forEnr(ctx.getEnr()).toString();
		return adminController.processUpload(
				req, resp, FileDao.SCOPE_STUD,
				model, refreshUri, ctx.getStudent().getName()
		);
	}

	@RequestMapping(value = "updateExpandTriggers", method = RequestMethod.POST)
	public ModelAndView do_updateExpandTriggers(
			final HttpServletRequest req, final HttpServletResponse resp
	) throws IOException {
		final HttpSession session = req.getSession();
		@SuppressWarnings({"unchecked"})
		Map<String, String> viewToExpandTriggers = (Map<String, String>) session.getAttribute("viewToExpandTriggers");
		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized(session) {
			if (viewToExpandTriggers == null) {
				viewToExpandTriggers = Collections.synchronizedMap(new TreeMap<String, String>());
				session.setAttribute("viewToExpandTriggers", viewToExpandTriggers);
			}
		}

		final String postBody = G4Io.dumpToString(req.getInputStream(), "UTF-8");
		final String viewName = req.getParameter("view");
		final String viewNameEff = viewName == null ? "" : viewName;
		viewToExpandTriggers.put(viewNameEff, postBody);

		return new ModelAndView(ViewJackson.success("success"));
	}

	@RequestMapping(value = "dl/*.*", method = RequestMethod.GET)
	public ModelAndView do_dl(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "../");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final String scope = req.getParameter("s");
		if (scope == null || scope.trim().length() == 0) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no scope (s) defined");
			return null;
		}

		final String slotId = req.getParameter("sId");
		if (slotId == null || slotId.trim().length() == 0) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
			return null;
		}

		final String fileId = req.getParameter("fId");
		if (fileId == null || fileId.trim().length() == 0) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no fileId (fId) defined");
			return null;
		}

		final Entry<FileMeta> entry = fileDao.findFileFor(scope, ctx, slotId, fileId);
		if (entry == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "no file found");
			return null;
		}

		final SortedMap<String, List<Entry<FileMeta>>> filesStud = fileDao.loadFilesStud(ctx);
		if (slotId != null && !ctx.getVer().checkRead(ctx.getAssType(), ctx.getAss(), slotId, filesStud)) {
			resp.sendError(HttpServletResponse.SC_FORBIDDEN, "not readable yet");
			return null;
		}

		boolean binary;
		if (slotId != null) {
			binary = ctx.getAssType().findSlotById(slotId).isBinary();
		} else {
			binary = entry.getFileBinary() != null;
		}

		try {
			if (binary) {
				resp.setContentType(entry.getMeta().getContentType());
				resp.setContentLength((int) (binary ? entry.getFileBinary() : entry.getFileText()).length());
				resp.setHeader("Content-Disposition", "attachment;");

				final BufferedInputStream bis = entry.openBinaryStream();
				final ServletOutputStream os = resp.getOutputStream();

				final byte[] buf = new byte[32768];
				int numRead;
				while ((numRead = bis.read(buf, 0, buf.length)) > 0) {
					os.write(buf, 0, numRead);
				}
			} else {
				resp.setCharacterEncoding("UTF-8");
				resp.setContentType(entry.getMeta().getContentType()+ "; charset=UTF-8");
				resp.setContentLength((int) (binary ? entry.getFileBinary() : entry.getFileText()).length());
				resp.setHeader("Content-Disposition", "attachment;");

				//	copy the data with the original line breaks (we've set the content-length header)
				final BufferedReader reader = entry.openTextReader();
				final PrintWriter writer = resp.getWriter();

				final char[] buf = new char[16384];
				int numRead;
				while ((numRead = reader.read(buf, 0, buf.length)) > 0) {
					writer.write(buf, 0, numRead);
				}
			}
		} finally {
			entry.closeStreams();
		}

		return null;
	}

	@RequestMapping(value = "edit", method = RequestMethod.GET)
	public ModelAndView do_edit(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final String slotId = req.getParameter("sId");
		if (slotId == null || slotId.trim().length() == 0) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
			return null;
		}

		final FileSlot slot = ctx.getAssType().findSlotById(slotId);

		final SortedMap<String, List<Entry<FileMeta>>> filesStud = fileDao.loadFilesStud(ctx);
		if (!ctx.getVer().checkRead(ctx.getAssType(), ctx.getAss(), slotId, filesStud)) {
			resp.sendError(HttpServletResponse.SC_FORBIDDEN, "not readable yet");
			return null;
		}

		final String customEditName = slot.getEditor();
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

		for (FileSlot slot : ctx.getAssType().getFileSlots()) {
			final HashMap<String, String[]> scopeToFileId = new HashMap<String, String[]>();
			slotToScopeToFileId.put(slot.getId(), scopeToFileId);

			for (String scope : FileDao.SCOPES) {
				final Entry<FileMeta>[] fileEntries = fileDao.findFilesFor(scope, ctx, slot.getId());
				final String[] fileIds = new String[fileEntries.length];

				for (int i = 0, filesLength = fileEntries.length; i < filesLength; i++) {
					fileIds[i] = fileEntries[i].getMeta().getId();
				}

				scopeToFileId.put(scope, fileIds);
			}
		}
		return new ModelAndView(ViewJackson.success(slotToScopeToFileId));
	}
}