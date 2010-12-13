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

import base.pattern.Result;
import elw.dao.*;
import elw.dp.mips.MipsValidator;
import elw.miniweb.Message;
import elw.miniweb.ViewJackson;
import elw.vo.*;
import elw.vo.Class;
import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Str;
import org.akraievoy.gear.G4mat;
import org.apache.commons.fileupload.FileItemIterator;
import org.apache.commons.fileupload.FileItemStream;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;
import org.springframework.web.servlet.support.RequestContextUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

@Controller
@RequestMapping("/a/**/*")
public class AdminController extends MultiActionController implements WebSymbols {
	private static final Logger log = LoggerFactory.getLogger(AdminController.class);

	protected static final int NONCE_TIMEOUT_MILLIS = 10000;
	protected static final String PASSWORD = System.getProperty("elw.admin.password", "swordfish");

	protected final CourseDao courseDao;
	protected final GroupDao groupDao;
	protected final EnrollDao enrollDao;
	protected final ScoreDao scoreDao;
	protected final FileDao fileDao;

	protected final ObjectMapper mapper = new ObjectMapper();
	protected final long cacheBustingToken = System.currentTimeMillis();

	protected final DiskFileItemFactory fileItemFactory;

	public AdminController(
			CourseDao courseDao, EnrollDao enrollDao, GroupDao groupDao, ScoreDao scoreDao, FileDao fileDao
	) {
		this.courseDao = courseDao;
		this.enrollDao = enrollDao;
		this.groupDao = groupDao;
		this.scoreDao = scoreDao;
		this.fileDao = fileDao;

		fileItemFactory = new DiskFileItemFactory();
		fileItemFactory.setRepository(new java.io.File(System.getProperty("java.io.tmpdir")));
		fileItemFactory.setSizeThreshold(2 * 1024 * 1024);
	}

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final String pathToRoot) throws IOException {
		final HttpSession session = req.getSession(true);
		final Boolean admin = (Boolean) session.getAttribute(S_ADMIN);

		if (!Boolean.TRUE.equals(admin)) {
			if (pathToRoot != null) {
				Message.addWarn(req, "Admin authentication required");
				if (req.getQueryString() != null) {
					session.setAttribute("loginTo", req.getRequestURI() + "?" + req.getQueryString());
				} else {
					session.setAttribute("loginTo", req.getRequestURI());
				}
				resp.sendRedirect(pathToRoot + "login");
			} else {
				resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Admin authentication required");
			}

			return null;
		}

		session.removeAttribute("loginTo");	//	LATER extract constant

		final HashMap<String, Object> model = prepareDefaultModel(req);
		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));
		model.put(R_CTX, Ctx.fromString(req.getParameter(R_CTX)).resolve(enrollDao, groupDao, courseDao));

		return model;
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

		model.put("nonce", Long.toString(System.currentTimeMillis(), 36));

		return new ModelAndView("a/login", model);
	}

	@RequestMapping(value = "loginPost", method = RequestMethod.POST)
	public ModelAndView do_loginPost(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final String nonce = req.getParameter("nonce");
		final String hash = req.getParameter("hash");

		final HttpSession session = req.getSession(true);
		if (
				nonce == null || hash == null ||
						nonce.trim().length() == 0 || hash.trim().length() == 0
				) {
			Message.addWarn(req, "nonce/hash parameters NOT set");
		} else {
			long nonceStamp = Long.valueOf(nonce, 36);
			if (System.currentTimeMillis() - nonceStamp < NONCE_TIMEOUT_MILLIS) {
				final String hashExpected = Ctx.digest(nonce + "-->" + PASSWORD);

				if (hashExpected.equalsIgnoreCase(hash)) {
					session.setAttribute(S_ADMIN, Boolean.TRUE);
					session.setMaxInactiveInterval(300);
					Message.addInfo(req, "Admin area : logged on");
					final Object loginToAttr = session.getAttribute("loginTo");
					if (loginToAttr instanceof String) {
						session.removeAttribute("loginTo");
						resp.sendRedirect((String) loginToAttr);
					} else {
						resp.sendRedirect("index");
					}
					return null;
				} else {
					Message.addWarn(req, "something went terribly wrong with your password, please retry authentication");
				}
			} else {
				Message.addWarn(req, "nonce expired, please retry authentication");
			}
		}

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

		return new ModelAndView("a/index", model);
	}

	@RequestMapping(value = "courses", method = RequestMethod.GET)
	public ModelAndView do_courses(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		model.put("courses", courseDao.findAllCourses());

		return new ModelAndView("a/courses", model);
	}

	@RequestMapping(value = "course", method = RequestMethod.GET)
	public ModelAndView do_course(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_C)) {
			Message.addWarn(req, "Course not found");
			resp.sendRedirect("courses");
			return null;
		}

		if ("true".equalsIgnoreCase(req.getParameter("test"))) {
			final HashMap<String, Result> testResults = new HashMap<String, Result>();
			final MipsValidator validator = new MipsValidator();
			for (AssignmentType assType : ctx.getCourse().getAssTypes()) {
				for (Assignment ass : assType.getAssignments()) {
					for (Version ver : ass.getVersions()) {
						final Result[] resRef = {new Result("unknown", false)};
/*
						validator.batch(resRef, ver, ver.getSolution(), null);
*/
						testResults.put(
								ctx.extendTAV(assType, ass, ver).toString(),
								resRef[0]
						);
					}
				}
			}

			model.put("testResults", testResults);
		}

		return new ModelAndView("a/course", model);
	}

	@RequestMapping(value = "launch", method = RequestMethod.GET)
	public ModelAndView do_launch(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_CIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final StringWriter verSw = new StringWriter();
		mapper.writeValue(verSw, ctx.getVer());

		//	LATER use HTTP instead of applet parameter for passing the problem/code to applet
		model.put("verJson", verSw.toString().replaceAll("&", "&amp;").replaceAll("\"", "&quot;"));
		model.put("upHeader", "JSESSIONID=" + req.getSession(true).getId());
		model.put("cacheBustingToken", cacheBustingToken);

		return new ModelAndView("a/launch", model);
	}

	@RequestMapping(value = "enrolls", method = RequestMethod.GET)
	public ModelAndView do_enrolls(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Enrollment[] enrolls = enrollDao.findAllEnrollments();
		final Map<String, Group> groups = new HashMap<String, Group>();
		final Map<String, Course> courses = new HashMap<String, Course>();
		for (Enrollment enr : enrolls) {
			groups.put(enr.getGroupId(), groupDao.findGroup(enr.getGroupId()));
			courses.put(enr.getCourseId(), courseDao.findCourse(enr.getCourseId()));
		}
		model.put("enrolls", enrolls);
		model.put("groups", groups);
		model.put("courses", courses);

		return new ModelAndView("a/enrolls", model);
	}

	@RequestMapping(value = "enroll", method = RequestMethod.GET)
	public ModelAndView do_enroll(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
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
		W.filterDefault(model, "f_mode", "a");

		final TreeMap<String, Map<String, List<Entry<FileMeta>>>> fileMetas = new TreeMap<String, Map<String, List<Entry<FileMeta>>>>();
		final HashMap<String, Integer> grossScores = new HashMap<String, Integer>();

		final int[] grossScore = new int[1];
		for (Student stud : ctx.getGroup().getStudents()) {
			if (W.excluded(model.get("f_studId"), stud.getId())) {
				continue;
			}
			final Ctx studCtx = ctx.extendStudent(stud);

			StudentController.storeMetas(
					studCtx, (String) model.get("f_slotId"),
					fileDao,
					fileMetas, grossScore
			);

			grossScores.put(studCtx.toString(), grossScore[0]);
		}

		model.put("fileMetas", fileMetas);
		model.put("grossScores", grossScores);

		return new ModelAndView("a/enroll", model);
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

		return new ModelAndView("a/log", model);
	}

	protected List<Object[]> prepareLogEntries(
			Ctx ctx, Format format, 
			boolean noDues, boolean latest, String slotId, String studId
	) {
		final List<Object[]> logData = new ArrayList<Object[]>();

		for (Student stud : ctx.getGroup().getStudents()) {
			if (W.excluded(studId, stud.getId())) {
				continue;
			}

			final Ctx ctxStud = ctx.extendStudent(stud);
			for (int index = 0; index < ctx.getEnr().getIndex().size(); index++) {
				final Ctx ctxVer = ctxStud.extendIndex(index);
				final AssignmentType aType = ctxVer.getAssType();
				final FileSlot[] slots = aType.getFileSlots();

				for (FileSlot slot : slots) {
					final Map<String, Integer> dueMap = ctxVer.getIndexEntry().getClassDue();
					if (!noDues && (dueMap == null || dueMap.get(slot.getId()) == null)) {
						continue;
					}
					if (W.excluded(slotId, aType.getId(), slot.getId())) {
						continue;
					}

					final Entry<FileMeta>[] uploads = fileDao.findFilesFor(FileDao.SCOPE_STUD, ctxVer, slot.getId());
					if (uploads != null) {
						for (int i = 0, uploadsLength = uploads.length; i < uploadsLength; i++) {
							final boolean last = i + 1 == uploadsLength;
							if (latest && !last) {
								continue;
							}

							logData.add(createRowLog(ctx, format, logData, index, ctxVer, slot, uploads[i]));
						}
					}
				}
			}
		}

		return logData;
	}

	protected Object[] createRowLog(
			Ctx ctx, Format format, List<Object[]> data,
			int index, Ctx ctxVer, FileSlot slot, Entry<FileMeta> e
	) {
		final long time = e.getMeta().getCreateStamp().getTime();

		final IndexEntry iEntry = ctxVer.getIndexEntry();
		final String nameNorm = iEntry.normName(
				ctxVer.getEnr(), ctxVer.getStudent(), ctxVer.getAss(),
				ctxVer.getVer(), slot, e.getMeta(), format
		);
		final String q = "?elw_ctx=" + ctxVer.toString() + "&s=s&sId=" + slot.getId() + "&fId=" + e.getMeta().getId();
		final Object[] dataRow = {
				/* 0 index */ data.size(),
				/* 1 upload millis */ time,
				/* 2 nice date - full */ format.format(time, "EEE d HH:mm"),
				/* 3 nice date - nice */ format.format(time),
				/* 4 author.id */ ctxVer.getStudent().getId(),
				/* 5 author.name */ ctxVer.getStudent().getName(),
				/* 6 class.index */ index,
				/* 7 class.name */ ctxVer.getAss().getName(),
				/* 8 slot.id */ slot.getId(),
				/* 9 slot.name */ slot.getName(),
				/* 10 comment */ e.getMeta().getComment(),
				/* 11 status text*/ iEntry.getStatusHtml(format, ctx.getEnr(), slot, e.getMeta()),
				/* 12 status classes */ iEntry.getStatusClasses(ctx.getEnr(), slot, e.getMeta()),
				/* 13 approve */ "<a href=\"approve" + q + "\">A</a>",
				/* 14 download */ "<a href=\"../s/dl/" + nameNorm + q + "\">D</a>"
		};
		return dataRow;
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

		final boolean noDues = "true".equals(req.getParameter("f_nodues"));
		final boolean latest = "true".equals(req.getParameter("f_latest"));
		final String slotId = req.getParameter("f_slotId");
		final String studId = req.getParameter("f_studId");

		final List<Object[]> logData = prepareLogEntries(
				ctx, format,
				noDues, latest, slotId, studId
		);

		return new ModelAndView(ViewJackson.success(logData));
	}

	@RequestMapping(value = "rest/index", method = RequestMethod.GET)
	public ModelAndView do_restIndex(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, null);
		if (model == null) {
			return null;
		}

		final Enrollment[] enrolls = enrollDao.findAllEnrollments();

		final List<Object[]> indexData = new ArrayList<Object[]>();
		for (Enrollment enr : enrolls) {
			indexData.add(createRowIndex(indexData, enr));
		}

		return new ModelAndView(ViewJackson.success(indexData));
	}

	protected Object[] createRowIndex(List<Object[]> indexData, Enrollment enr) {
		final Group group = groupDao.findGroup(enr.getGroupId());
		final Course course = courseDao.findCourse(enr.getCourseId());

		final Object[] arr = {
				/* 0 index - */ indexData.size(),
				/* 1 enr.id - */ enr.getId(),
				/* 2 group.id - */ group.getId(),
				/* 3 group.name 0*/ group.getName(),
				/* 4 course.id - */ course.getId(),
				/* 5 course.name 1 */ course.getName(),
				/* 6 summary ref 2 */ "enroll?elw_ctx=e--"+enr.getId(),
				/* 7 assignments ref 3 */ "#",
				/* 8 uploads ref 4 */ "log?elw_ctx=e--"+enr.getId(),
				/* 9 classes ref 5 */ "#",
				/* 10 students ref 6 */ "#"
		};
		return arr;
	}

	@RequestMapping(value = "approve", method = RequestMethod.GET)
	public ModelAndView do_approve(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
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

		final Entry<FileMeta> file = fileDao.findFileFor(FileDao.SCOPE_STUD, ctx, slotId, fileId);
		if (file == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "file for id " + fileId + " not found");
			return null;
		}

		final SortedMap<Stamp, Entry<Score>> allScores = scoreDao.findAllScores(ctx, slotId, fileId);

		final Stamp stamp = req.getParameter("stamp") != null ? Stamp.fromString(req.getParameter("stamp")) : null;
		final Entry<Score> lastScoreEntry = allScores.isEmpty() ? null : allScores.get(allScores.lastKey());
		final Entry<Score> scoreEntry = stamp == null ? lastScoreEntry : scoreDao.findScoreByStamp(ctx, stamp, slotId, fileId);
		final Score score = W.updateAutos(ctx, slotId, file, scoreEntry == null ? null : scoreEntry.getMeta());

		model.put("scores", allScores);
		model.put("score", score);
		model.put("slot", slot);

		return new ModelAndView("a/approve", model);
	}

	@RequestMapping(value = "approve", method = RequestMethod.POST)
	public ModelAndView do_approvePost(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
			return null;
		}

		final String codeStampStr = req.getParameter("codeStamp");
		final Stamp codeStamp = Stamp.parse(codeStampStr, null);

		final String reportStampStr = req.getParameter("stamp");
		final Stamp reportStamp = Stamp.parse(reportStampStr, null);

		final String refreshUri = "approve?elw_ctx=" + ctx + "&stamp=" + reportStampStr + "&codeStamp=" + codeStampStr;
		if (codeStamp == null || reportStamp == null) {
			Message.addWarn(req, "Missing either report or code stamp, approval NOT performed");
			resp.sendRedirect(refreshUri);
			return null;
		}

/*
		final ReportMeta reportMeta = reportDao.findByStamp(ctx, reportStamp).getMeta();
		final CodeMeta codeMeta = codeDao.findMetaByStamp(ctx, codeStamp).getMeta();

		if (reportMeta == null || codeMeta == null) {
			Message.addWarn(req, "Missing either report or code metas, approval NOT performed");
			resp.sendRedirect(refreshUri);
			return null;
		}

		if ("Decline".equals(req.getParameter("action"))) {
			reportMeta.setScoreStamp(null);	//	FIXME decline stamp?!!
			reportDao.updateMeta(ctx, reportMeta);
			Message.addWarn(req, "Report declined");
		} else {
			final Score codeScore = computeCodeAuto(codeMeta, ctx);
			final Score score = computeReportAuto(ctx, reportMeta, codeScore);
			computeReportDefault(ctx, score);

			score.setCodeStamp(codeStamp);
			score.setReportStamp(reportStamp);

			final Criteria[] cris = ctx.getAssType().getScoring().getCriterias();
			for (Criteria cri : cris) {
				final int powDef = G4Parse.parse(cri.getPowDef(), 0);
				final String powReq = req.getParameter("cri--" + cri.getId());
				final int pow = G4Parse.parse(powReq, powDef);

				score.getPows().put(cri.getId(), pow);
			}

			final Stamp scoreStamp = scoreDao.createScore(ctx, "FIXME:slotId", "FIXME:fileId", score);
			reportMeta.setScoreStamp(scoreStamp);
			reportDao.updateMeta(ctx, reportMeta);
			Message.addInfo(req, "Report approved");
		}
*/

//		final List<LogEntry> entries = prepareLogEntries(ctx, true, latest, null, studId);
		Object[] nextEntry = null;
/*
		for (LogEntry entry : entries) {
			if (entry.getMeta().getScore() == null) {
				nextEntry = entry;
				break;
			}
		}
*/
		if (nextEntry != null) {
			final String nextUri = "approve?elw_ctx=" /*+ nextEntry.getCtx() + "&stamp=" + nextEntry.getMeta().getCreateStamp()*/;
			resp.sendRedirect(nextUri);
		} else {
			resp.sendRedirect(refreshUri);
		}
		return null;
	}

	protected static Score computeCodeAuto(CodeMeta meta, Ctx ctx) {
		final Class classCodeDue = ctx.getEnr().getClasses().get(ctx.getIndexEntry().getClassDue().get("code"));
		final Criteria[] autos = ctx.getAssType().findSlotById("code").getCriterias();

		if (meta.getValidatorStamp() <= 0) {
			return null;
		}

		final int overdue = classCodeDue.computeDaysOverdue(meta.getCreateStamp());
		final Map<String, Double> vars = new TreeMap<String, Double>();
		vars.put("$passratio", meta.getPassRatio());
		vars.put("$overdue", (double) overdue);

		final Score score = new Score();
		for (Criteria c : autos) {
			score.getPows().put(c.getId(), c.resolvePowDef(vars));
			score.getRatios().put(c.getId(), c.resolveRatio(vars));
		}

		return score;
	}

	@RequestMapping(value = "groups", method = RequestMethod.GET)
	public ModelAndView do_groups(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Group[] groups = groupDao.findAllGroups();

		model.put("groups", groups);

		return new ModelAndView("a/groups", model);
	}

	@RequestMapping(value = "ul", method = RequestMethod.POST)
	public ModelAndView do_ul(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		final String refreshUri = "course?" + WebSymbols.R_CTX + "=" + Ctx.forCourse(ctx.getCourse()).toString();

		return processUpload(req, resp, null, model, refreshUri, "akraievoy");
	}

	public ModelAndView processUpload(
			HttpServletRequest req, HttpServletResponse resp, String scopeForced,
			HashMap<String, Object> model, String refreshUri, String authorName
	) throws IOException, FileUploadException {
		final String scope = scopeForced == null ? req.getParameter("s") : scopeForced;
		if (scope == null) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "scope not set");
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (
				FileDao.SCOPE_ASS_TYPE.equals(scope) && !ctx.resolved(Ctx.STATE_CT) ||
				FileDao.SCOPE_ASS.equals(scope) && !ctx.resolved(Ctx.STATE_CTA) ||
				FileDao.SCOPE_VER.equals(scope) && !ctx.resolved(Ctx.STATE_CTAV) ||
				FileDao.SCOPE_STUD.equals(scope) && !ctx.resolved(Ctx.STATE_EGSCIV)
		) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final String slotId = req.getParameter("sId");
		if ((slotId == null || slotId.trim().length() == 0)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
			return null;
		}

		final FileSlot slot = ctx.getAssType().findSlotById(slotId);
		if (slot == null) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "slot '" + slotId + "' not found");
			return null;
		}

		final boolean put = "PUT".equalsIgnoreCase(req.getMethod());
		if (req.getContentLength() == -1) {
			if (!put) {
				Message.addWarn(req, "upload size not reported");
				resp.sendRedirect(refreshUri);
			} else {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "upload size not reported");
			}
			return null;
		}

		if (req.getContentLength() > slot.getLengthLimit()) {
			final String message = "upload exceeds " + G4mat.formatMem(slot.getLengthLimit());
			if (!put) {
				Message.addWarn(req, message);
				resp.sendRedirect(refreshUri);
			} else {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, message);
			}
			return null;
		}

		final boolean binary = slot.isBinary() ;
		if (put) {
			//	LATER add some sensible defaults to the FileSlot entity
			final String name = "upload" + (binary ? ".bin" : ".txt");
			final FileMeta fileMeta = new FileMeta(
					name,
					name,
					binary ? "application/octet-stream" : "text/plain",
					authorName,
					W.resolveRemoteAddress(req)
			);

			fileDao.createFileFor(
					scope,
					ctx,
					slot.getId(),
					fileMeta,
					binary ? new BufferedInputStream(req.getInputStream()) : null,
					binary ? null : new BufferedReader(new InputStreamReader(req.getInputStream(), "UTF-8"))
			);

			return null;
		}

		final ServletFileUpload sfu = new ServletFileUpload(fileItemFactory);
		final FileItemIterator fii = sfu.getItemIterator(req);
		int fileCount = 0;
		String comment = null;
		while (fii.hasNext()) {
			final FileItemStream item = fii.next();
			if (item.isFormField()) {
				if ("comment".equals(item.getFieldName())) {
					comment = G4Io.dumpToString(item.openStream());
				}
				if ("expandTriggers".equals(item.getFieldName())) {	//	FIXME review
					req.getSession().setAttribute("course.expandTriggers", G4Io.dumpToString(item.openStream()));
				}
				continue;
			}

			if (fileCount > 0) {
				Message.addWarn(req, "one file per upload, please");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String fName = FileMeta.extractNameFromPath(item.getName());
			if (!Pattern.compile(slot.getNameRegex()).matcher(fName.toLowerCase()).matches()) {
				Message.addWarn(req, "check the file name: regex check failed");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String contentType = item.getContentType();
			if (G4Str.indexOf(contentType, slot.getContentTypes()) < 0) {
				Message.addWarn(req, "contentType '" + contentType + "': not listed in the slot");
			}

			final FileMeta fileMeta = new FileMeta(
					fName,
					fName,
					contentType,
					authorName,
					W.resolveRemoteAddress(req)
			);
			if (comment != null) {
				fileMeta.setComment(comment);
			}

			fileDao.createFileFor(
					scope,
					ctx,
					slot.getId(),
					fileMeta,
					binary ? new BufferedInputStream(item.openStream()) : null,
					binary ? null : new BufferedReader(new InputStreamReader(item.openStream(), "UTF-8"))
			);
			fileCount++;
		}

		if (fileCount == 1) {
			Message.addInfo(req, "Your upload has succeeded");
			resp.sendRedirect(refreshUri);
		} else {
			Message.addWarn(req, "Multiple uploads not allowed");
			resp.sendRedirect(refreshUri);
		}

		return null;
	}

	static class W {
		protected static void storeFilter(HttpServletRequest req, HashMap<String, Object> model) {
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

		protected static String resolveRemoteAddress(HttpServletRequest req) {
			final String remoteAddr = req.getRemoteAddr();

			final String xff = req.getHeader("X-Forwarded-For");
			if (xff != null && xff.trim().length() > 0) {
				return xff.replaceAll("\\s+", "").split(",")[0];
			}

			return remoteAddr;
		}

		protected static boolean excluded(Object filterValue, String actualValue) {
			if (!(filterValue instanceof String)) {
				return false;
			}
			return ((String) filterValue).trim().length() > 0 && !filterValue.equals(actualValue);
		}

		protected static void filterDefault(HashMap<String, Object> model, String fKey, String fDefault) {
			if (model.get(fKey) == null || model.get(fKey).toString().trim().length() == 0) {
				model.put(fKey, fDefault);
			}
		}

		public static boolean excluded(String aTypeSlotFilter, String typeId, String slotId) {
			return excluded(aTypeSlotFilter, typeId + "--" + slotId);
		}

		protected static Score updateAutos(Ctx ctx, final String slotId, Entry<FileMeta> file, final Score score) {
			final Map<String, Integer> dueMap = ctx.getIndexEntry().getClassDue();
			final Class classDue;
			if (dueMap == null) {
				classDue = ctx.getEnr().getClasses().get(ctx.getEnr().getClasses().size() - 1);
			} else {
				classDue = ctx.getEnr().getClasses().get(dueMap.get(slotId));
			}
			final FileSlot slot = ctx.getAssType().findSlotById(slotId);

			final Map<String, Double> vars = new TreeMap<String, Double>();
			vars.put("$overdue", (double) classDue.computeDaysOverdue(file.getMeta().getCreateStamp()));
			if (slot.getValidator() != null && file.getMeta().isValidated()) {
				vars.put("$passratio", (double) classDue.computeDaysOverdue(file.getMeta().getCreateStamp()));
			}

			final Score res = score == null ? new Score() : score.copy();
			for (Criteria c : slot.getCriterias()) {
				if (!c.isAuto()) {
					continue;
				}

				final Double ratio = c.resolveRatio(vars);
				final Integer powDef = c.resolvePowDef(vars);
				if (ratio != null && powDef != null) {
					final String id = res.idFor(slot, c);
					res.getPows().put(id, powDef);
					res.getRatios().put(id, ratio);
				}
			}

			return res;
		}
	}
}

