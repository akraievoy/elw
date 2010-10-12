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
import elw.vo.*;
import elw.vo.Class;
import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Parse;
import org.akraievoy.gear.G4Str;
import org.akraievoy.gear.G4mat;
import org.apache.commons.fileupload.FileItemIterator;
import org.apache.commons.fileupload.FileItemStream;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.codehaus.jackson.map.ObjectMapper;
import org.joda.time.DateTime;
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
	protected final CodeDao codeDao;
	protected final ReportDao reportDao;
	protected final ScoreDao scoreDao;
	protected final FileDao fileDao;

	protected final ObjectMapper mapper = new ObjectMapper();
	protected final long cacheBustingToken = System.currentTimeMillis();

	protected final DiskFileItemFactory fileItemFactory;

	public AdminController(
			CourseDao courseDao, EnrollDao enrollDao, GroupDao groupDao, CodeDao codeDao,
			ReportDao reportDao, ScoreDao scoreDao, FileDao fileDao
	) {
		this.courseDao = courseDao;
		this.enrollDao = enrollDao;
		this.groupDao = groupDao;
		this.codeDao = codeDao;
		this.reportDao = reportDao;
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
		model.put("format", FormatTool.forLocale(RequestContextUtils.getLocale(req)));

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

		//	TODO: check that studentId is not deleted from the context as redundant element
		final TreeMap<String, Map<String, List<Entry<FileMeta>>>> fileMetas =
				new TreeMap<String, Map<String, List<Entry<FileMeta>>>>();
		final HashMap<String, CodeMeta> codeMetas = new HashMap<String, CodeMeta>();
		final HashMap<String, ReportMeta> reportMetas = new HashMap<String, ReportMeta>();
		final HashMap<String, Score> scores = new HashMap<String, Score>();
		final HashMap<String, Integer> grossScores = new HashMap<String, Integer>();
		final int[] grossScore = new int[1];
		for (Student stud : ctx.getGroup().getStudents()) {
			final Ctx studCtx = ctx.extendStudent(stud);

			StudentController.storeMetas(
					studCtx,
					codeDao, reportDao, scoreDao, fileDao,
					fileMetas, codeMetas, reportMetas, scores,
					grossScore);

			grossScores.put(studCtx.toString(), grossScore[0]);
		}

		model.put("fileMetas", fileMetas);
		model.put("codeMetas", codeMetas);
		model.put("reportMetas", reportMetas);
		model.put("scores", scores);
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

		final List<LogEntry> logEntries = prepareLogEntries(ctx);

		model.put("logEntries", logEntries);

		return new ModelAndView("a/log", model);
	}

	protected List<LogEntry> prepareLogEntries(Ctx ctx) {
		final List<LogEntry> logEntries = new ArrayList<LogEntry>();

		for (Student stud : ctx.getGroup().getStudents()) {
			final Ctx ctxStud = ctx.extendStudent(stud);
			for (int index = 0; index < ctx.getEnr().getIndex().size(); index++) {
				final Ctx ctxVer = ctxStud.extendIndex(index);
				final AssignmentType assType = ctxVer.getAssType();
				final Assignment ass = ctxVer.getAss();
				final Version ver = ctxVer.getVer();

				final Entry<ReportMeta> lastReport = reportDao.findLast(ctxVer);
				if (lastReport != null) {
					logEntries.add(new LogEntry(stud, ctxVer, ass, lastReport.getMeta()));
				}
			}
		}

		Collections.sort(logEntries);

		return logEntries;
	}

	protected static String resolveRemoteAddress(HttpServletRequest req) {
		final String remoteAddr = req.getRemoteAddr();

		final String xff = req.getHeader("X-Forwarded-For");
		if (xff != null && xff.trim().length() > 0) {
			return xff.replaceAll("\\s+", "").split(",")[0];
		}

		return remoteAddr;
	}

	public static class LogEntry implements Comparable<LogEntry> {
		protected final Assignment ass;
		protected final ReportMeta meta;
		protected final Student student;
		protected final Ctx ctx;

		public LogEntry(Student student, Ctx ctx, Assignment ass, ReportMeta meta) {
			this.ass = ass;
			this.meta = meta;
			this.student = student;
			this.ctx = ctx;
		}

		public int compareTo(LogEntry o) {
			final long thisVal = meta.getCreateStamp().getTime();
			final long anotherVal = o.meta.getCreateStamp().getTime();
			return (thisVal < anotherVal ? -1 : (thisVal == anotherVal ? 0 : 1));
		}

		public ReportMeta getMeta() {
			return meta;
		}

		public Student getStudent() {
			return student;
		}

		public Ctx getCtx() {
			return ctx;
		}

		public Assignment getAss() {
			return ass;
		}
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

		final Map<Stamp, Entry<ReportMeta>> reports = reportDao.findAll(ctx);

		final String reportStampStr = req.getParameter("stamp");
		Stamp reportStamp = Stamp.parse(reportStampStr, null);
		if (reportStamp == null) {
			reportStamp = reports.keySet().iterator().next();
		}

		model.put("stamp", reportStamp);
		model.put("reports", reports);

		final Map<Stamp, Entry<CodeMeta>> codes = codeDao.findAllMetas(ctx);
		model.put("codes", codes);

		final Map<Stamp, Score> codeScores = new TreeMap<Stamp, Score>();

		Stamp codeStamp = computeCodeScores(ctx, codes, codeScores, reportStamp);

		model.put("codeStamp", Stamp.parse(req.getParameter("codeStamp"), codeStamp));

		model.put("codeScores", codeScores);

		final Score reportScore;
		final ReportMeta reportMeta = reports.get(reportStamp).getMeta();
		final Stamp scoreStamp = reportMeta.getScoreStamp();
		if (scoreStamp == null) {
			final Score reportBaseScore;
			final Score codeScore = codeScores.get(codeStamp);
			if (codeStamp == null || codeScore == null){
				Message.addWarn(req, "Unable to find any codes preceding selected report");
				reportBaseScore = new Score();
			} else {
				reportBaseScore = codeScore.copy();
			}
			reportScore = computeReportAuto(ctx, reportMeta, reportBaseScore);
			computeReportDefault(ctx, reportScore);
		} else {
			final Entry<Score> score = scoreDao.findScoreByStamp(ctx, scoreStamp, "FIXME:slotId", "FIXME:fileId");
			reportScore = score.getMeta();
			codeScores.put(score.getMeta().getCodeStamp(), score.getMeta());
		}

		model.put("scores", scoreDao.findAllScores(ctx, "FIXME:slotId", "FIXME:fileId"));
		model.put("score", reportScore);

		return new ModelAndView("a/approve", model);
	}

	public static Stamp computeCodeScores(Ctx ctx, Map<Stamp, Entry<CodeMeta>> codes, Map<Stamp, Score> codeScores, Stamp reportStamp) {
		double bestRatio = 0;
		Stamp codeStamp = null;

		for (Stamp cStamp : codes.keySet()) {
			final Entry<CodeMeta> entry = codes.get(cStamp);
			final CodeMeta meta = entry.getMeta();
			final Score score = computeCodeAuto(meta, ctx);

			codeScores.put(cStamp, score);

			//	here we select the best code scoring to base our report rating on
			final double ratio = score.getRatio(ctx.getAssType().getScoring().getBreakdown().get("code").getAuto());
			if ((reportStamp == null || meta.getCreateStamp().getTime() <= reportStamp.getTime()) && ratio > bestRatio) {
				codeStamp = cStamp;
				bestRatio = ratio;
			}
		}

		return codeStamp;
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

		final List<LogEntry> entries = prepareLogEntries(ctx);
		LogEntry nextEntry = null;
		for (LogEntry entry : entries) {
			if (entry.getMeta().getScoreStamp() == null && entry.getMeta().getTotalUploads() > 0) {
				nextEntry = entry;
				break;
			}
		}
		if (nextEntry != null) {
			final String nextUri = "approve?elw_ctx=" + nextEntry.getCtx() + "&stamp=" + nextEntry.getMeta().getCreateStamp();
			resp.sendRedirect(nextUri);
		} else {
			resp.sendRedirect(refreshUri);
		}
		return null;
	}

	protected static void computeReportDefault(Ctx ctx, Score reportScore) {
		final Criteria[] criterias = ctx.getAssType().getScoring().getCriterias();
		for (Criteria c : criterias) {
			if (!reportScore.contains(c.getId())) {
				reportScore.getPows().put(c.getId(), c.resolvePowDef(null));
				reportScore.getRatios().put(c.getId(), c.resolveRatio(null));
			}
		}
	}

	protected static Score computeReportAuto(Ctx ctx, ReportMeta meta, final Score baseScore) {
		final Class classDue = ctx.getEnr().getClasses().get(ctx.getIndexEntry().getClassDue().get("report"));
		final TypeScoring reportScoring = ctx.getAssType().getScoring().getBreakdown().get("report");
		final Criteria[] autos = reportScoring.resolveAuto();

		final DateTime uploadStamp = new DateTime(meta.getCreateStamp());
		final double overdue;
		if (classDue.isPassed(uploadStamp)) {
			overdue = classDue.getDayDiff(uploadStamp);
		} else {
			overdue = 0.0;
		}
		final Map<String, Double> vars = new TreeMap<String, Double>();
		vars.put("$overdue", overdue);

		final Score score = baseScore == null ? new Score() : baseScore;
		for (Criteria c : autos) {
			score.getPows().put(c.getId(), c.resolvePowDef(vars));
			score.getRatios().put(c.getId(), c.resolveRatio(vars));
		}

		return score;
	}

	protected static Score computeCodeAuto(CodeMeta meta, Ctx ctx) {
		final Class classCodeDue = ctx.getEnr().getClasses().get(ctx.getIndexEntry().getClassDue().get("code"));
		final TypeScoring codeScoring = ctx.getAssType().getScoring().getBreakdown().get("code");
		final Criteria[] autos = codeScoring.resolveAuto();

		if (meta.getValidatorStamp() <= 0) {
			return null;
		}

		final DateTime uploadStamp = new DateTime(meta.getCreateStamp().getTime());
		final double overdue;
		if (classCodeDue.isPassed(uploadStamp)) {
			overdue = classCodeDue.getDayDiff(uploadStamp);
		} else {
			overdue = 0.0;
		}
		final Map<String, Double> vars = new TreeMap<String, Double>();
		vars.put("$passratio", meta.getPassRatio());
		vars.put("$overdue", overdue);

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
				FileDao.SCOPE_COURSE.equals(scope) && !ctx.resolved(Ctx.STATE_C) ||
				FileDao.SCOPE_ASS_TYPE.equals(scope) && !ctx.resolved(Ctx.STATE_CT) ||
				FileDao.SCOPE_ASS.equals(scope) && !ctx.resolved(Ctx.STATE_CTA) ||
				FileDao.SCOPE_VER.equals(scope) && !ctx.resolved(Ctx.STATE_CTAV) ||
				FileDao.SCOPE_STUD.equals(scope) && !ctx.resolved(Ctx.STATE_EGSCIV)
		) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final String slotId = req.getParameter("sId");
		final boolean course = FileDao.SCOPE_COURSE.equals(scope);
		if (!course && (slotId == null || slotId.trim().length() == 0)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
			return null;
		}

		final FileSlot slot;
		if (!course) {
			slot = ctx.getAssType().findSlotById(slotId);
			if (slot == null) {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "slot '" + slotId + "' not found");
				return null;
			}
		} else {
			slot = null;
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

		if (!course) {
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
		}

		final boolean binary = course || slot.isBinary() ;
		if (put) {
			if (course) {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no course-scope PUT uploads, please");
				return null;
			}

			//	LATER add some sensible defaults to the FileSlot entity 
			final String name = "upload" + (binary ? ".bin" : ".txt");
			final FileMeta fileMeta = new FileMeta(
					name,
					name,
					binary ? "application/octet-stream" : "text/plain",
					authorName,
					resolveRemoteAddress(req)
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
				if ("expandTriggers".equals(item.getFieldName())) {
					req.getSession().setAttribute("course.expandTriggers", G4Io.dumpToString(item.openStream()));
				}
				continue;
			}

			if (fileCount > 0) {
				Message.addWarn(req, "one file per upload, please");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String fileName = item.getName();
			if (!course && !Pattern.compile(slot.getNameRegex()).matcher(fileName.toLowerCase()).matches()) {
				Message.addWarn(req, "check the file name: regex check failed");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String contentType = item.getContentType();
			if (!course && G4Str.indexOf(contentType, slot.getContentTypes()) < 0) {
				Message.addWarn(req, "contentType '" + contentType + "': not allowed");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final FileMeta fileMeta = new FileMeta(
					fileName,
					fileName,
					contentType,
					authorName,
					resolveRemoteAddress(req)
			);
			if (comment != null) {
				fileMeta.setComment(comment);
			}

			fileDao.createFileFor(
					scope,
					ctx,
					course ? null : slot.getId(),
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
			Message.addWarn(req, "Something went terribly wrong");
			resp.sendRedirect(refreshUri);
		}

		return null;
	}
}
