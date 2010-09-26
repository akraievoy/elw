package elw.web;

import elw.dao.*;
import elw.miniweb.Message;
import elw.vo.*;
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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.*;
import java.util.*;

@Controller
@RequestMapping("/s/**/*")
public class StudentController extends MultiActionController implements WebSymbols {
	private static final Logger log = LoggerFactory.getLogger(StudentController.class);

	protected final CourseDao courseDao;
	protected final GroupDao groupDao;
	protected final EnrollDao enrollDao;
	protected final CodeDao codeDao;
	protected final ReportDao reportDao;
	protected final ScoreDao scoreDao;

	protected final ObjectMapper mapper = new ObjectMapper();
	protected final long cacheBustingToken = System.currentTimeMillis();

	private static final int UPLOAD_LIMIT = 2 * 1024 * 1024;

	public StudentController(CourseDao courseDao, GroupDao groupDao, EnrollDao enrollDao, final CodeDao codeDao, ReportDao reportDao, ScoreDao scoreDao) {
		this.courseDao = courseDao;
		this.groupDao = groupDao;
		this.enrollDao = enrollDao;
		this.codeDao = codeDao;
		this.reportDao = reportDao;
		this.scoreDao = scoreDao;
	}

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final String pathToRoot) throws IOException {
		final HttpSession session = req.getSession(true);
		Ctx ctx = Ctx.fromString(req.getParameter(	R_CTX));

		//	admin impersonation
		final Boolean admin = (Boolean) session.getAttribute(S_ADMIN);
		if (Boolean.TRUE.equals(admin)) {
			if (!ctx.resolve(enrollDao, groupDao, courseDao).resolved(Ctx.STATE_GS)) {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
				return null;
			}

			final HashMap<String, Object> model = new HashMap<String, Object>();

			model.put(R_CTX, ctx);
			model.put(S_MESSAGES, Message.drainMessages(req));

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

			final HashMap<String, Object> model = new HashMap<String, Object>();

			model.put(R_CTX, ctx);
			model.put(S_MESSAGES, Message.drainMessages(req));

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

	@RequestMapping(value = "login", method = RequestMethod.GET)
	public ModelAndView do_login(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put("groupName", req.getSession(true).getAttribute("groupName"));
		model.put("studentName", req.getSession(true).getAttribute("studentName"));
		model.put(S_MESSAGES, Message.drainMessages(req));

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
			Message.addWarn(req, "fields NOT set");
		} else {
			final Group[] groups = groupDao.findAllGroups();
			final Group group = IdName.findByName(groups, groupName, true);

			if (group != null) {
				final Student[] students = group.getStudents();
				final Student student = IdName.findByName(students, studentName, true);

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
					Message.addWarn(req, "no such student");
				}
			} else {
				Message.addWarn(req, "no such group");
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
		final int[] grossScore = new int[1];
		storeMetas(ctx, codeDao, reportDao, scoreDao, codeMetas, reportMetas, scores, grossScore);
		model.put("codeMetas", codeMetas);
		model.put("reportMetas", reportMetas);
		model.put("scores", scores);
		model.put("grossScore", grossScore[0]);

		return new ModelAndView("s/course", model);
	}

	public static void storeMetas(
			Ctx ctx,
			CodeDao codeDao, ReportDao reportDao, ScoreDao scoreDao,
			Map<String, CodeMeta> codeMetas, Map<String, ReportMeta> reportMetas, Map<String, Score> scores, int[] grossScore) {
		double grossScoreFuzzy = 0;

		for (int index = 0; index < ctx.getEnr().getIndex().size(); index++) {
			final Ctx ctxIdx = ctx.extendIndex(index);
			final AssignmentType assType = ctxIdx.getAssType();
			final Assignment ass = ctxIdx.getAss();
			for (Version ver : ass.getVersions()) {
				Student student = ctxIdx.getStudent();
				if (Ctx.isVersionIncorrect(student, ass, ver)) {
					continue;
				}

				final Ctx ctxAss = ctxIdx.extendTAV(assType, ass, ver);
				final String assPath = ctxAss.toString();

				if (codeMetas != null) {
					final Entry<CodeMeta> last = codeDao.findLast(ctxAss);
					if (last != null) {
						codeMetas.put(assPath, last.getMeta());
					}
				}
				if (reportMetas != null) {
					final Entry<ReportMeta> lastReport = reportDao.findLast(ctxAss);
					if (lastReport != null) {
						reportMetas.put(assPath, lastReport.getMeta());
					}
				}
				if (scores != null) {
					final Entry<Score> lastScore = scoreDao.findLastScore(ctxAss);
					Score effectiveScore = null;
					if (lastScore == null) {
						if (codeDao != null) {
							final Entry<ReportMeta> lastReport = reportDao != null ? reportDao.findLast(ctxAss) : null;
							final HashMap<Stamp, Score> allCodeScores = new HashMap<Stamp, Score>();
							final Stamp bestCodeStamp = AdminController.computeCodeScores(
									ctxAss,
									codeDao.findAllMetas(ctxAss),
									allCodeScores,
									lastReport != null ? lastReport.getMeta().getCreateStamp() : null
							);
							effectiveScore = allCodeScores.get(bestCodeStamp);
						}
					} else {
						effectiveScore = lastScore.getMeta();
					}
					if (effectiveScore != null) {
						scores.put(assPath, effectiveScore);
						grossScoreFuzzy += effectiveScore.getTotal(assType.getScoring(), ctxAss.getIndexEntry());
					}
				}
			}
		}

		grossScore[0] = (int) Math.floor(grossScoreFuzzy + 0.1);
	}

	@RequestMapping(value = "uploadReport", method = RequestMethod.POST)
	public ModelAndView do_uploadReport(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		if (ctx.getAss().isShared()) {
			Message.addWarn(req, "shared assignment do not require any reports");
			resp.sendRedirect("course?" + WebSymbols.R_CTX + "=e--" + ctx.getEnr().getId());
			return null;
		}

		final String refreshUri = "uploadPage?" + WebSymbols.R_CTX + "=" + ctx.toString();
		if (scoreDao.findLastScore(ctx) != null) {
			Message.addWarn(req, "report already approved, upload NOT allowed");
			resp.sendRedirect(refreshUri);
			return null;
		}

		if (req.getContentLength() > UPLOAD_LIMIT) {
			Message.addWarn(req, "apparently you're trying to upload more than " + G4mat.formatMem(UPLOAD_LIMIT));
			resp.sendRedirect(refreshUri);
			return null;
		}

		final DiskFileItemFactory fif = new DiskFileItemFactory();
		fif.setRepository(new java.io.File(System.getProperty("java.io.tmpdir")));
		fif.setSizeThreshold(2 * UPLOAD_LIMIT);

		final ServletFileUpload sfu = new ServletFileUpload(fif);

		final FileItemIterator fii = sfu.getItemIterator(req);
		int fileCount = 0;
		while (fii.hasNext()) {
			final FileItemStream item = fii.next();
			if (item.isFormField()) {
					continue;
			}

			if (fileCount > 0) {
				Message.addWarn(req, "one file per upload, please");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String fileName = item.getName();
			if (!fileName.endsWith(".rtf")) {
				Message.addWarn(req, "is that an .rtf file or what?");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String contentType = item.getContentType();
			if (!"application/rtf".equalsIgnoreCase(contentType) && !"application/msword".equalsIgnoreCase(contentType)) {
				Message.addWarn(req, "contentType is '" + contentType + "', which is not what is expected for an .rtf file");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final InputStream itemIs = item.openStream();
			reportDao.createReport(ctx, new BufferedInputStream(itemIs), resolveRemoteAddress(req));
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

	@RequestMapping(value = "uploadPage", method = RequestMethod.GET)
	public ModelAndView do_uploadPage(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		if (ctx.getAss().isShared()) {
			Message.addWarn(req, "shared assignments do not require any reports");
			resp.sendRedirect("course?" + WebSymbols.R_CTX + "=e--" + ctx.getEnr().getId());
			return null;
		}

		model.put("uploadLimit", G4mat.formatMem(UPLOAD_LIMIT));

		final Map<Stamp, Entry<ReportMeta>> reports = reportDao.findAll(ctx);
		final Map<Stamp, Entry<CodeMeta>> codes = codeDao.findAllMetas(ctx);
		final Map<Stamp, Score> codeScores = new TreeMap<Stamp, Score>();
		AdminController.computeCodeScores(ctx, codes, codeScores, null);

		final Entry<Score> lastScore = scoreDao.findLastScore(ctx);
		if (lastScore != null) {
			model.put("stamp", lastScore.getMeta().getReportStamp());
			model.put("codeStamp", lastScore.getMeta().getCodeStamp());
			codeScores.put(lastScore.getMeta().getCodeStamp(), lastScore.getMeta());
		}

		model.put("reports", reports);
		model.put("codes", codes);
		model.put("codeScores", codeScores);
		model.put("scores", scoreDao.findAllScores(ctx));
		model.put("elw_ctx", ctx);

		return new ModelAndView("s/uploadPage", model);
	}

	@RequestMapping(value = "reportDl/*.*", method = RequestMethod.GET)
	public ModelAndView do_reportDl(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "../");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		if (ctx.getAss().isShared()) {
			Message.addWarn(req, "shared assignments do not require any reports");
			resp.sendRedirect("course?" + WebSymbols.R_CTX + "=e--" + ctx.getEnr().getId());
			return null;
		}

		final String stampStr = req.getParameter("stamp");
		if (stampStr == null) {
			Message.addWarn(req, "no stamp defined");
			resp.sendRedirect("uploadPage?" + WebSymbols.R_CTX + "=" + ctx.toString());
			return null;
		}	

		final Stamp stamp = Stamp.parse(stampStr, null);
		if (stamp == null) {
			Message.addWarn(req, "stamp parse error");
			resp.sendRedirect("uploadPage?" + WebSymbols.R_CTX + "=" + ctx.toString());
			return null;
		}

		Entry<ReportMeta> report = null;
		try {
			report = reportDao.findByStamp(ctx, stamp);
			if (report == null) {
				Message.addWarn(req, "no report for stamp " + stamp);
				resp.sendRedirect("uploadPage?" + WebSymbols.R_CTX + "=" + ctx.toString());
				return null;
			}

			resp.setCharacterEncoding("UTF-8");
			resp.setContentType("application/rtf");
			resp.setHeader("Content-Disposition", "attachment;");

			final BufferedInputStream in = report.openBinaryStream();
			final byte[] buf = new byte[32768];
			int bytesRead;
			while ((bytesRead = in.read(buf)) >= 0) {
				resp.getOutputStream().write(buf, 0, bytesRead);
			}
		} finally {
			report.closeStreams();
		}

		return null;
	}

	@RequestMapping(value = "codeDl/*.*", method = RequestMethod.GET)
	public ModelAndView do_codeDl(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "../");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		if (ctx.getAss().isShared()) {
			Message.addWarn(req, "shared assignments do not require any code");
			resp.sendRedirect("course?" + WebSymbols.R_CTX + "=e--" + ctx.getEnr().getId());
			return null;
		}

		final String stampStr = req.getParameter("stamp");
		if (stampStr == null) {
			Message.addWarn(req, "no stamp defined");
			resp.sendRedirect("uploadPage?" + WebSymbols.R_CTX + "=" + ctx.toString());
			return null;
		}

		final Stamp stamp = Stamp.parse(stampStr, null);
		final Entry<CodeMeta> entry = codeDao.findMetaByStamp(ctx, stamp);
		if (entry == null) {
			Message.addWarn(req, "no code for stamp " + stamp);
			resp.sendRedirect("uploadPage?" + WebSymbols.R_CTX + "=" + ctx.toString());
			return null;
		}

		resp.setCharacterEncoding("UTF-8");
		resp.setContentType("text");
		resp.setHeader("Content-Disposition", "attachment;");

		final String[] code;
		try {
			code = entry.dumpText();
		} finally {
			entry.closeStreams();
		}
		for (String aCode : code) {
			resp.getWriter().println(aCode);
		}

		return null;
	}

	@RequestMapping(value = "launch", method = RequestMethod.GET)
	public ModelAndView do_launch(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final StringWriter verSw = new StringWriter();
		mapper.writeValue(verSw, ctx.getVer());
		final Version verCopy = mapper.readValue(verSw.toString(), Version.class);

		final Entry<CodeMeta> last = codeDao.findLast(ctx);

		if (!ctx.getAss().isShared() || last != null) {
			try {
				verCopy.setSolution(last.dumpText());
			} finally {
				last.closeStreams();
			}
		}

		final StringWriter verCopySol = new StringWriter();
		mapper.writeValue(verCopySol, verCopy);
		final String solutionStr = verCopySol.toString();

		//	LATER use HTTP instead of applet parameter for passing the problem/code to applet
		model.put("verJson", solutionStr.replaceAll("&", "&amp;").replaceAll("\"", "&quot;"));
		model.put("upHeader", "JSESSIONID=" + req.getSession(true).getId());
		model.put("cacheBustingToken", cacheBustingToken);

		return new ModelAndView("s/launch", model);
	}

	@RequestMapping(value = "upload", method = RequestMethod.POST)
	public ModelAndView do_upload(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, null);
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		if (scoreDao.findLastScore(ctx) != null) {
			resp.sendError(HttpServletResponse.SC_CONFLICT, "report for given task already approved");
			return null;
		}

		final BufferedReader codeReader = req.getReader();
		try {
			codeDao.createCode(ctx, codeReader, resolveRemoteAddress(req));
		} catch (IOException e) {
			resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}

		resp.setStatus(HttpServletResponse.SC_OK);
		return null;
	}

	protected static String resolveRemoteAddress(HttpServletRequest req) {
		return req.getRemoteAddr();
	}
}