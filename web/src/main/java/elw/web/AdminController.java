package elw.web;

import base.pattern.Result;
import elw.dao.*;
import elw.dp.mips.MipsValidator;
import elw.miniweb.Message;
import elw.vo.*;
import org.akraievoy.gear.G4Parse;
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
import java.io.IOException;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

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

	protected final ObjectMapper mapper = new ObjectMapper();
	protected final long cacheBustingToken = System.currentTimeMillis();

	public AdminController(CourseDao courseDao, EnrollDao enrollDao, GroupDao groupDao, CodeDao codeDao, ReportDao reportDao) {
		this.courseDao = courseDao;
		this.enrollDao = enrollDao;
		this.groupDao = groupDao;
		this.codeDao = codeDao;
		this.reportDao = reportDao;
	}

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final String pathToRoot) throws IOException {
		final HttpSession session = req.getSession(true);
		final Boolean admin = (Boolean) session.getAttribute(S_ADMIN);

		if (!Boolean.TRUE.equals(admin)) {
			if (pathToRoot != null) {
				Message.addWarn(req, "Admin authentication required");
				resp.sendRedirect(pathToRoot + "login");
			} else {
				resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Admin authentication required");
			}

			return null;
		}

		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put(S_MESSAGES, Message.drainMessages(req));
		model.put("auth", req.getSession(true).getAttribute(S_ADMIN)); //	LATER move this to auth()

		return model;
	}

	@RequestMapping(value = "login", method = RequestMethod.GET)
	public ModelAndView do_login(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put("nonce", Long.toString(System.currentTimeMillis(), 36));
		model.put(S_MESSAGES, Message.drainMessages(req));

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
				final String hashExpected = digest(nonce + "-->" + PASSWORD);

				if (hashExpected.equalsIgnoreCase(hash)) {
					session.setAttribute(S_ADMIN, Boolean.TRUE);
					session.setMaxInactiveInterval(300);
					Message.addInfo(req, "Admin area : logged on");
					resp.sendRedirect("index");
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

	//	LATER move this to base.G4mat
	public static String renderBytes(byte[] checkSum) {
		final StringBuffer result = new StringBuffer();

		for (byte checkByte : checkSum) {
			result.append(Integer.toString((checkByte & 0xff) + 0x100, 16).substring(1));
		}

		return result.toString();
	}

	public static String digest(String text) throws IOException {
		try {
			final MessageDigest md = MessageDigest.getInstance("SHA-1");
			md.update(text.getBytes("UTF-8"), 0, text.length());
			return renderBytes(md.digest());
		} catch (NoSuchAlgorithmException e) {
			throw new IOException(e);
		} catch (UnsupportedEncodingException e) {
			throw new IOException(e);
		}
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

		final Course course = courseDao.findCourse(req.getParameter("id"));
		if (course == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);	//	LATER review : setStatus -> sendRedirect in most cases
			return null;
		}

		if ("true".equalsIgnoreCase(req.getParameter("test"))) {
			final HashMap<String, Result> testResults = new HashMap<String, Result>();
			final MipsValidator validator = new MipsValidator();
			for (int bunI = 0, assBundlesLength = course.getAssBundles().length; bunI < assBundlesLength; bunI++) {
				AssignmentBundle bundle = course.getAssBundles()[bunI];
				for (Assignment ass : bundle.getAssignments()) {
					for (Version ver : ass.getVersions()) {
						final Result[] resRef = {new Result("unknown", false)};
						validator.batch(resRef, ver, ver.getSolution(), null);
						testResults.put(
								course.getId() + "--" + bunI + "--" + ass.getId() + "--" + ver.getId(),
								resRef[0]
						);
					}
				}
			}
			model.put("testResults", testResults);
		}

		model.put("course", course);

		return new ModelAndView("a/course", model);
	}

	@RequestMapping(value = "launch", method = RequestMethod.GET)
	public ModelAndView do_launch(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final String path = req.getParameter("path");
		final String[] ids = path.split("--");
		if (ids.length != 4) {
			log.warn("malformed path {}", Arrays.toString(ids));

			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		final String courseId = ids[0];
		final Course course = courseDao.findCourse(courseId);
		if (course == null) {
			log.warn("course not found by id {}", courseId);
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		final int assBundleIndex = G4Parse.parse(ids[1], -1);
		if (assBundleIndex < 0 || course.getAssBundles().length <= assBundleIndex) {
			log.warn("bundle not found by index {}", assBundleIndex);
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		final String assId = ids[2];
		final AssignmentBundle bundle = course.getAssBundles()[assBundleIndex];
		final Assignment ass = IdName.findById(bundle.getAssignments(), assId);
		if (ass == null) {
			log.warn("assignment not found by id {}", assId);
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		final String verId = ids[3];
		final Version ver = IdName.findById(ass.getVersions(), verId);
		if (ver == null) {
			log.warn("version not found by id {}", verId);
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		final StringWriter verSw = new StringWriter();
		mapper.writeValue(verSw, ver);

		//	LATER use HTTP instead of applet parameter for passing the problem/code to applet
		model.put("ver", verSw.toString().replaceAll("&", "&amp;").replaceAll("\"", "&quot;"));
		model.put("verBean", ver);
		model.put("cacheBustingToken", cacheBustingToken);
		model.put("course", course);

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

		final Enrollment enr = enrollDao.findEnrollment(req.getParameter("id"));
		if (enr == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}
		final Course course = courseDao.findCourse(enr.getCourseId());
		if (course == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}
		final Group group = groupDao.findGroup(enr.getGroupId());
		if (group == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		model.put("enr", enr);
		model.put("group", group);
		model.put("course", course);

		final HashMap<String, CodeMeta> codeMetas = new HashMap<String, CodeMeta>();
		final HashMap<String, ReportMeta> reportMetas = new HashMap<String, ReportMeta>();
		for (Student stud : group.getStudents()) {
			StudentController.storeMetas(
					codeDao, reportDao,
					course, group, stud,
					codeMetas, reportMetas, stud.getId()
			);
		}
		model.put("codeMetas", codeMetas);
		model.put("reportMetas", reportMetas);

		return new ModelAndView("a/enroll", model);
	}

	@RequestMapping(value = "log", method = RequestMethod.GET)
	public ModelAndView do_log(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Enrollment enr = enrollDao.findEnrollment(req.getParameter("id"));
		if (enr == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}
		final Course course = courseDao.findCourse(enr.getCourseId());
		if (course == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}
		final Group group = groupDao.findGroup(enr.getGroupId());
		if (group == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		model.put("enr", enr);
		model.put("group", group);
		model.put("course", course);

		final HashMap<String, ReportMeta> reportMetas = new HashMap<String, ReportMeta>();
		final List<LogEntry> logEntries = new ArrayList<LogEntry>();
		for (Student stud : group.getStudents()) {
			reportMetas.clear();
			StudentController.storeMetas(codeDao, reportDao, course, group, stud, null, reportMetas, stud.getId());
			for (String key : reportMetas.keySet()) {
				final ReportMeta meta = reportMetas.get(key);
				if (meta.getTotalUploads() == 0) {
					continue;
				}

				final String[] keyAsPath = key.split("--");
				final int assBundleIndex = G4Parse.parse(keyAsPath[2], -1);
				final AssignmentBundle bundle = course.getAssBundles()[assBundleIndex];
				final Assignment ass = IdName.findById(bundle.getAssignments(), keyAsPath[3]);
				logEntries.add(new LogEntry(stud, key, ass, meta));
			}
		}

		Collections.sort(logEntries);
		model.put("logEntries", logEntries);

		return new ModelAndView("a/log", model);
	}

	public static class LogEntry implements Comparable<LogEntry> {
		private final Assignment ass;
		protected final ReportMeta meta;
		protected final Student student;
		protected final String path;

		public LogEntry(Student student, String path, Assignment ass, ReportMeta meta) {
			this.ass = ass;
			this.meta = meta;
			this.student = student;
			this.path = path;
		}

		public int compareTo(LogEntry o) {
			final long thisVal = meta.getUploadStamp();
			final long anotherVal = o.meta.getUploadStamp();
			return (thisVal < anotherVal ? -1 : (thisVal == anotherVal ? 0 : 1));
		}

		public ReportMeta getMeta() {
			return meta;
		}

		public Student getStudent() {
			return student;
		}

		public String getPath() {
			return path;
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

		final String path = req.getParameter("path");
		if (path == null || path.trim().length() == 0) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path not set");
			return null;
		}
		final String[] pathArr = path.split("--");
		if (pathArr.length != 5) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path syntax weirdo");
			return null;
		}

		final String enrId = req.getParameter("id");
		if (enrId == null || enrId.trim().length() == 0) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Enrollment ID not set");
			return null;
		}
		final Enrollment enr = enrollDao.findEnrollment(enrId);
		if (enr == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Enrollment " + enrId + " not found");
			return null;
		}
		final Course course = courseDao.findCourse(enr.getCourseId());
		if (course == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Course " + enr.getCourseId() + " not found");
			return null;
		}
		final Group group = groupDao.findGroup(enr.getGroupId());
		if (group == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Group " + enr.getGroupId() + " not found");
			return null;
		}

		model.put("path", path);
		model.put("id", enrId);
		model.put("enr", enr);
		model.put("group", group);
		model.put("course", course);

		final Student student = IdName.findById(group.getStudents(), pathArr[0]);
		if (student == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Student " + pathArr[0] + " not found");
			return null;
		}

		final int assBundleIndex = G4Parse.parse(pathArr[2], -1);
		if (assBundleIndex < 0 || assBundleIndex >= course.getAssBundles().length) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Bundle " + pathArr[2] + " not found");
			return null;
		}

		final AssignmentBundle bundle = course.getAssBundles()[assBundleIndex];
		final Assignment ass = IdName.findById(bundle.getAssignments(), pathArr[3]);
		if (ass == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Assignment " + pathArr[3] + " not found");
			return null;
		}
		
		Version ver = null;
		for (Version v : ass.getVersions()) {
			if (StudentController.isVersionIncorrect(student, ass, v)) {
				continue;
			}
			ver = v;
		}
		if (ver == null) {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Version not found");
			return null;
		}
		model.put("student", student);
		model.put("ass", ass);
		model.put("ver", ver);

		final AssignmentPath assPath = new AssignmentPath(
				course.getId(), group.getId(), student,
				assBundleIndex, ass.getId(), ver.getId()
		);

		final Map<Long, ReportMeta> reports = reportDao.findAllMetas(assPath);

		final String reportStampStr = req.getParameter("stamp");
		long reportStamp = G4Parse.parse(reportStampStr, -1L);
		if (reportStamp == -1) {
			reportStamp = reports.keySet().iterator().next();
		}

		model.put("stamp", reportStamp);
		model.put("reports", reports);
		model.put("codes", codeDao.findAllMetas(assPath));

		return new ModelAndView("a/approve", model);
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

}
