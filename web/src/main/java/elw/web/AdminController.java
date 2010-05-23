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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

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

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final boolean redirect) throws IOException {
		final HttpSession session = req.getSession(true);
		final Boolean admin = (Boolean) session.getAttribute(S_ADMIN);

		if (!Boolean.TRUE.equals(admin)) {
			if (redirect) {
				Message.addWarn(req, "Admin authentication required");
				resp.sendRedirect("login");
			} else {
				resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Admin authentication required");
			}

			return null;
		}

		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put(S_MESSAGES, Message.drainMessages(req));

		return model;
	}

	public ModelAndView do_login(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put("nonce", Long.toString(System.currentTimeMillis(), 36));
		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));
		model.put(S_MESSAGES, Message.drainMessages(req));

		return new ModelAndView("a/login", model);
	}

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

	public ModelAndView do_logout(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		req.getSession(true).invalidate();
		resp.sendRedirect("index");
		return null;
	}

	public ModelAndView do_index(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
		if (model == null) {
			return null;
		}

		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));

		return new ModelAndView("a/index", model);
	}

	public ModelAndView do_courses(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
		if (model == null) {
			return null;
		}

		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));
		model.put("courses", courseDao.findAllCourses());

		return new ModelAndView("a/courses", model);
	}

	public ModelAndView do_course(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
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

		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));
		model.put("course", course);

		return new ModelAndView("a/course", model);
	}

	public ModelAndView do_launch(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
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
			log.warn("assignment not found by id {}", verId);
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
		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));

		return new ModelAndView("a/launch", model);
	}

	public ModelAndView do_enrolls(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
		if (model == null) {
			return null;
		}

		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));
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

	public ModelAndView do_enroll(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
		if (model == null) {
			return null;
		}

		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));

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

	public ModelAndView do_groups(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
		if (model == null) {
			return null;
		}

		final Group[] groups = groupDao.findAllGroups();

		model.put("auth", req.getSession(true).getAttribute(S_ADMIN));
		model.put("groups", groups);

		return new ModelAndView("a/groups", model);
	}

}
