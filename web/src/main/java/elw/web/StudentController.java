package elw.web;

import elw.dao.*;
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
import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class StudentController extends MultiActionController {
	private static final Logger log = LoggerFactory.getLogger(StudentController.class);

	public static final String S_GROUP = "elw_group";
	public static final String S_STUD = "elw_student";
	public static final String S_MESSAGES = "elw_messages";

	protected final CourseDao courseDao;
	protected final GroupDao groupDao;
	protected final EnrollDao enrollDao;
	protected final CodeDao codeDao;

	protected final ObjectMapper mapper = new ObjectMapper();

	protected final long cacheBustingToken = System.currentTimeMillis();

	public StudentController(CourseDao courseDao, GroupDao groupDao, EnrollDao enrollDao, final CodeDao codeDao) {
		this.courseDao = courseDao;
		this.groupDao = groupDao;
		this.enrollDao = enrollDao;
		this.codeDao = codeDao;
	}

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final boolean redirect) throws IOException {
		final Group group = (Group) req.getSession(true).getAttribute(S_GROUP);
		final Student student = (Student) req.getSession(true).getAttribute(S_STUD);

		if (group == null || student == null) {
			if (redirect) {
				Message.addWarn(req, "Login required");
				resp.sendRedirect("login");
			} else {
				resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Login required");
			}

			return null;
		}

		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put(S_GROUP, group);
		model.put(S_STUD, student);
		model.put(S_MESSAGES, Message.drainMessages(req));

		return model;
	}

	public ModelAndView do_login(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put("groupName", req.getSession(true).getAttribute("groupName"));
		model.put("studentName", req.getSession(true).getAttribute("studentName"));
		model.put(S_MESSAGES, Message.drainMessages(req));

		return new ModelAndView("s/login", model);
	}

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

					resp.sendRedirect("courses");
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

	public ModelAndView do_courses(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
		if (model == null) {
			return null;
		}

		final Group group = (Group) req.getSession().getAttribute(S_GROUP);

		final String groupId = group.getId();
		List<Course> courses = enrollDao.findCoursesByGroupId(groupId);

		model.put("courses", courses.toArray(new Course[courses.size()]));

		return new ModelAndView("s/courses", model);
	}

	//	LATER check enrollment also
	public ModelAndView do_course(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, true);
		if (model == null) {
			return null;
		}

		final Course course = courseDao.findCourse(req.getParameter("id"));
		if (course == null) {
			Message.addWarn(req, "course not found");
			resp.sendRedirect("courses");
			return null;
		}

		final Student student = (Student) req.getSession(true).getAttribute(S_STUD);
		final int studId = Integer.parseInt(student.getId());
		model.put("studId", studId);
		model.put("course", course);

		return new ModelAndView("s/course", model);
	}

	public ModelAndView do_launch(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		VersionLookup lookup = versionLookup(req, resp, true);
		if (lookup == null) {
			return null;
		}

		final StringWriter verSw = new StringWriter();
		mapper.writeValue(verSw, lookup.getVer());
		final Version verCopy = mapper.readValue(verSw.toString(), Version.class);

		final Group group = (Group) req.getSession(true).getAttribute(S_GROUP);
		final AssignmentPath path = lookup.createPath(group);
		final long lastStamp = codeDao.findLastStamp(path);

		if (!lookup.getAss().isShared() || lastStamp >= 0) {
			verCopy.setSolution(codeDao.findCodeByStamp(path, lastStamp));
		}

		final StringWriter verCopySol = new StringWriter();
		mapper.writeValue(verCopySol, verCopy);
		final String solutionStr = verCopySol.toString();

		final HashMap<String, Object> model = lookup.getModel();
		model.put("verBean", verCopy);
		//	LATER use HTTP instead of applet parameter for passing the problem/code to applet
		model.put("ver", solutionStr.replaceAll("&", "&amp;").replaceAll("\"", "&quot;"));
		model.put("upHeader", "JSESSIONID=" + req.getSession(true).getId());
		model.put("upPath", lookup.getPath());
		model.put("cacheBustingToken", cacheBustingToken);

		return new ModelAndView("s/launch", model);
	}

	public ModelAndView do_upload(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		VersionLookup lookup = versionLookup(req, resp, false);
		if (lookup == null) {
			return null;
		}

		final Group group = (Group) req.getSession(true).getAttribute(S_GROUP);
		final BufferedReader codeReader = req.getReader();
		try {
			codeDao.createCode(lookup.createPath(group), codeReader);
		} catch (IOException e) {
			resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}

		resp.setStatus(HttpServletResponse.SC_OK);
		return null;
	}

	protected VersionLookup versionLookup(HttpServletRequest req, HttpServletResponse resp, final boolean redirect) throws IOException {
		final VersionLookup vl = new VersionLookup();
		vl.model = auth(req, resp, redirect);
		if (vl.model == null) {
			return null;
		}

		vl.path = req.getParameter("path");
		final String[] ids = vl.path.split("--");
		if (ids.length != 4) {
			if (redirect) {
				Message.addWarn(req, "malformed path:" + Arrays.toString(ids));
				resp.sendRedirect("courses");
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "malformed path:" + Arrays.toString(ids));
			}
			return null;
		}

		final String courseId = ids[0];
		vl.course = courseDao.findCourse(courseId);
		if (vl.course == null) {
			if (redirect) {
				Message.addWarn(req, "course not found by id " + courseId);
				resp.sendRedirect("courses");
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "course not found by id " + courseId);
			}
			return null;
		}

		vl.assBundleIndex = G4Parse.parse(ids[1], -1);
		if (vl.assBundleIndex < 0 || vl.course.getAssBundles().length <= vl.assBundleIndex) {
			if (redirect) {
				Message.addWarn(req, "bundle not found by index " + vl.assBundleIndex);
				resp.sendRedirect("course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "bundle not found by index " + vl.assBundleIndex);
			}
			return null;
		}

		vl.assId = ids[2];
		final AssignmentBundle bundle = vl.course.getAssBundles()[vl.assBundleIndex];
		vl.ass = IdName.findById(bundle.getAssignments(), vl.assId);
		if (vl.ass == null) {
			if (redirect) {
				Message.addWarn(req, "assignment not found by id " + vl.assId);
				resp.sendRedirect("course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "assignment not found by id " + vl.assId);
			}
			return null;
		}

		final String verIdStr = ids[3];
		vl.ver = IdName.findById(vl.ass.getVersions(), verIdStr);
		if (vl.ver == null) {
			if (redirect) {
				Message.addWarn(req, "version not found by id " + verIdStr);
				resp.sendRedirect("course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "version not found by id " + verIdStr);
			}
			return null;
		}

		vl.student = (Student) req.getSession(true).getAttribute(S_STUD);
		final int studId = Integer.parseInt(vl.student.getId());
		final int verIdx = IdName.indexOfId(vl.ass.getVersions(), vl.ver.getId());
		if (!vl.ass.isShared() && (studId) % vl.ass.getVersions().length != verIdx) {
			if (redirect) {
				Message.addWarn(req, "variant mismatch");
				resp.sendRedirect("course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "variant mismatch");
			}
			return null;
		}

		return vl;
	}

	static class VersionLookup {
		protected HashMap<String, Object> model;
		protected String path;
		protected Course course;
		protected int assBundleIndex;
		protected String assId;
		protected Assignment ass;
		protected Version ver;
		protected Student student;

		public VersionLookup() {
		}

		public Course getCourse() {
			return course;
		}

		public int getAssBundleIndex() {
			return assBundleIndex;
		}

		public String getAssId() {
			return assId;
		}

		public Version getVer() {
			return ver;
		}

		public Student getStudent() {
			return student;
		}

		public Assignment getAss() {
			return ass;
		}

		public HashMap<String, Object> getModel() {
			return model;
		}

		public String getPath() {
			return path;
		}

		protected AssignmentPath createPath(Group group) {
			return new AssignmentPath(
					getCourse().getId(), group.getId(), getStudent(),
					getAssBundleIndex(), getAssId(), getVer().getId()
			);
		}
	}
}