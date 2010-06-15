package elw.web;

import elw.dao.*;
import elw.miniweb.Message;
import elw.vo.*;
import org.akraievoy.gear.G4Parse;
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
	private final ReportDao reportDao;

	protected final ObjectMapper mapper = new ObjectMapper();
	protected final long cacheBustingToken = System.currentTimeMillis();

	private static final int UPLOAD_LIMIT = 2 * 1024 * 1024;

	public StudentController(CourseDao courseDao, GroupDao groupDao, EnrollDao enrollDao, final CodeDao codeDao, ReportDao reportDao) {
		this.courseDao = courseDao;
		this.groupDao = groupDao;
		this.enrollDao = enrollDao;
		this.codeDao = codeDao;
		this.reportDao = reportDao;
	}

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final String pathToRoot) throws IOException {
		final Group group = (Group) req.getSession(true).getAttribute(S_GROUP);
		final Student student = (Student) req.getSession(true).getAttribute(S_STUD);

		if (group == null || student == null) {
			if (pathToRoot != null) {
				Message.addWarn(req, "Login required");
				resp.sendRedirect(pathToRoot + "login");
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

					List<Course> courses = enrollDao.findCoursesByGroupId(group.getId());
					if (courses.size() == 1) {
						resp.sendRedirect("course?id="+courses.get(0).getId());
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

		final Group group = (Group) req.getSession().getAttribute(S_GROUP);

		final String groupId = group.getId();
		List<Course> courses = enrollDao.findCoursesByGroupId(groupId);

		model.put("courses", courses.toArray(new Course[courses.size()]));

		return new ModelAndView("s/courses", model);
	}

	@RequestMapping(value = "course", method = RequestMethod.GET)
	public ModelAndView do_course(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Group group = (Group) req.getSession(true).getAttribute(S_GROUP);
		final Course course = courseDao.findCourse(req.getParameter("id"));
		if (course == null) {
			Message.addWarn(req, "course not found (or not enrolled)");
			resp.sendRedirect("courses");
			return null;
		}

		final Enrollment enr = enrollDao.findEnrollmentForGroupAndCourse(group.getId(), course.getId());
		if (enr == null) {
			Message.addWarn(req, "course not enrolled");
			resp.sendRedirect("courses");
			return null;
		}

		final Student student = (Student) req.getSession(true).getAttribute(S_STUD);
		final int studId = Integer.parseInt(student.getId());
		model.put("studId", studId);
		model.put("course", course);
		model.put("enr", enr);

		final HashMap<String, CodeMeta> codeMetas = new HashMap<String, CodeMeta>();
		final HashMap<String, ReportMeta> reportMetas = new HashMap<String, ReportMeta>();
		storeMetas(codeDao, reportDao, course, group, student, codeMetas, reportMetas, "");
		model.put("codeMetas", codeMetas);
		model.put("reportMetas", reportMetas);

		return new ModelAndView("s/course", model);
	}

	public static void storeMetas(
			CodeDao codeDao, ReportDao reportDao,
			Course course, Group group, Student student,
			HashMap<String, CodeMeta> codeMetas, HashMap<String, ReportMeta> reportMetas, final String prefix
	) {
		for (int bunI = 0, assBundlesLength = course.getAssBundles().length; bunI < assBundlesLength; bunI++) {
			AssignmentBundle bundle = course.getAssBundles()[bunI];
			for (Assignment ass : bundle.getAssignments()) {
				for (Version ver : ass.getVersions()) {
					if (isVersionIncorrect(student, ass, ver)) {
						continue;
					}
					String path = course.getId() + "--" + bunI + "--" + ass.getId() + "--" + ver.getId();
					if (prefix != null && prefix.length() > 0) {
						path = prefix + "--" + path;
					}
					final AssignmentPath assPath = new AssignmentPath(
							course.getId(), group.getId(), student,
							bunI, ass.getId(), ver.getId()
					);
					if (codeMetas != null) {
						final long lastStamp = codeDao.findLastStamp(assPath);
						codeMetas.put(path, codeDao.findMetaByStamp(assPath, lastStamp));
					}
					if (reportMetas != null) {
						final long lastStampReport = reportDao.findLastStamp(assPath);
						reportMetas.put(path, reportDao.findMetaByStamp(assPath, lastStampReport));
					}
				}
			}
		}
	}

	@RequestMapping(value = "uploadReport", method = RequestMethod.POST)
	public ModelAndView do_uploadReport(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
		VersionLookup lookup = versionLookup(req, resp, "../");
		if (lookup == null) {
			return null;
		}

		if (lookup.getAss().isShared()) {
			Message.addWarn(req, "shared assignment do not require any reports");
			resp.sendRedirect("course?id=" + lookup.course.getId());
			return null;
		}

		if (req.getContentLength() > UPLOAD_LIMIT) {
			Message.addWarn(req, "apparently you're trying to upload more than " + G4mat.formatMem(UPLOAD_LIMIT));
			resp.sendRedirect("uploadPage?path="+lookup.getPath());
			return null;
		}

		final DiskFileItemFactory fif = new DiskFileItemFactory();
		fif.setRepository(new File(System.getProperty("java.io.tmpdir")));
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
				resp.sendRedirect("uploadPage?path="+lookup.getPath());
				return null;
			}

			final String fileName = item.getName();
			if (!fileName.endsWith(".rtf")) {
				Message.addWarn(req, "is that an .rtf file or what?");
				resp.sendRedirect("uploadPage?path="+lookup.getPath());
				return null;
			}

			final String contentType = item.getContentType();
			if (!"application/rtf".equalsIgnoreCase(contentType) && !"application/msword".equalsIgnoreCase(contentType)) {
				Message.addWarn(req, "contentType is '" + contentType + "', which is not what is expected for an .rtf file");
				resp.sendRedirect("uploadPage?path="+lookup.getPath());
				return null;
			}

			final InputStream itemIs = item.openStream();
			reportDao.createReport(lookup.createPath(), itemIs, resolveRemoteAddress(req));
			fileCount++;
		}

		if (fileCount == 1) {
			Message.addInfo(req, "Your upload has succeeded");
			resp.sendRedirect("uploadPage?path="+lookup.getPath());
		} else {
			Message.addWarn(req, "Something went terribly wrong");
			resp.sendRedirect("uploadPage?path="+lookup.getPath());
		}

		return null;
	}

	@RequestMapping(value = "uploadPage", method = RequestMethod.GET)
	public ModelAndView do_uploadPage(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		VersionLookup lookup = versionLookup(req, resp, "");
		if (lookup == null) {
			return null;
		}

		if (lookup.getAss().isShared()) {
			Message.addWarn(req, "shared assignments do not require any reports");
			resp.sendRedirect("course?id=" + lookup.course.getId());
			return null;
		}

		final HashMap<String, Object> model = lookup.getModel();
		model.put("verBean", lookup.getVer());
		model.put("upPath", lookup.getPath());
		model.put("course", lookup.getCourse());
		model.put("uploadLimit", G4mat.formatMem(UPLOAD_LIMIT));
		final Map<Long,ReportMeta> reports = reportDao.findAllMetas(lookup.createPath());

		for (Iterator<ReportMeta> metaIt = reports.values().iterator(); metaIt.hasNext();) {
			ReportMeta reportMeta = metaIt.next();
			final String fileName =
					lookup.getStudent().getName().replaceAll("\\s+", "_") + "-" +
					lookup.getAss().getId() + "-" +
					ReportMeta.getFileNameUploadStamp(reportMeta.getUploadStamp()) +
					".rtf";

			reportMeta.setFileName(fileName);
		}

		model.put("reports", reports);

		return new ModelAndView("s/uploadPage", model);
	}

	@RequestMapping(value = "reportDl/*.*", method = RequestMethod.GET)
	public ModelAndView do_reportDl(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		VersionLookup lookup = versionLookup(req, resp, "");
		if (lookup == null) {
			return null;
		}

		if (lookup.getAss().isShared()) {
			Message.addWarn(req, "shared assignments do not require any reports");
			resp.sendRedirect("course?id=" + lookup.course.getId());
			return null;
		}

		final String stampStr = req.getParameter("stamp");
		if (stampStr == null) {
			Message.addWarn(req, "no stamp defined");
			resp.sendRedirect("uploadPage?path=" + req.getParameter("path"));
			return null;
		}	

		final long stamp = G4Parse.parse(stampStr, -1L);
		InputStream report = null;
		try {
			report = reportDao.findReportByStamp(lookup.createPath(), stamp);
			if (report == null) {
				Message.addWarn(req, "no report for stamp " + stamp);
				resp.sendRedirect("uploadPage?path=" + req.getParameter("path"));
				return null;
			}

			resp.setCharacterEncoding("UTF-8");
			resp.setContentType("application/rtf");
			resp.setHeader("Content-Disposition", "attachment;");

			final byte[] buf = new byte[32768];
			int bytesRead;
			while ((bytesRead = report.read(buf)) >= 0) {
				resp.getOutputStream().write(buf, 0, bytesRead);
			}
		} finally {
			if (report != null) {
				try {
					report.close();
				} catch (IOException e) {
					log.warn("failed on close", e);
				}
			}
		}

		return null;
	}

	@RequestMapping(value = "launch", method = RequestMethod.GET)
	public ModelAndView do_launch(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		VersionLookup lookup = versionLookup(req, resp, "");
		if (lookup == null) {
			return null;
		}

		final StringWriter verSw = new StringWriter();
		mapper.writeValue(verSw, lookup.getVer());
		final Version verCopy = mapper.readValue(verSw.toString(), Version.class);

		final AssignmentPath path = lookup.createPath();
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
		model.put("course", lookup.getCourse());

		return new ModelAndView("s/launch", model);
	}

	@RequestMapping(value = "upload", method = RequestMethod.POST)
	public ModelAndView do_upload(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		VersionLookup lookup = versionLookup(req, resp, "");
		if (lookup == null) {
			return null;
		}

		final BufferedReader codeReader = req.getReader();
		try {
			codeDao.createCode(lookup.createPath(), codeReader, resolveRemoteAddress(req));
		} catch (IOException e) {
			resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}

		resp.setStatus(HttpServletResponse.SC_OK);
		return null;
	}

	protected static String resolveRemoteAddress(HttpServletRequest req) {
		return req.getRemoteAddr();
	}

	protected VersionLookup versionLookup(HttpServletRequest req, HttpServletResponse resp, final String pathToRoot) throws IOException {
		final VersionLookup vl = new VersionLookup();
		vl.model = auth(req, resp, pathToRoot);
		if (vl.model == null) {
			return null;
		}

		vl.path = req.getParameter("path");
		if (vl.path == null) {
			if (pathToRoot != null) {
				Message.addWarn(req, "no assignment path");
				resp.sendRedirect(pathToRoot + "courses");
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "no assignment path");
			}
			return null;
		}

		final String[] ids = vl.path.split("--");
		if (ids.length != 4) {
			if (pathToRoot != null) {
				Message.addWarn(req, "malformed path:" + Arrays.toString(ids));
				resp.sendRedirect(pathToRoot + "courses");
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "malformed path:" + Arrays.toString(ids));
			}
			return null;
		}

		final String courseId = ids[0];
		vl.course = courseDao.findCourse(courseId);
		if (vl.course == null) {
			if (pathToRoot != null) {
				Message.addWarn(req, "course not found by id " + courseId);
				resp.sendRedirect(pathToRoot + "courses");
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "course not found by id " + courseId);
			}
			return null;
		}

		vl.assBundleIndex = G4Parse.parse(ids[1], -1);
		if (vl.assBundleIndex < 0 || vl.course.getAssBundles().length <= vl.assBundleIndex) {
			if (pathToRoot != null) {
				Message.addWarn(req, "bundle not found by index " + vl.assBundleIndex);
				resp.sendRedirect(pathToRoot + "course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "bundle not found by index " + vl.assBundleIndex);
			}
			return null;
		}

		vl.assId = ids[2];
		final AssignmentBundle bundle = vl.course.getAssBundles()[vl.assBundleIndex];
		vl.ass = IdName.findById(bundle.getAssignments(), vl.assId);
		if (vl.ass == null) {
			if (pathToRoot != null) {
				Message.addWarn(req, "assignment not found by id " + vl.assId);
				resp.sendRedirect(pathToRoot + "course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "assignment not found by id " + vl.assId);
			}
			return null;
		}

		final String verIdStr = ids[3];
		vl.ver = IdName.findById(vl.ass.getVersions(), verIdStr);
		if (vl.ver == null) {
			if (pathToRoot != null) {
				Message.addWarn(req, "version not found by id " + verIdStr);
				resp.sendRedirect(pathToRoot + "course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "version not found by id " + verIdStr);
			}
			return null;
		}

		vl.student = (Student) req.getSession(true).getAttribute(S_STUD);
		vl.group = (Group) req.getSession(true).getAttribute(S_GROUP);
		vl.enr = enrollDao.findEnrollmentForGroupAndCourse(
				vl.group.getId(),
				vl.course.getId()
		);
		if (vl.enr == null) {
			if (pathToRoot != null) {
				Message.addWarn(req, "course not enrolled");
				resp.sendRedirect(pathToRoot + "courses");
			} else {
				resp.sendError(HttpServletResponse.SC_FORBIDDEN, "course not enrolled");
			}
			return null;
		}
		if (!vl.enr.getClasses()[vl.ass.getScoring().getClassFrom()].isStarted()) {
			if (pathToRoot != null) {
				Message.addWarn(req, "task not open yet");
				resp.sendRedirect(pathToRoot + "course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_FORBIDDEN, "task not open yet");
			}
			return null;
		}

		if (isVersionIncorrect(vl.student, vl.ass, vl.ver)) {
			if (pathToRoot != null) {
				Message.addWarn(req, "variant mismatch");
				resp.sendRedirect(pathToRoot + "course?id=" + vl.course.getId());
			} else {
				resp.sendError(HttpServletResponse.SC_NOT_FOUND, "variant mismatch");
			}
			return null;
		}

		return vl;
	}

	public static boolean isVersionIncorrect(Student student, Assignment ass, Version ver) {
		final int studId = Integer.parseInt(student.getId());
		final int verIdx = IdName.indexOfId(ass.getVersions(), ver.getId());

		return !ass.isShared() && (studId) % ass.getVersions().length != verIdx;
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
		protected Group group;
		protected Enrollment enr;

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

		public Group getGroup() {
			return group;
		}

		public Enrollment getEnr() {
			return enr;
		}

		protected AssignmentPath createPath() {
			return new AssignmentPath(
					getCourse().getId(), getGroup().getId(), getStudent(),
					getAssBundleIndex(), getAssId(), getVer().getId()
			);
		}
	}
}