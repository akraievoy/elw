package elw.web;

import elw.dao.CourseDao;
import elw.vo.Course;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;

public class AdminController extends MultiActionController {
	protected final CourseDao courseDao;

	public AdminController(CourseDao courseDao) {
		this.courseDao = courseDao;
	}

	public ModelAndView do_courses(final HttpServletRequest req, final HttpServletResponse resp) {
		resp.setCharacterEncoding("UTF-8");
		resp.setContentType("text/html; charset=UTF-8");

		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put("courses", courseDao.findAllCourses());

		return new ModelAndView("a/courses", model);
	}

	public ModelAndView do_course(final HttpServletRequest req, final HttpServletResponse resp) {
		final Course course = courseDao.findCourse(req.getParameter("id"));
		if (course == null) {
			resp.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return null;
		}

		resp.setCharacterEncoding("UTF-8");
		resp.setContentType("text/html; charset=UTF-8");

		final HashMap<String, Object> model = new HashMap<String, Object>();

		model.put("course", course);

		return new ModelAndView("a/course", model);
	}
}
