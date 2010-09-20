package elw.dao;

import elw.vo.Course;
import elw.vo.Group;
import org.akraievoy.gear.G;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class CourseDao extends Dao<Ctx, Course> {
	private static final Logger log = LoggerFactory.getLogger(CourseDao.class);

	public CourseDao() {
	}

	public synchronized String[] findCourseIds() {
		final String[][] pathElems = listCriteria(criteria, null, false, true, false, null);

		return pathElems[0];
	}

	public synchronized Course findCourse(final String id) {
		if (id == null) {
			return null;
		}

		final Ctx groupCtx = new Ctx(Ctx.STATE_C, null, null, null, id, -1, null, null);
		final Entry<Course> entry = findLast(groupCtx, null, null);

		return entry.getMeta();
	}

	public Course[] findAllCourses() {
		final String[] groupIds = findCourseIds();
		final Course[] courses = new Course[groupIds.length];

		for (int i = 0; i < courses.length; i++) {
			courses[i] = findCourse(groupIds[i]);
		}

		return courses;
	}
}
