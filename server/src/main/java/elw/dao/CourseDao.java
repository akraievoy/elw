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

public class CourseDao extends Dao<Course> {
	private static final Logger log = LoggerFactory.getLogger(CourseDao.class);

	public CourseDao() {
	}

	@Override
	public Path pathFromMeta(Course course) {
		return new Path(course.getId());
	}

	public synchronized String[] findCourseIds() {
		final Path pathAll = new Path(new String[]{null});
		final String[][] pathElems = listCriteria(pathAll, null, false, true, false, null);

		return pathElems[0];
	}

	public synchronized Course findCourse(final String id) {
		if (id == null) {
			return null;
		}

		final Entry<Course> entry = findLast(new Path(id), null, null);

		return entry.getMeta();
	}

	public Course[] findAllCourses() {
		final String[] courseIds = findCourseIds();
		return load(courseIds);
	}

	protected Course[] load(String[] courseIds) {
		final Course[] courses = new Course[courseIds.length];

		for (int i = 0; i < courses.length; i++) {
			courses[i] = findCourse(courseIds[i]);
		}

		return courses;
	}
}
