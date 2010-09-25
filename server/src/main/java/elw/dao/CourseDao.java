package elw.dao;

import elw.vo.Assignment;
import elw.vo.AssignmentType;
import elw.vo.Course;
import elw.vo.Path;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CourseDao extends Dao<Course> {
	private static final Logger log = LoggerFactory.getLogger(CourseDao.class);
	protected final AssDao assDao;

	public CourseDao(AssDao assDao) {
		this.assDao = assDao;
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

		final Course course = entry.getMeta();
		final AssignmentType[] assTypes = course.getAssTypes();
		for (final AssignmentType assType : assTypes) {
			//	LATER this should be done less intrusively
			final Assignment[] asses = assDao.findAllForAssType(course.getId(), assType.getId());
			assType.setAssignments(asses);
		}

		return course;
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
