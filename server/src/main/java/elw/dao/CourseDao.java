package elw.dao;

import elw.vo.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CourseDao extends Dao<Course> {
	private static final Logger log = LoggerFactory.getLogger(CourseDao.class);
	protected final AssDao assDao;
	protected final FileDao fileDao;

	public CourseDao(final AssDao assDao, final FileDao fileDao) {
		this.assDao = assDao;
		this.fileDao = fileDao;
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
		course.setFiles(fileDao.findFilesForCourse(Ctx.forCourse(course)));

		final AssignmentType[] assTypes = course.getAssTypes();
		for (final AssignmentType assType : assTypes) {
			//	LATER this should be done less intrusively
			assType.setAssignments(assDao.findAllForAssType(course, assType));

			final FileSlot[] slots = assType.getFileSlots();
			for (FileSlot slot : slots) {
				assType.setFiles(
						slot.getId(),
						fileDao.findFilesForAssType(Ctx.forAssType(course, assType), slot.getId())
				);
			}
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
