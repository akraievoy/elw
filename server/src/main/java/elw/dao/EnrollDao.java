package elw.dao;

import elw.vo.Course;
import elw.vo.Enrollment;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EnrollDao extends Dao<Enrollment> {
	private static final Logger log = LoggerFactory.getLogger(EnrollDao.class);

	protected final CourseDao courseDao;

	public EnrollDao(CourseDao courseDao) {
		this.courseDao = courseDao;
	}

	@Override
	public Path pathFromMeta(Enrollment e) {
		return new Path(new String[] {e.getGroupId(), e.getCourseId(), e.getId()});
	}

	public String[] findEnrollmentIds() {
		final String[] pathAll = {null, null, null};
		final String[][] pathElems = listCriteria(new Path(pathAll), null, false, true, false, null);

		return pathElems[2];
	}

	public synchronized Enrollment findEnrollment(final String id) {
		if (id == null) {
			return null;
		}

		final Path pathId = new Path(new String[]{null, null, id});
		final Entry<Enrollment> entry = findLast(pathId, null, null);

		return entry.getMeta();
	}

	public Enrollment[] findAllEnrollments() {
		final String[] enrIds = findEnrollmentIds();

		return load(enrIds);
	}

	protected Enrollment[] load(String[] enrIds) {
		final Enrollment[] enrs = new Enrollment[enrIds.length];

		for (int i = 0; i < enrs.length; i++) {
			enrs[i] = findEnrollment(enrIds[i]);
		}

		return enrs;
	}

	public Enrollment[] findEnrollmentsForGroupId(final String groupId) {
		final Path pathGroup = new Path(new String[]{groupId, null, null});
		final String[][] pathElems = listCriteria(pathGroup, null, false, true, false, null);
		final String[] enrIds = pathElems[2];

		return load(enrIds);
	}

	public Course[] findCoursesByGroupId(String groupId) {
		final Path pathGroup = new Path(new String[]{groupId, null, null});
		final String[][] pathElems = listCriteria(pathGroup, null, false, true, false, null);
		final String[] courseIds = pathElems[1];

		return courseDao.load(courseIds);
	}

	public Enrollment findEnrollmentForGroupAndCourse(String groupId, String courseId) {
		final Path pathGroupAndCourse = new Path(new String[]{groupId, courseId, null});
		final String[][] pathElems = listCriteria(pathGroupAndCourse, null, false, true, false, null);
		final String[] enrollmentIds = pathElems[2];

		if (enrollmentIds.length == 0) {
			return null;
		}
		if (enrollmentIds.length > 1) {
			log.warn("miltiple enrollments for group {} and course {}", groupId, courseId);
		}
		return load(enrollmentIds)[0];
	}
}