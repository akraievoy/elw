package elw.dao;

import elw.vo.Course;
import elw.vo.Enrollment;
import org.akraievoy.gear.G;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.*;

public class EnrollDao {
	private static final Logger log = LoggerFactory.getLogger(EnrollDao.class);

	protected final ObjectMapper mapper;
	protected final CourseDao courseDao;
	protected int cacheTime = 180000;

	final Map<String, Enrollment> enrollCache = new TreeMap<String, Enrollment>();

	protected long cacheStamp = 0;

	public void setCacheTime(int cacheTime) {
		this.cacheTime = cacheTime;
	}

	public EnrollDao(ObjectMapper mapper, CourseDao courseDao) {
		this.mapper = mapper;
		this.courseDao = courseDao;
	}

	public synchronized String[] findEnrollmentIds() {
		refreshCache();

		final Set<String> keys = enrollCache.keySet();
		return keys.toArray(new String[keys.size()]);
	}

	public void refreshCache() {
		final long now = System.currentTimeMillis();
		if (now - cacheStamp >= cacheTime) {
			cacheStamp = now;
		} else {
			return;
		}

		final File enrollDir = new File(System.getProperty("user.home"), "elw-data/enroll");
		final File[] groupFiles = enrollDir.listFiles(new FileFilter() {
			public boolean accept(File pathname) {
				return pathname.isFile() && pathname.getName().endsWith(".json");
			}
		});

		for (File groupFile : groupFiles) {
			try {
				final Enrollment enrollment = mapper.readValue(groupFile, Enrollment.class);
				if (groupFile.getName().equals(enrollment.getId() + ".json")) {
					enrollCache.put(enrollment.getId(), enrollment);
				} else {
					log.warn("wrong id at file {}", groupFile.getPath());
				}
			} catch (IOException e) {
				log.warn("failed to read {}: {}", groupFile.getPath(), G.report(e));
				log.trace("trace", e);
			}
		}
	}

	public synchronized Enrollment findEnrollment(final String id) {
		refreshCache();

		return enrollCache.get(id);
	}

	public Enrollment[] findAllEnrollments() {
		refreshCache();

		final Collection<Enrollment> enrollments = enrollCache.values();
		return enrollments.toArray(new Enrollment[enrollments.size()]);
	}

	public Enrollment[] findEnrollmentsForGroupId(final String groupId) {
		refreshCache();

		final Collection<Enrollment> allEnrollments = enrollCache.values();
		final List<Enrollment> groupEnrollments = new ArrayList<Enrollment>();
		for (Enrollment e : allEnrollments) {
			if (groupId.equals(e.getGroupId())) {
				groupEnrollments.add(e);
			}
		}

		return groupEnrollments.toArray(new Enrollment[groupEnrollments.size()]);
	}

	public List<Course> findCoursesByGroupId(String groupId) {
		final Enrollment[] enrollments = findEnrollmentsForGroupId(groupId);
		List<Course> courses = new ArrayList<Course>();
		for (Enrollment e : enrollments) {
			final Course course = courseDao.findCourse(e.getCourseId());
			if (course == null) {
				continue;
			}
			courses.add(course);
		}
		return courses;
	}

	public Enrollment findEnrollmentForGroupAndCourse(String groupId, String courseId) {
		final Enrollment[] enrollments = findEnrollmentsForGroupId(groupId);
		Enrollment enr = null;
		for (Enrollment e : enrollments) {
			if (e.getCourseId().equals(courseId)) {
				enr = e;
				break;
			}
		}
		return enr;
	}
}