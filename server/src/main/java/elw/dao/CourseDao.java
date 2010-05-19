package elw.dao;

import elw.vo.Course;
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

public class CourseDao {
	private static final Logger log = LoggerFactory.getLogger(CourseDao.class);

	protected final ObjectMapper mapper;
	protected int cacheTime = 60000;

	final Map<String, Course> courseCache = new TreeMap<String, Course>();

	protected long cacheStamp = 0;

	public void setCacheTime(int cacheTime) {
		this.cacheTime = cacheTime;
	}

	public CourseDao(ObjectMapper mapper) {
		this.mapper = mapper;
	}

	public synchronized String[] findCourseIds() {
		refreshCache();

		final Set<String> keys = courseCache.keySet();
		return keys.toArray(new String[keys.size()]);
	}

	public void refreshCache() {
		final long now = System.currentTimeMillis();
		final long prevStamp = cacheStamp;

		if (now - cacheStamp >= cacheTime) {
			cacheStamp = now;
		} else {
			return;
		}

		final File coursesDir = new File(System.getProperty("user.home"), "elw-data/courses");
		final File[] courseFiles = coursesDir.listFiles(new FileFilter() {
			public boolean accept(File pathname) {
				return pathname.isFile() && pathname.getName().endsWith(".json");
			}
		});

		for (File courseFile : courseFiles) {
			if (courseFile.lastModified() < prevStamp) {
				log.warn("{} not modified since last read", courseFile.getPath());
				continue;
			}
			try {
				final Course course = mapper.readValue(courseFile, Course.class);
				if (courseFile.getName().equals(course.getId() + ".json")) {
					courseCache.put(course.getId(), course);
				} else {
					log.warn("wrong course id at file {}", courseFile.getPath());
				}
			} catch (IOException e) {
				log.warn("failed to read {}: {}", courseFile.getPath(), G.report(e));
				log.trace("trace", e);
			}
		}
	}

	public synchronized Course findCourse(final String id) {
		refreshCache();

		if (id == null) {
			return null;
		}

		return courseCache.get(id);
	}

	public Course[] findAllCourses() {
		refreshCache();

		final Collection<Course> courses = courseCache.values();
		return courses.toArray(new Course[courses.size()]);
	}
}
