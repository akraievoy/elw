package elw.web;

import base.pattern.Result;
import elw.dao.*;
import elw.dp.mips.MipsValidator;
import elw.vo.*;
import org.akraievoy.gear.G4Run;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;

public class StudentCodeValidator extends G4Run.Task {
	private static final Logger log = LoggerFactory.getLogger(StudentCodeValidator.class);

	protected int periodMillis = 300000;
	protected final EnrollDao enrollDao;
	protected final CodeDao codeDao;
	protected final GroupDao groupDao;
	protected final CourseDao courseDao;
	protected final MipsValidator validator;

	public StudentCodeValidator(ScheduledExecutorService executor, CodeDao codeDao, EnrollDao enrollDao, GroupDao groupDao, CourseDao courseDao) {
		super(executor);
		this.codeDao = codeDao;
		this.enrollDao = enrollDao;
		this.groupDao = groupDao;
		this.courseDao = courseDao;
		this.validator = new MipsValidator();
	}

	public void setPeriodMillis(int periodMillis) {
		this.periodMillis = periodMillis;
	}

	protected long getRestartDelay() {
		return periodMillis;
	}

	protected long getInitialDelay() {
		return periodMillis;
	}

	protected void runInternal() throws Throwable {
		final Enrollment[] enrs = enrollDao.findAllEnrollments();
		for (Enrollment enr : enrs) {
			final Group group = groupDao.findGroup(enr.getGroupId());
			final Course course = courseDao.findCourse(enr.getCourseId());

			final Student[] students = group.getStudents();
			for (Student student : students) {
				for (int bunI = 0, assBundlesLength = course.getAssBundles().length; bunI < assBundlesLength; bunI++) {
					final AssignmentBundle bundle = course.getAssBundles()[bunI];
					for (Assignment ass : bundle.getAssignments()) {
						for (Version ver : ass.getVersions()) {
							if (StudentController.isVersionIncorrect(student, ass, ver)) {
								continue;
							}
							final AssignmentPath assPath = new AssignmentPath(
									course.getId(), group.getId(), student,
									bunI, ass.getId(), ver.getId()
							);
							final Map<Long,CodeMeta> metas = codeDao.findAllMetas(assPath);
							final Set<Long> stamps = metas.keySet();
							for (Long stamp : stamps) {
								final CodeMeta meta = metas.get(stamp);
								boolean update = false;
								final CodeMeta metaSafe;
								if (meta == null) {
									metaSafe = new CodeMeta();
									metaSafe.setUploadStamp(stamp);
									update = true;
								} else {
									metaSafe = meta;
								}

								if (metaSafe.getValidatorStamp() <= 0) {
									update = true;
									try {
										final Result[] resRef = {new Result("unknown", false)};
										final int[] passFailCounts = new int[2];
										validator.batch(resRef, ver, codeDao.findCodeByStamp(assPath, stamp), passFailCounts);
										metaSafe.setTestsFailed(passFailCounts[1]);
										metaSafe.setTestsPassed(passFailCounts[0]);
									} catch (Throwable t) {
										log.warn("exception while validating {} / {}", assPath, stamp);
									} finally {
										metaSafe.setValidatorStamp(System.currentTimeMillis());
									}
								}

								if (update) {
									codeDao.updateMeta(assPath, stamp, metaSafe);
								}
							}
						}
					}
				}
			}

		}
	}
}
