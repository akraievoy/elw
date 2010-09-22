package elw.web;

import base.pattern.Result;
import elw.dao.*;
import elw.dp.mips.MipsValidator;
import elw.vo.*;
import org.akraievoy.gear.G4Run;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
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
	protected final ScoreDao scoreDao;
	protected final MipsValidator validator;

	public StudentCodeValidator(ScheduledExecutorService executor, CodeDao codeDao, EnrollDao enrollDao, GroupDao groupDao, CourseDao courseDao, ScoreDao scoreDao) {
		super(executor);
		this.codeDao = codeDao;
		this.enrollDao = enrollDao;
		this.groupDao = groupDao;
		this.courseDao = courseDao;
		this.scoreDao = scoreDao;
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
				for (AssignmentType assType: course.getAssTypes()) {
					for (Assignment ass : assType.getAssignments()) {
						for (Version ver : ass.getVersions()) {
							if (Ctx.isVersionIncorrect(student, ass, ver)) {
								continue;
							}
							final Ctx ctx = new Ctx(
									Ctx.STATE_EGSCTAV,
									enr.getId(), group.getId(), student.getId(),
									course.getId(), assType.getId(), ass.getId(), ver.getId()
							);
							ctx.resolve(enrollDao, groupDao, courseDao);

							final Map<Stamp,Dao.Entry<CodeMeta>> metas = codeDao.findAllMetas(ctx);
							final Set<Stamp> stamps = metas.keySet();
							for (Stamp stamp : stamps) {
								final Dao.Entry<CodeMeta> entry = metas.get(stamp);
								final CodeMeta meta = entry.getMeta();
								boolean update = false;
								final CodeMeta metaSafe;
								if (meta == null) {	//	FIXME now meta should be always defined!
									metaSafe = new CodeMeta();
									metaSafe.setUpdateStamp(stamp);
									update = true;
								} else {
									metaSafe = meta;
								}

								if (metaSafe.getValidatorStamp() <= 0) {
									update = true;
									try {
										final Result[] resRef = {new Result("unknown", false)};
										final int[] passFailCounts = new int[2];
										validator.batch(resRef, ver, entry.dumpText(), passFailCounts);
										metaSafe.setTestsFailed(passFailCounts[1]);
										metaSafe.setTestsPassed(passFailCounts[0]);
									} catch (Throwable t) {
										log.warn("exception while validating {} / {}", ctx, stamp);
									} finally {
										metaSafe.setValidatorStamp(System.currentTimeMillis());
										entry.closeStreams();
									}
								}

								if (update) {
									try {
										codeDao.updateMeta(ctx, stamp, metaSafe);
									} catch (IOException t) {
										log.warn("exception while storing update {} / {}", ctx, stamp);
									}
								}
							}
						}
					}
				}
			}
		}
	}
}
