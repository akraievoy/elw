/*
 * ELW : e-learning workspace
 * Copyright (C) 2010  Anton Kraievoy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package elw.web;

import base.pattern.Result;
import elw.dao.*;
import elw.dp.mips.MipsValidator;
import elw.dp.mips.TaskBean;
import elw.vo.*;
import org.akraievoy.gear.G4Run;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;

public class StudentCodeValidator extends G4Run.Task {
	private static final Logger log = LoggerFactory.getLogger(StudentCodeValidator.class);

	protected int periodMillis = 300000;
	protected final EnrollDao enrollDao;
	protected final GroupDao groupDao;
	protected final CourseDao courseDao;
	protected final ScoreDao scoreDao;
	protected final FileDao fileDao;
	protected final MipsValidator validator;

	public StudentCodeValidator(
			ScheduledExecutorService executor,
			EnrollDao enrollDao, GroupDao groupDao,
			CourseDao courseDao, ScoreDao scoreDao, FileDao fileDao) {
		super(executor);
		this.enrollDao = enrollDao;
		this.groupDao = groupDao;
		this.courseDao = courseDao;
		this.scoreDao = scoreDao;
		this.fileDao = fileDao;
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
			final Ctx ctxEnr;
			{
				final Course course = courseDao.findCourse(enr.getCourseId());
				final Group group = groupDao.findGroup(enr.getGroupId());
				ctxEnr = Ctx.forEnr(enr).extendCourse(course).extendGroup(group);
			}

			if (!ctxEnr.getCourse().getId().contains("aos")) {
				continue;
			}

			final Student[] students = ctxEnr.getGroup().getStudents();
			for (Student student : students) {
				final Ctx ctxStud = ctxEnr.extendStudent(student);
				for (int index = 0; index < enr.getIndex().size(); index++) {
					final Ctx ctxVer = ctxStud.extendIndex(index);
					if (!"lr".equals(ctxVer.getAssType().getId())) {
						continue;
					}

					final String slotId = "code";
					final Entry<FileMeta>[] files = fileDao.findFilesFor(FileDao.SCOPE_STUD, ctxVer, slotId);
					for (Entry<FileMeta> f : files) {
						if (f.getMeta().getValidatorStamp() > 0 && f.getMeta().getScore() != null) {
							continue;
						}

						Score score = null;
						try {
							final Result[] resRef = {new Result("unknown", false)};
							final int[] passFailCounts = new int[2];
							final Entry<FileMeta>[] allStatements = fileDao.findFilesFor(FileDao.SCOPE_VER, ctxVer, "statement");
							final Entry<FileMeta>[] allTests = fileDao.findFilesFor(FileDao.SCOPE_VER, ctxVer, "test");
							final String[] allTestsStr = new String[allTests.length];
							for (int i = 0; i < allTestsStr.length; i++) {
								allTestsStr[i] = allTests[i].getText();
							}
							final TaskBean taskBean = new TaskBean(
									allStatements[allStatements.length - 1].getText(),
									Arrays.asList(allTestsStr),
									""
							);
							validator.batch(resRef, taskBean, f.getText().split("\r\n|\r|\n"), passFailCounts);
							f.getMeta().setTestsFailed(passFailCounts[1]);
							f.getMeta().setTestsPassed(passFailCounts[0]);

							score = ScoreDao.updateAutos(ctxVer, slotId, f, null);
							score.setApproved(passFailCounts[1] == 0 && passFailCounts[0] > 0);
						} catch (Throwable t) {
							log.warn("exception while validating {} / {}", ctxVer, f.getMeta().getCreateStamp());
						} finally {
							f.getMeta().setValidatorStamp(System.currentTimeMillis());
							f.closeStreams();
						}

						if (score != null) {
							try {
								fileDao.update(new Path(f.getMeta().getPath()), f.getMeta(), null, null);
								scoreDao.createScore(ctxVer, slotId, f.getMeta().getId(), score);
							} catch (IOException t) {
								log.warn("exception while storing update {} / {}", ctxVer, f.getMeta().getCreateStamp());
							}
						}
					}
				}
			}
		}
	}
}
