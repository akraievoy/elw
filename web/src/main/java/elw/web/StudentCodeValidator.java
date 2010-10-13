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
	protected final FileDao fileDao;
	protected final MipsValidator validator;

	public StudentCodeValidator(
			ScheduledExecutorService executor,
			CodeDao codeDao, EnrollDao enrollDao, GroupDao groupDao,
			CourseDao courseDao, ScoreDao scoreDao, FileDao fileDao) {
		super(executor);
		this.codeDao = codeDao;
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

			final Student[] students = ctxEnr.getGroup().getStudents();
			for (Student student : students) {
				final Ctx ctxStud = ctxEnr.extendStudent(student);
				for (int index = 0; index < enr.getIndex().size(); index++) {
					final Ctx ctxVer = ctxStud.extendIndex(index);
					final Map<Stamp, Entry<CodeMeta>> metas = codeDao.findAllMetas(ctxVer);
					final Set<Stamp> stamps = metas.keySet();
					for (Stamp stamp : stamps) {
						final Entry<CodeMeta> entry = metas.get(stamp);
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
/*
							try {
								final Result[] resRef = {new Result("unknown", false)};
								final int[] passFailCounts = new int[2];
								validator.batch(resRef, ctxVer.getVer(), entry.dumpText(), passFailCounts);
								metaSafe.setTestsFailed(passFailCounts[1]);
								metaSafe.setTestsPassed(passFailCounts[0]);
							} catch (Throwable t) {
								log.warn("exception while validating {} / {}", ctxVer, stamp);
							} finally {
								metaSafe.setValidatorStamp(System.currentTimeMillis());
								entry.closeStreams();
							}
*/
						}

						if (update) {
							try {
								codeDao.updateMeta(ctxVer, stamp, metaSafe);
							} catch (IOException t) {
								log.warn("exception while storing update {} / {}", ctxVer, stamp);
							}
						}
					}
				}
			}
		}
	}
}
