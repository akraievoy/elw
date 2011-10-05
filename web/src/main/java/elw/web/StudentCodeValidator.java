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
import elw.dao.CouchDao;
import elw.dao.Ctx;
import elw.dao.Queries;
import elw.dp.mips.MipsValidator;
import elw.dp.mips.TaskBean;
import elw.vo.*;
import org.akraievoy.gear.G4Run;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;

public class StudentCodeValidator extends G4Run.Task {
    private static final Logger log = LoggerFactory.getLogger(StudentCodeValidator.class);

    private int periodMillis = 300000;
    private final CouchDao couchDao;
    private final Queries q;
    private final MipsValidator validator;

    public StudentCodeValidator(
            ScheduledExecutorService executor,
            CouchDao couchDao) {
        super(executor);
        this.couchDao = couchDao;
        this.validator = new MipsValidator();
        this.q = new Queries(couchDao);
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
        final List<Enrollment> enrs = q.enrollments();
        for (Enrollment enr : enrs) {
            final Ctx ctxEnr;
            {
                final Course course = q.course(enr.getCourseId());
                final Group group = q.group(enr.getGroupId());
                ctxEnr = Ctx.forEnr(enr).extendCourse(course).extendGroup(group);
            }

            if (!ctxEnr.getCourse().getId().contains("aos")) {
                continue;
            }

            for (Student student : ctxEnr.getGroup().getStudents().values()) {
                final Ctx ctxStud = ctxEnr.extendStudent(student);
                for (int index = 0; index < enr.getIndex().size(); index++) {
                    final Ctx ctxVer = ctxStud.extendIndex(index);
                    if (!"lr".equals(ctxVer.getAssType().getId())) {
                        continue;
                    }

                    final String slotId = "code";
                    final List<Solution> files = q.solutions(ctxVer, slotId);
                    for (Solution f : files) {
                        if (f.getValidatorStamp() > 0 && f.getScore() != null) {
                            continue;
                        }

                        Score score = null;
                        try {
                            final Result[] resRef = {new Result("unknown", false)};
                            final int[] passFailCounts = new int[2];
                            final List<Attachment> allStatements = q.attachments(ctxVer, "statement");
                            final List<Attachment> allTests = q.attachments(ctxVer, "test");
                            final List<String> allTestsStr = new ArrayList<String>();
                            for (int i = 0; i < allTestsStr.size(); i++) {
                                allTestsStr.add(couchDao.fileText(allTests.get(i), FileBase.CONTENT));
                            }
                            final TaskBean taskBean = new TaskBean(
                                    couchDao.fileText(allStatements.get(allStatements.size() - 1), FileBase.CONTENT),
                                    allTestsStr,
                                    ""
                            );
                            validator.batch(resRef, taskBean, couchDao.fileLines(f, FileBase.CONTENT), passFailCounts);
                            f.setTestsFailed(passFailCounts[1]);
                            f.setTestsPassed(passFailCounts[0]);

                            score = Queries.updateAutos(ctxVer, slotId, f, null);
                            final boolean passed = passFailCounts[1] == 0 && passFailCounts[0] > 0;
                            if (passed) {
                                score.setApproved(passed);
                            }
                        } catch (Throwable t) {
                            log.warn("exception while validating {} / {} / {}", new Object[]{ctxVer, f.getId(), f.getStamp()});
                        } finally {
                            f.setValidatorStamp(System.currentTimeMillis());
                        }

                        if (score != null) {
                            try {
                                couchDao.update(f);
                                score.setupPathElems(ctxVer, ctxVer.getAssType().getFileSlots().get(slotId), f);
                                couchDao.update(score);
                            } catch (Throwable t) {
                                log.warn("exception while storing update {} / {} / {}", new Object[]{ctxVer, f.getId(), f.getStamp()});
                            }
                        }
                    }
                }
            }
        }
    }
}
