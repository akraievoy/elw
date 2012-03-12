package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;

/**
 *  Parameter Object, storing the full enrollment context.
 */
public class CtxStudent extends CtxEnrollment {

    public final Student student;

    public CtxStudent(
            Enrollment enr, Course course,
            Group group, Student student
    ) {
        super(enr, course, group);
        this.student = student;
    }

    public CtxTask task(final String indexKey) {
        final IndexEntry indexEntry = enr.getIndex().get(indexKey);
        final String taskTypeId = indexEntry.getTaskTypeId();
        final String taskId = indexEntry.getTaskId();

        final TaskType taskType = course.getTaskTypes().get(taskTypeId);
        final Task task = taskType.getTasks().get(taskId);
        final Version ver = Nav.resolveVersion(
                task, indexEntry, group, student.getId()
        );

        final CtxTask ctxTask = new CtxTask(
                enr, course, group, student,
                indexEntry, task, taskType, ver
        );

        return propagateTZCache(ctxTask);
    }

}
