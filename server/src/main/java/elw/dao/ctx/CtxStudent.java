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

    public CtxTask task(final int idxPos) {
        final IndexEntry idxEntry = enr.getIndex().get(idxPos);
        final String[] path = idxEntry.getPath();
        final String taskTypeId = path[0];
        final String taskId = path[1];

        final TaskType taskType = course.getTaskTypes().get(taskTypeId);
        final Task task = taskType.getTasks().get(taskId);
        final Version ver = Nav.resolveVersion(
                task, idxEntry, group, student.getId()
        );

        final CtxTask ctxTask = new CtxTask(
                enr, course, group, student,
                idxPos, task, taskType, ver
        );

        return ctxTask;
    }

}
