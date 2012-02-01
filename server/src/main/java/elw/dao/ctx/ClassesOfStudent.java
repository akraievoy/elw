package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;

/**
 *  Parameter Object, storing the full enrollment context.
 */
public class ClassesOfStudent {
    public final Enrollment enr;
    public final Group group;
    public final Student student;
    public final Course course;

    public ClassesOfStudent(Enrollment enr, Course course, Student student, Group group) {
        this.enr = enr;
        this.course = course;
        this.student = student;
        this.group = group;
    }
    
    public SlotsOfTask slots(final int idxPos) {
        final IndexEntry idxEntry = enr.getIndex().get(idxPos);
        final String[] path = idxEntry.getPath();
        final String taskTypeId = path[0];
        final String taskId = path[1];

        final TaskType taskType = course.getTaskTypes().get(taskTypeId);
        final Task task = taskType.getTasks().get(taskId);
        final Version ver = Nav.resolveVersion(
                task, idxEntry, group, student.getId()
        );

        final SlotsOfTask slotsOfTask = new SlotsOfTask(
                enr, course, group, student,
                idxPos, task, taskType, ver
        );

        return slotsOfTask;
    }
}
