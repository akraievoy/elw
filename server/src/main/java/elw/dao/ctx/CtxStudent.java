package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;

import java.util.Iterator;
import java.util.Map;

/**
 *  Parameter Object, storing the full enrollment context.
 */
public class CtxStudent extends CtxEnrollment {

    public final Student student;
    public final Iterable<CtxTask> tasks;

    public CtxStudent(
            Enrollment enr, Course course,
            Group group, Student student
    ) {
        super(enr, course, group);
        this.student = student;
        this.tasks = new Iterable<CtxTask>() {
            public Iterator<CtxTask> iterator() {
                final Iterator<Map.Entry<String, IndexEntry>> indexIterator =
                        CtxStudent.this.enr.getIndex().entrySet().iterator();

                return new Iterator<CtxTask>() {
                    public boolean hasNext() {
                        return indexIterator.hasNext();
                    }

                    public CtxTask next() {
                        return task(indexIterator.next().getKey());
                    }

                    public void remove() {
                        indexIterator.remove();
                    }
                };
            }
        };
    }

    //  LATER use IndexEntry reference instead of indexKey
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
