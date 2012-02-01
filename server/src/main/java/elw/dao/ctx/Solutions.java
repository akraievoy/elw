package elw.dao.ctx;

import elw.vo.*;

/**
 * Relatively harmless parameter object, storing the full Solution context.
 */
public class Solutions {
    public final Group group;
    public final Student student;
    public final Course course;
    public final int idx;   //  TODO morph to String
    public final TaskType tType;
    public final Task task;
    public final Version ver;
    public final FileSlot slot;

    public Solutions(
            Group group,
            Student student,
            Course course,
            int idx,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot
    ) {
        this.course = course;
        this.group = group;
        this.idx = idx;
        this.slot = slot;
        this.student = student;
        this.task = task;
        this.tType = tType;
        this.ver = ver;
    }
    
    public Scores scores(final Solution solution) {
        return new Scores(
                group, student, course,
                idx, task, tType, ver,
                slot, solution
        );
    }
}
