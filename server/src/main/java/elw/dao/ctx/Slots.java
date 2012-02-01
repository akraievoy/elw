package elw.dao.ctx;

import elw.vo.*;

/**
 * Parameter Object, storing the full Class/Task/Version context.
 */
public class Slots extends Classes {
    public final int idx;
    public final IndexEntry idxEntry;
    public final TaskType tType;
    public final Task task;
    public final Version ver;

    public Slots(
            Enrollment enr,
            Course course,
            Group group,
            Student student,
            int idx,
            Task task,
            TaskType tType,
            Version ver
    ) {
        super(enr, course, student, group);
        //  LATER simplify this
        this.idx = idx;
        this.idxEntry = enr.getIndex().get(idx);
        this.tType = tType;
        this.task = task;
        this.ver = ver;
    }
    
    public Solutions solutions(final FileSlot slot) {
        final Solutions solutions = new Solutions(
                enr, group, student, course,
                idx, task, tType, ver,
                slot
        );

        return solutions;
    }
}
