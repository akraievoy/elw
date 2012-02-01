package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;
import elw.vo.Class;

/**
 * Parameter Object, storing the full Class/Task/Version context.
 */
public class Slots extends Classes {    //  FIXME rename
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
        //  LATER simplify idx/indexEntry
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
    
    public long openMillis() {
        final Class classFrom = Nav.classFrom(enr, idxEntry);

        return classFrom.getFromDateTime().getMillis();
    }

}
