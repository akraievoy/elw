package elw.dao.ctx;

import elw.vo.*;
import elw.vo.Class;

/**
 * Parameter Object, storing the full Class/Task/Version context.
 */
public class CtxTask extends CtxStudent {
    public final int idx;
    public final IndexEntry idxEntry;
    public final TaskType tType;
    public final Task task;
    public final Version ver;

    public CtxTask(
            Enrollment enr,
            Course course,
            Group group,
            Student student,
            int idx,
            Task task,
            TaskType tType,
            Version ver
    ) {
        super(enr, course, group, student);
        //  LATER simplify idx/indexEntry
        this.idx = idx;
        this.idxEntry = enr.getIndex().get(idx);
        this.tType = tType;
        this.task = task;
        this.ver = ver;
    }

    public CtxSlot slot(final FileSlot slot) {
        final CtxSlot ctxSlot = new CtxSlot(
                enr, group, student, course,
                idx, task, tType, ver,
                slot
        );

        return propagateTZCache(ctxSlot);
    }

    public Class openClass() {
        final int classFromIndex =
                idxEntry.getClassFrom();

        final int classFromIndexSafe = 
                Math.min(
                        classFromIndex,
                        enr.getClasses().size() - 1
                );

        final Class classFrom =
                enr.getClasses().get(classFromIndexSafe);

        return classFrom;
    }

    public long openMillis() {
        final Class classOpen = openClass();

        return classOpen.getFromDateTime().getMillis();
    }

    public boolean open() {
        long now = System.currentTimeMillis();
        return openMillis() <= now;
    }
}
