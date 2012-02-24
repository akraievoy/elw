package elw.dao.ctx;

import elw.vo.*;
import elw.vo.Class;

import java.util.List;

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

    //  some tasks consist completely or partially of shared versions
    //      solutions to which may queried or uploaded by students
    public CtxTask overrideToShared(Version sharedVersion) {
        if (!sharedVersion.isShared()) {
            throw new IllegalStateException("overriding to non-shared version");
        }

        final CtxTask ctxTask = new CtxTask(
                enr, course, group,
                student,
                idx, task, tType,
                sharedVersion
        );

        return propagateTZCache(ctxTask);
    }

    public static interface StateForSlot {
        State getState(FileSlot slot);
    }

    public boolean writable(
            final FileSlot slot,
            final StateForSlot stateForSlot
    ) {
        if (!slot.isWritable()) {
            return false;
        }

        final List<String> writeApprovals =
                slot.getWriteApprovals();

        for (String writeApproval : writeApprovals) {
            final FileSlot approvalSlot =
                    tType.getFileSlots().get(writeApproval);
            if (State.APPROVED != stateForSlot.getState(slot)) {
                return false;
            }
        }

        return true;
    }

    public boolean readable(FileSlot slot, StateForSlot stateForSlot) {
        final List<String> readApprovals =
                slot.getReadApprovals();

        for (String readApproval : readApprovals) {
            final FileSlot approvalSlot =
                    tType.getFileSlots().get(readApproval);
            if (State.APPROVED != stateForSlot.getState(slot)) {
                return false;
            }
        }

        return true;
    }
}
