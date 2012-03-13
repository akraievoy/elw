package elw.dao.ctx;

import elw.vo.*;
import elw.vo.Class;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;

/**
 * Parameter Object, storing the full Class/Task/Version context.
 */
public class CtxTask extends CtxStudent {
    public final IndexEntry indexEntry;
    public final TaskType tType;
    public final Task task;
    public final Version ver;
    public final Iterable<CtxSlot> slots;

    public CtxTask(
            Enrollment enr,
            Course course,
            Group group,
            Student student,
            IndexEntry indexEntry,
            Task task,
            TaskType tType,
            Version ver
    ) {
        super(enr, course, group, student);
        this.indexEntry = indexEntry;
        this.tType = tType;
        this.task = task;
        this.ver = ver;
        this.slots = new Iterable<CtxSlot>() {
            public Iterator<CtxSlot> iterator() {
                final Iterator<Map.Entry<String, FileSlot>> slotIterator = 
                        CtxTask.this.tType.getFileSlots().entrySet().iterator();

                return new Iterator<CtxSlot>() {
                    public boolean hasNext() {
                        return slotIterator.hasNext();
                    }

                    public CtxSlot next() {
                        return slot(slotIterator.next().getValue());
                    }

                    public void remove() {
                        slotIterator.remove();
                    }
                };
            }
        };
    }

    protected static String safeClassKey(
            final SortedMap<String, Class> classMap,
            final String classKey
    ) {
        final SortedMap<String, Class> tailMap =
                classMap.tailMap(classKey);

        if (!tailMap.isEmpty()) {
            return tailMap.firstKey();
        }

        return classMap.lastKey();
    }
    
    public static Class classForKey(
            final SortedMap<String, Class> classMap,
            final String classKey
    ) {
        return classMap.get(safeClassKey(classMap, classKey));
    }

    public CtxSlot slot(final FileSlot slot) {
        final CtxSlot ctxSlot = new CtxSlot(
                enr, group, student, course,
                indexEntry, task, tType, ver,
                slot
        );

        return propagateTZCache(ctxSlot);
    }

    public Class openClass() {
        return classForKey(
                enr.getClasses(),
                indexEntry.getClassFrom()
        );
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
                indexEntry, task, tType,
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
            if (State.APPROVED != stateForSlot.getState(approvalSlot)) {
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
            if (State.APPROVED != stateForSlot.getState(approvalSlot)) {
                return false;
            }
        }

        return true;
    }
}
