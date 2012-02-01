package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;
import elw.vo.Class;

/**
 * Parameter Object, storing the full Solution context.
 */
public class SolutionsOfSlot extends SlotsOfTask {
    public final FileSlot slot;

    public SolutionsOfSlot(
            Enrollment enr,
            Group group,
            Student student,
            Course course,
            int idx,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot
    ) {
        super(enr, course, group, student, idx, task, tType, ver);

        this.slot = slot;
    }
    
    public ScoresOfSolution scores(final Solution solution) {
        return new ScoresOfSolution(
                enr, group, student, course,
                idx, task, tType, ver,
                slot, solution
        );
    }

    public long dueMillis() {
        final Class classDue = Nav.classDue(enr, idxEntry, slot);

        return classDue.getToDateTime().getMillis();
    }

}
