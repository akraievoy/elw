package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;
import elw.vo.Class;

import java.util.List;

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

    public boolean isOpen() {
        long now = System.currentTimeMillis();
        return dueMillis() <= now;
    }

    public boolean isSomeApproved(List<Solution> solutions) {
        for (Solution solution : solutions) {
            if (Boolean.TRUE.equals(solution.getScore().getApproved())) {
                return true;
            }
        }

        return false;
    }

    public boolean isAllDeclined(List<Solution> solutions) {
        if (solutions.isEmpty()) {
            return false;
        }

        for (Solution solution : solutions) {
            if (!Boolean.FALSE.equals(solution.getScore().getApproved())) {
                return false;
            }
        }

        return true;
    }
}
