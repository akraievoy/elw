package elw.dao.ctx;

import elw.vo.State;
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
        super(
                enr, course, group,
                student, idx, task, tType, ver
        );

        this.slot = slot;
    }

    public ScoresOfSolution scores(final Solution solution) {
        return new ScoresOfSolution(
                enr, group, student, course,
                idx, task, tType, ver,
                slot, solution
        );
    }

    public State state(List<Solution> solutions) {
        State state;
        if (!open()) {
            state = State.CLOSED;
        } else if (solutions.isEmpty()) {
            state = State.OPEN;
        } else if (someApproved(solutions)) {
            state = State.APPROVED;
        } else if (allDeclined(solutions)) {
            state = State.DECLINED;
        } else {
            state = State.PENDING;
        }
        return state;
    }

    public Class dueClass() {
        final int classDueIndex =
                idxEntry.getClassDue().get(slot.getId());

        final Class classDue =
                enr.getClasses().get(classDueIndex);

        return classDue;
    }

    public long dueMillis() {
        return dueClass().getToDateTime().getMillis();
    }

    public boolean due() {
        long now = System.currentTimeMillis();
        return dueMillis() <= now;
    }

    public boolean someApproved(List<Solution> solutions) {
        for (Solution solution : solutions) {
            if (solution.getScore().state() == State.APPROVED) {
                return true;
            }
        }

        return false;
    }

    public boolean allDeclined(List<Solution> solutions) {
        if (solutions.isEmpty()) {
            return false;
        }

        for (Solution solution : solutions) {
            if (solution.getScore().state() != State.DECLINED) {
                return false;
            }
        }

        return true;
    }

    public ScoresOfSolution bestApproved(List<Solution> solutions) {
        ScoresOfSolution best = null;

        for (Solution solution : solutions) {
            if (solution.getScore().state() == State.APPROVED) {
                final ScoresOfSolution curr = scores(solution);
                if (best == null) {
                    best = curr;
                    continue;
                }

                double bestPoints = best.lastScore().points();
                double currPoints = curr.lastScore().points();
                if (bestPoints < currPoints) {
                    best = curr;
                }
            }
        }
        
        return best;
    }

    public ScoresOfSolution lastPending(List<Solution> solutions) {
        ScoresOfSolution last = null;

        for (Solution solution : solutions) {
            if (solution.getScore().state() == State.PENDING) {
                final ScoresOfSolution curr = scores(solution);
                if (last == null) {
                    last = curr;
                    continue;
                }

                final Long lastStamp = last.solution.getStamp();
                final Long currStamp = curr.solution.getStamp();
                if (lastStamp < currStamp) {
                    last = curr;
                }
            }
        }

        return last;
    }
    
    public double pointsBudget() {
        return idxEntry.getScoreBudget() * slot.getScoreWeight();
    }
}
