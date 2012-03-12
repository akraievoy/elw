package elw.dao.ctx;

import elw.vo.State;
import elw.vo.*;
import elw.vo.Class;

import java.util.List;
import java.util.Map;

/**
 * Parameter Object, storing the full Solution context.
 */
public class CtxSlot extends CtxTask {
    public final FileSlot slot;

    public CtxSlot(
            Enrollment enr,
            Group group,
            Student student,
            Course course,
            IndexEntry indexEntry,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot
    ) {
        super(
                enr, course, group,
                student, indexEntry, task, tType, ver
        );

        this.slot = slot;
    }

    public CtxSolution solution(final Solution solution) {
        final CtxSolution ctxSolution = new CtxSolution(
                enr, group, student, course,
                indexEntry, task, tType, ver,
                slot, solution
        );

        return propagateTZCache(ctxSolution);
    }

    public State state(List<Solution> solutions) {
        if (!open()) {
            return State.CLOSED;
        }

        if (solutions.isEmpty()) {
            return State.OPEN;
        }

        if (someInState(solutions, State.APPROVED)) {
            return State.APPROVED;
        }

        if (allInState(solutions, State.DECLINED)) {
            return State.DECLINED;
        }

        return State.PENDING;
    }

    public Class dueClass() {
        final Map<String,String> classDueMap =
                indexEntry.getClassDue();

        if (classDueMap == null || classDueMap.isEmpty()) {
            return null;
        }

        final String classDue =
                classDueMap.get(slot.getId());

        if (classDue == null) {
            return null;
        }

        return classForKey(enr.getClasses(), classDue);
    }

    public long dueMillis() {
        final Class dueClass = dueClass();
        
        if (dueClass == null) {
            return Long.MAX_VALUE;
        }

        //  TODO timezone is not accounted for in Class.java
        return dueClass.getToDateTime().getMillis();
    }

    public boolean dueOver() {
        long now = System.currentTimeMillis();
        return dueMillis() < now;
    }

    public boolean dueToday() {
        long now = System.currentTimeMillis();
        return dueMillis() < endOfDay(now).getMillis();
    }

    public boolean dueNextTwoWeeks() {
        long now = System.currentTimeMillis();
        return dueMillis() < endOfDay(now).plusDays(14).getMillis();
    }

    public static boolean someInState(
            final List<Solution> solutions,
            final State state
    ) {
        for (Solution solution : solutions) {
            if (solution.getScore().state() == state) {
                return true;
            }
        }

        return false;
    }

    public static boolean allInState(
            final List<Solution> solutions,
            final State state
    ) {
        if (solutions.isEmpty()) {
            return false;
        }

        for (Solution solution : solutions) {
            if (solution.getScore().state() != state) {
                return false;
            }
        }

        return true;
    }

    public CtxSolution bestApproved(List<Solution> solutions) {
        CtxSolution best = null;

        for (Solution solution : solutions) {
            if (solution.getScore().state() == State.APPROVED) {
                final CtxSolution curr = solution(solution);
                if (best == null) {
                    best = curr;
                    continue;
                }

                double bestPoints = best.lastScore().pointsForSolution();
                double currPoints = curr.lastScore().pointsForSolution();

                if (bestPoints < currPoints) {
                    best = curr;
                }
            }
        }
        
        return best;
    }

    public CtxSolution lastPending(List<Solution> solutions) {
        CtxSolution last = null;

        for (Solution solution : solutions) {
            if (solution.getScore().state() == State.PENDING) {
                final CtxSolution curr = solution(solution);
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
    
    public double pointsForSlot() {
        return indexEntry.getScoreBudget() * slot.getScoreWeight();
    }
    
    public boolean dueApplies(DueState state) {
        if (state == DueState.EVER) {
            return dueClass() != null;
        }
        
        if (state == DueState.OVERDUE) {
            return dueOver();
        }
        
        if (state == DueState.TODAY) {
            return dueToday();
        }

        //  I would like default branch to be articulated explicitly
        //noinspection SimplifiableIfStatement
        if (state == DueState.NEXT_TWO_WEEKS) {
            return dueNextTwoWeeks();
        }

        return true;
    }
    
    public String[] solutionPathElems() {
        return new String[] {
                group.getId(),
                student.getId(),
                course.getId(),
                indexEntry.getId(),
                tType.getId(),
                task.getId(),
                ver.getId(),
                slot.getId(),
        };

    }
}
