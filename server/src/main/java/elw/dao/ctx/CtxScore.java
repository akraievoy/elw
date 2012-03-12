package elw.dao.ctx;

import elw.vo.*;

/**
 * Parameter Object, storing the full Score context.
 */
public class CtxScore extends CtxSolution {
    public final Score score;

    public CtxScore(
            Enrollment enr,
            Group group,
            Student student,
            Course course,
            IndexEntry indexEntry,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot,
            Solution solution,
            Score score
    ) {
        super(
                enr, group, student,
                course, indexEntry, task, tType, ver,
                slot, solution
        );
        this.score = score;
    }

    @Override
    public Score score() {
        return score;
    }
}
