package elw.dao.ctx;

import elw.vo.*;

/**
 * Relatively harmless parameter object, storing the full Score context.
 */
public class Scores extends Solutions {
    public final Solution solution;
    
    public Scores(
            Group group,
            Student student,
            Course course,
            int idx,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot,
            Solution solution
    ) {
        super(group, student, course, idx, task, tType, ver, slot);
        this.solution = solution;
    }
}
