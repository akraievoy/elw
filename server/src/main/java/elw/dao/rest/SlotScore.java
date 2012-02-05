package elw.dao.rest;

import elw.dao.ctx.ScoresOfSolution;
import elw.dao.ctx.SolutionsOfSlot;
import elw.vo.Solution;
import elw.vo.State;

import java.util.List;

/**
 * Complete ReST-view of score for one particular Task / Version / FileSlot.
 */
public class SlotScore {
    /**
     * @see SlotScore#create(elw.dao.ctx.SolutionsOfSlot, java.util.List)
     */
    public SlotScore() {
        //  nothing to do here
    }

    public static SlotScore create(
            SolutionsOfSlot solutionsOfSlot,
            List<Solution> solutions
    ) {
        final SlotScore slotScore = new SlotScore();

        slotScore.taskTypeId = solutionsOfSlot.tType.getId();
        //  FIXME move to rest enrollment
        slotScore.taskTypeName = solutionsOfSlot.tType.getName();

        slotScore.taskId = solutionsOfSlot.task.getId();
        slotScore.taskName = solutionsOfSlot.task.getName();

        slotScore.versionId = solutionsOfSlot.ver.getId();
        slotScore.versionName = solutionsOfSlot.ver.getName();

        slotScore.openMillis = solutionsOfSlot.openMillis();
        slotScore.dueMillis = solutionsOfSlot.dueMillis();

        slotScore.state = solutionsOfSlot.state(solutions);

        slotScore.bestApproved = SolutionScore.create(solutionsOfSlot.bestApproved(solutions));
        slotScore.lastPending =  SolutionScore.create(solutionsOfSlot.lastPending(solutions));
        //  FIXME move solution-specific props to score or some wrapper hereof

        return slotScore;
    }

    private State state;
    public State getState() { return state; }
    public void setState(State state) { this.state = state; }
    
    private SolutionScore bestApproved;
    public SolutionScore getBestApproved() { return bestApproved; }
    public void setBestApproved(SolutionScore bestApproved) {
        this.bestApproved = bestApproved;
    }

    private SolutionScore lastPending;
    public SolutionScore getLastPending() { return lastPending; }
    public void setLastPending(SolutionScore lastPending) {
        this.lastPending = lastPending;
    }

    //  FIXME this should be moved to enrollement rest representation
    private long openMillis;
    public long getOpenMillis() { return openMillis; }
    public void setOpenMillis(long openMillis) { this.openMillis = openMillis; }

    //  FIXME this should be moved to enrollement rest representation
    private long dueMillis;
    public long getDueMillis() { return dueMillis; }
    public void setDueMillis(long dueMillis) { this.dueMillis = dueMillis; }

    private String taskTypeId;
    public String getTaskTypeId() { return taskTypeId; }
    public void setTaskTypeId(String taskTypeId) { this.taskTypeId = taskTypeId; }

    private String taskTypeName;
    public String getTaskTypeName() { return taskTypeName; }
    public void setTaskTypeName(String taskTypeName) { this.taskTypeName = taskTypeName; }
    
    private String taskId;
    public String getTaskId() { return taskId; }
    public void setTaskId(String taskId) { this.taskId = taskId; }

    private String taskName;
    public String getTaskName() { return taskName; }
    public void setTaskName(String taskName) { this.taskName = taskName; }

    private String versionId;
    public String getVersionId() { return versionId; }
    public void setVersionId(String versionId) { this.versionId = versionId; }

    private String versionName;
    public String getVersionName() { return versionName; }
    public void setVersionName(String versionName) { this.versionName = versionName; }
}
