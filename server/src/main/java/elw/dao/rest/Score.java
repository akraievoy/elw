package elw.dao.rest;

import elw.dao.ctx.SolutionsOfSlot;
import elw.vo.Solution;

import java.util.List;

/**
 * Complete ReST-view of score for one particular Task / Version / FileSlot.
 */
public class Score {
    /**
     * @see Score#create(elw.dao.ctx.SolutionsOfSlot, java.util.List
     */
    public Score() {
        //  nothing to do here
    }

    public static Score create(SolutionsOfSlot solutionsOfSlot, List<Solution> solutions) {
        final Score score = new Score();

        score.taskTypeId = solutionsOfSlot.tType.getId();
        score.taskTypeName = solutionsOfSlot.tType.getName();

        score.taskId = solutionsOfSlot.task.getId();
        score.taskName = solutionsOfSlot.task.getName();

        score.versionId = solutionsOfSlot.ver.getId();
        score.versionName = solutionsOfSlot.ver.getName();

        score.openMillis = solutionsOfSlot.openMillis();
        score.dueMillis = solutionsOfSlot.dueMillis();

        if (!solutionsOfSlot.isOpen()) {
            score.state = SolutionState.CLOSED;
        } else if (solutions.isEmpty()) {
            score.state = SolutionState.OPEN;
        } else if (solutionsOfSlot.isSomeApproved(solutions)) {
            score.state = SolutionState.APPROVED;
        } else if (solutionsOfSlot.isAllDeclined(solutions)) {
            score.state = SolutionState.DECLINED;
        } else {
            score.state = SolutionState.PENDING;
        }

        return score;
    }

    private SolutionState state;
    public SolutionState getState() { return state; }
    public void setState(SolutionState state) { this.state = state; }

    private long openMillis;
    public long getOpenMillis() { return openMillis; }
    public void setOpenMillis(long openMillis) { this.openMillis = openMillis; }

    private long dueMillis;
    public long getDueMillis() { return dueMillis; }
    public void setDueMillis(long dueMillis) { this.dueMillis = dueMillis; }

    //  TODO populate
    private int daysOverdue;
    public int getDaysOverdue() { return daysOverdue; }
    public void setDaysOverdue(int daysOverdue) { this.daysOverdue = daysOverdue; }

    //  TODO populate
    private int daysOpen;
    public int getDaysOpen() { return daysOpen; }
    public void setDaysOpen(int daysOpen) { this.daysOpen = daysOpen; }

    //  TODO populate
    private int daysPending;
    public int getDaysPending() { return daysPending; }
    public void setDaysPending(int daysPending) { this.daysPending = daysPending; }

    //  TODO populate
    private String sourceAddress;
    public String getSourceAddress() { return sourceAddress; }
    public void setSourceAddress(String sourceAddress) { this.sourceAddress = sourceAddress; }

    //  TODO populate
    private double pointsApproved;
    public double getPointsApproved() { return pointsApproved; }
    public void setPointsApproved(double pointsApproved) { this.pointsApproved = pointsApproved; }

    //  TODO populate
    private double pointsPending;
    public double getPointsPending() { return pointsPending; }
    public void setPointsPending(double pointsPending) { this.pointsPending = pointsPending; }

    //  TODO populate
    private double pointsBudget;
    public double getPointsBudget() { return pointsBudget; }
    public void setPointsBudget(double pointsBudget) { this.pointsBudget = pointsBudget; }
    
    //  TODO populate
    private String fileId;
    public String getFileId() { return fileId; }
    public void setFileId(String fileId) { this.fileId = fileId; }

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

    //  FIXME add list of criterias
}
