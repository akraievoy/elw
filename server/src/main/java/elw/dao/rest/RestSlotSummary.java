package elw.dao.rest;

import elw.dao.ctx.CtxSlot;
import elw.vo.Solution;
import elw.vo.State;

import java.util.*;

/**
 * Complete ReST-view of score for one particular Task / Version / FileSlot.
 */
public class RestSlotSummary {
    /**
     * States which are computed simply as enrollment budgets.
     */
    public static final List<State> SIMPLE_STATES =
            Collections.unmodifiableList(
                    Arrays.asList(
                            State.CLOSED, State.OPEN, State.DECLINED
                    )
            );

    /**
     * @see RestSlotSummary#create(elw.dao.ctx.CtxSlot, java.util.List)
     */
    public RestSlotSummary() {
        //  nothing to do here
    }

    public static RestSlotSummary create(
            CtxSlot ctxSlot,
            List<Solution> solutions
    ) {
        final RestSlotSummary slotSummary = new RestSlotSummary();

        slotSummary.taskTypeId = ctxSlot.tType.getId();
        //  FIXME move to rest enrollment
        slotSummary.taskTypeName = ctxSlot.tType.getName();

        slotSummary.taskId = ctxSlot.task.getId();
        slotSummary.taskName = ctxSlot.task.getName();

        slotSummary.versionId = ctxSlot.ver.getId();
        slotSummary.versionName = ctxSlot.ver.getName();

        slotSummary.openMillis = ctxSlot.openMillis();
        slotSummary.dueMillis = ctxSlot.dueMillis();

        slotSummary.state = ctxSlot.state(solutions);

        slotSummary.bestApproved = RestScore.create(ctxSlot.bestApproved(solutions));
        slotSummary.lastPending =  RestScore.create(ctxSlot.lastPending(solutions));
        //  FIXME move solution-specific props to score or some wrapper hereof

        return slotSummary;
    }

    private final SortedMap<State, Double> points =
            new TreeMap<State, Double>();
    public SortedMap<State, Double> getPoints() {
        return Collections.unmodifiableSortedMap(points);
    }

    private State state;
    public State getState() { return state; }
    public void setState(State state) { this.state = state; }

    public void precachePointTotals(final CtxSlot ctxSlot) {
        clearPoints(points);

        final double pointsForSlot =
                ctxSlot.pointsForSlot();

        final RestScore lastPending =
                getLastPending();

        final State slotState = getState();
        if (SIMPLE_STATES.contains(slotState)) {
            increment(points, slotState, pointsForSlot);
        } else if (slotState == State.PENDING) {
            increment(points, State.PENDING, lastPending.getPoints());
        } else if (slotState == State.APPROVED) {
            //  here we track both approved AND pending solutions
            //      so we're able to aggregate the data
            increment(
                    points,
                    State.APPROVED,
                    getBestApproved().getPoints()
            );
            if (lastPending != null) {
                increment(points, State.PENDING, lastPending.getPoints());
            }
        } else {
            throw new IllegalStateException("was unreachable before");
        }
    }

    private RestScore bestApproved;
    public RestScore getBestApproved() { return bestApproved; }
    public void setBestApproved(RestScore bestApproved) {
        this.bestApproved = bestApproved;
    }

    private RestScore lastPending;
    public RestScore getLastPending() { return lastPending; }
    public void setLastPending(RestScore lastPending) {
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

    public static void increment(
            final SortedMap<State, Double> points,
            final State addedState,
            final double pointsForSlot
    ) {
        points.put(addedState, points.get(addedState) + pointsForSlot);
    }

    public static void clearPoints(
            final SortedMap<State, Double> points
    ) {
        for (State state : State.values()) {
            points.put(state, (double) 0);
        }
    }
}
