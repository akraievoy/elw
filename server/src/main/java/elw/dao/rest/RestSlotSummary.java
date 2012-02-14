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
        
        slotSummary.slotInfo = RestSlotInfo.create(ctxSlot);
        slotSummary.state = ctxSlot.state(solutions);
        slotSummary.bestApproved = RestSolution.create(
                ctxSlot.bestApproved(solutions), false
        );
        slotSummary.lastPending =  RestSolution.create(
                ctxSlot.lastPending(solutions), false
        );

        return slotSummary;
    }

    private final SortedMap<State, Double> points =
            new TreeMap<State, Double>();
    public SortedMap<State, Double> getPoints() {
        return Collections.unmodifiableSortedMap(points);
    }

    public void precachePointTotals(final CtxSlot ctxSlot) {
        clearPoints(points);

        final double pointsForSlot =
                ctxSlot.pointsForSlot();

        final RestScore lastPendingScore =
                getLastPending() != null ? getLastPending().getScore() : null;
        final RestScore bestApprovedScore =
                getBestApproved() != null ? getBestApproved().getScore() : null;

        final State slotState = getState();
        if (SIMPLE_STATES.contains(slotState)) {
            increment(points, slotState, pointsForSlot);
        } else if (slotState == State.PENDING && lastPendingScore != null) {
            increment(points, State.PENDING, lastPendingScore.getPoints());
        } else if (slotState == State.APPROVED && bestApprovedScore != null) {
            //  here we track both approved AND pending solutions
            //      so we're able to aggregate the data
            increment(
                    points,
                    State.APPROVED,
                    bestApprovedScore.getPoints()
            );
            if (lastPendingScore != null) {
                increment(points, State.PENDING, lastPendingScore.getPoints());
            }
        } else {
            throw new IllegalStateException("was unreachable before");
        }
    }

    private RestSlotInfo slotInfo = new RestSlotInfo();
    public RestSlotInfo getSlotInfo() { return slotInfo; }

    private State state;
    public State getState() { return state; }
    public void setState(State state) { this.state = state; }

    private RestSolution bestApproved;
    public RestSolution getBestApproved() { return bestApproved; }

    private RestSolution lastPending;
    public RestSolution getLastPending() { return lastPending; }

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
