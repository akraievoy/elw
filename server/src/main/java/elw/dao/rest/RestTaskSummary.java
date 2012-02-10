package elw.dao.rest;

import elw.dao.ctx.CtxSlot;
import elw.dao.ctx.CtxTask;
import elw.vo.FileSlot;
import elw.vo.State;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Summary of task, per slot.
 */
public class RestTaskSummary {
    private final SortedMap<String, RestSlotSummary> slots =
            new TreeMap<String, RestSlotSummary>();

    private final SortedMap<State, Double> points =
            new TreeMap<State, Double>();

    public static void increment(
            final SortedMap<State, Double> points,
            final SortedMap<State, Double> increment
    ) {
        for (State state : State.values()) {
            points.put(
                state,
                points.get(state) + increment.get(state)
            );
        }
    }

    public void register(
            final CtxSlot ctxSlot,
            final RestSlotSummary slotSummary
    ) {
        slotSummary.precachePointTotals(ctxSlot);

        slots.put(ctxSlot.slot.getId(), slotSummary);
    }

    public void precachePointTotals(final CtxTask ctxTask) {
        RestSlotSummary.clearPoints(points);

        for (FileSlot slot : ctxTask.tType.getFileSlots().values()) {
            final RestSlotSummary slotSummary =
                    slots.get(slot.getId());

            if (slotSummary == null) {
                continue;
            }

            increment(
                    points, slotSummary.getPoints()
            );
        }
    }

    public SortedMap<String, RestSlotSummary> getSlots() {
        return Collections.unmodifiableSortedMap(slots);
    }

    public SortedMap<State, Double> getPoints() {
        return Collections.unmodifiableSortedMap(points);
    }
}
