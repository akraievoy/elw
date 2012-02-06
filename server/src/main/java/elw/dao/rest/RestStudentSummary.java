package elw.dao.rest;

import elw.dao.ctx.CtxSlot;
import elw.dao.ctx.CtxTask;
import elw.vo.State;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * All scores for student's solutions.
 */
public class RestStudentSummary {
    private final SortedMap<String, RestTaskSummary> tasks =
            new TreeMap<String, RestTaskSummary>();

    private final SortedMap<State, Double> points =
            new TreeMap<State, Double>();

    public void register(
            final CtxSlot ctxSlot,
            RestSlotSummary slotSummary
    ) {
        final RestTaskSummary existing =
                tasks.get(String.valueOf(ctxSlot.idx));

        final RestTaskSummary updated;
        if (existing == null) {
            final RestTaskSummary created =
                    new RestTaskSummary();
            tasks.put(String.valueOf(ctxSlot.idx), created);
            updated = created;
        } else {
            updated = existing;
        }

        updated.register(ctxSlot, slotSummary);
    }

    public void register(
            final CtxTask ctxTask,
            final RestTaskSummary taskSummary
    ) {
        taskSummary.precachePointTotals(ctxTask);

        tasks.put(String.valueOf(ctxTask.idx), taskSummary);
    }
    
    public void precachePointTotals() {
        RestSlotSummary.clearPoints(points);

        for (RestTaskSummary taskSummary : tasks.values()) {
            final SortedMap<State, Double> taskPoints =
                    taskSummary.getPoints();

            RestTaskSummary.increment(points, taskPoints);
        }
    }

    public SortedMap<State, Double> getPoints() {
        return Collections.unmodifiableSortedMap(points);
    }

    public SortedMap<String, RestTaskSummary> getTasks() {
        return Collections.unmodifiableSortedMap(tasks);
    }
}
