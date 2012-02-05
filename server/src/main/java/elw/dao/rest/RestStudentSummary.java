package elw.dao.rest;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * All scores for student's solutions.
 */
public class RestStudentSummary {
    private final SortedMap<String, SortedMap<String, RestSlotSummary>> indexToSlotToScore =
            new TreeMap<String, SortedMap<String, RestSlotSummary>>();

    public void register(
            String index,
            String slotId,
            RestSlotSummary slotSummary
    ) {
        final SortedMap<String, RestSlotSummary> slotToScoreExisting =
                indexToSlotToScore.get(index);
        final SortedMap<String, RestSlotSummary> slotToScoreUpdated;
        if (slotToScoreExisting == null) {
            final SortedMap<String, RestSlotSummary> slotToScoreCreated =
                    new TreeMap<String, RestSlotSummary>();
            indexToSlotToScore.put(index, slotToScoreCreated);
            slotToScoreUpdated = slotToScoreCreated;
        } else {
            slotToScoreUpdated = slotToScoreExisting;
        }

        slotToScoreUpdated.put(slotId, slotSummary);
    }

    public SortedMap<String, SortedMap<String, RestSlotSummary>> getIndexToSlotToScore() {
        return Collections.unmodifiableSortedMap(indexToSlotToScore);
    }

    public double getPointsOpen() {
        return 0.0; //  FIXME proceed with computed value
    }

    public double getPointsApproved() {
        return 0.0; //  FIXME proceed with computed value
    }

    public double getPointsPending() {
        return 0.0; //  FIXME proceed with computed value
    }

    public double getPointsBudget() {
        return 0.0; //  FIXME proceed with computed value
    }
}
