package elw.dao.rest;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * All scores for student's solutions.
 */
public class StudScores {
    private final SortedMap<String, SortedMap<String, SlotScore>> indexToSlotToScore =
            new TreeMap<String, SortedMap<String, SlotScore>>();

    public void register(
            String index,
            String slotId,
            SlotScore slotScore
    ) {
        final SortedMap<String, SlotScore> slotToScoreExisting =
                indexToSlotToScore.get(index);
        final SortedMap<String, SlotScore> slotToScoreUpdated;
        if (slotToScoreExisting == null) {
            final SortedMap<String, SlotScore> slotToScoreCreated =
                    new TreeMap<String, SlotScore>();
            indexToSlotToScore.put(index, slotToScoreCreated);
            slotToScoreUpdated = slotToScoreCreated;
        } else {
            slotToScoreUpdated = slotToScoreExisting;
        }

        slotToScoreUpdated.put(slotId, slotScore);
    }

    public SortedMap<String, SortedMap<String, SlotScore>> getIndexToSlotToScore() {
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
