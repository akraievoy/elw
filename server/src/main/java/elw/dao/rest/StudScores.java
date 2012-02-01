package elw.dao.rest;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * All scores for student's solutions.
 */
public class StudScores {
    private final SortedMap<String, SortedMap<String, Score>> indexToSlotToScore =
            new TreeMap<String, SortedMap<String, Score>>();

    public void register(
            String index,
            String slotId,
            Score score
    ) {
        final SortedMap<String, Score> slotToScoreExisting =
                indexToSlotToScore.get(index);
        final SortedMap<String, Score> slotToScoreUpdated;
        if (slotToScoreExisting == null) {
            final SortedMap<String, Score> slotToScoreCreated =
                    new TreeMap<String, Score>();
            indexToSlotToScore.put(index, slotToScoreCreated);
            slotToScoreUpdated = slotToScoreCreated;
        } else {
            slotToScoreUpdated = slotToScoreExisting;
        }

        slotToScoreUpdated.put(slotId, score);
    }

    public SortedMap<String, SortedMap<String, Score>> getIndexToSlotToScore() {
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
