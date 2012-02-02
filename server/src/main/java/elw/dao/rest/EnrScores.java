package elw.dao.rest;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * All scores for given enrollment.
 */
public class EnrScores {
    private final TreeMap<String, StudScores> studentToScore =
            new TreeMap<String, StudScores>();

    public void register(
            final String studentId,
            final String index,
            final String slotId,
            final SlotScore slotScore
    ) {
        final StudScores updated;
        final StudScores existing = studentToScore.get(studentId);
        if (existing != null) {
            updated = existing;
        } else {
            final StudScores created = new StudScores();
            studentToScore.put(studentId, created);
            updated = created;
        }

        updated.register(index, slotId, slotScore);
    }

    public SortedMap<String, StudScores> getStudentToScore() {
        return Collections.unmodifiableSortedMap(studentToScore);
    }
}
