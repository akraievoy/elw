package elw.dao.rest;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * All scores for given enrollment.
 */
public class RestEnrollmentSummary {
    private final TreeMap<String, RestStudentSummary> studentToScore =
            new TreeMap<String, RestStudentSummary>();

    public void register(
            final String studentId,
            final String index,
            final String slotId,
            final RestSlotSummary slotSummary
    ) {
        final RestStudentSummary updated;
        final RestStudentSummary existing = studentToScore.get(studentId);
        if (existing != null) {
            updated = existing;
        } else {
            final RestStudentSummary created = new RestStudentSummary();
            studentToScore.put(studentId, created);
            updated = created;
        }

        updated.register(index, slotId, slotSummary);
    }

    public SortedMap<String, RestStudentSummary> getStudentToScore() {
        return Collections.unmodifiableSortedMap(studentToScore);
    }
}
