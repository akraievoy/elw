package elw.dao.rest;

import elw.dao.ctx.CtxStudent;

import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * All scores for given enrollment, group-wise mapping
 * from student ID to summary per student.
 *
 * @see RestStudentSummary
 */
public class RestEnrollmentSummary {
    private final TreeMap<String, RestStudentSummary> students =
            new TreeMap<String, RestStudentSummary>();

    public void register(
            final CtxStudent ctxSlot,
            final RestStudentSummary studentSummary
    ) {
        studentSummary.precachePointTotals();

        final String studentId = ctxSlot.student.getId();

        students.put(studentId, studentSummary);
    }

    public SortedMap<String, RestStudentSummary> getStudents() {
        return Collections.unmodifiableSortedMap(students);
    }
}
