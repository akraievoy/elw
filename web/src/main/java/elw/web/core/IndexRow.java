package elw.web.core;

/**
 * Used for student/admin entry-level index.
 */
public class IndexRow {
    public final int index;
    public final String enrId;
    public final String groupId;
    public final String groupName;
    public final String courseId;
    public final String courseName;
    public final String summaryRef;
    public final String tasksRef;
    public final String solutionsRef;
    public final String attachmentsRef;

    public IndexRow(
            int index,
            String enrId,
            String groupId, String groupName,
            String courseId, String courseName,
            String summaryRef, String tasksRef, String solutionsRef,
            String attachmentsRef
    ) {
        this.attachmentsRef = attachmentsRef;
        this.courseId = courseId;
        this.courseName = courseName;
        this.enrId = enrId;
        this.groupId = groupId;
        this.groupName = groupName;
        this.index = index;
        this.solutionsRef = solutionsRef;
        this.summaryRef = summaryRef;
        this.tasksRef = tasksRef;
    }
}
