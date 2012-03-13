package elw.dao.ctx;

import elw.vo.*;

/**
 * Parameter Object, storing the full Attachment context.
 */
public class CtxAttachment extends CtxSlot {
    public final Attachment attachment;

    public CtxAttachment(
            Enrollment enr,
            Group group,
            Student student,
            Course course,
            IndexEntry indexEntry,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot,
            Attachment attachment
    ) {
        super(enr, group, student, course, indexEntry, task, tType, ver, slot);
        this.attachment = attachment;
    }
}
