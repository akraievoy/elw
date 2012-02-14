package elw.dao.rest;

import elw.dao.ctx.CtxSlot;
import org.codehaus.jackson.map.annotate.JsonSerialize;

/**
 * Common attributes for slot or an entity within slot.
 */
@JsonSerialize(include=JsonSerialize.Inclusion.NON_NULL)
public class RestSlotInfo {
    protected String taskTypeId;
    public String getTaskTypeId() { return taskTypeId; }

    protected String taskTypeName;
    public String getTaskTypeName() { return taskTypeName; }

    protected String taskId;
    public String getTaskId() { return taskId; }

    protected String taskName;
    public String getTaskName() { return taskName; }

    protected String versionId;
    public String getVersionId() { return versionId; }

    protected String versionName;
    public String getVersionName() { return versionName; }

    private long openMillis;
    public long getOpenMillis() { return openMillis; }

    private String openNice;
    public String getOpenNice() { return openNice; }

    private long dueMillis;
    public long getDueMillis() { return dueMillis; }

    private String dueNice;
    public String getDueNice() { return dueNice; }

    private String studentId;
    public String getStudentId() { return studentId; }

    private String studentName;
    public String getStudentName() { return studentName; }

    public static RestSlotInfo create(CtxSlot ctx) {
        final RestSlotInfo slotInfo = new RestSlotInfo();

        slotInfo.taskTypeId = ctx.tType.getId();
        slotInfo.taskTypeName = ctx.tType.getName();
        slotInfo.taskId = ctx.task.getId();
        slotInfo.taskName = ctx.task.getName();
        slotInfo.versionId = ctx.ver.getId();
        slotInfo.versionName = ctx.ver.getName();

        slotInfo.openMillis = ctx.openMillis();
        slotInfo.openNice = ctx.dateTimeNice(slotInfo.openMillis);
        if (ctx.dueClass() != null) {
            slotInfo.dueMillis = ctx.dueMillis();
            slotInfo.dueNice = ctx.dateTimeNice(slotInfo.dueMillis);
        }

        slotInfo.studentId = ctx.student.getId();
        slotInfo.studentName = ctx.student.getName();

        return slotInfo;
    }
}
