package elw.dao.rest;

import elw.dao.ctx.CtxSolution;
import elw.vo.FileType;
import elw.vo.IdNamed;

/**
 * ReST representation of Solution information.
 */
public class RestSolution {
    private String id;
    public String getId() { return id; }

    private String name;
    public String getName() { return name; }

    private String studentId;
    public String getStudentId() { return studentId; }

    private String studentName;
    public String getStudentName() { return studentName; }

    private String taskTypeId;
    public String getTaskTypeId() { return taskTypeId; }

    private String taskTypeName;
    public String getTaskTypeName() { return taskTypeName; }

    private String taskId;
    public String getTaskId() { return taskId; }

    private String taskName;
    public String getTaskName() { return taskName; }

    private String versionId;
    public String getVersionId() { return versionId; }

    private String versionName;
    public String getVersionName() { return versionName; }

    private String comment;
    public String getComment() { return comment; }

    private RestScore score;
    public RestScore getScore() { return score; }

    private long uploadMillis;
    public long getUploadMillis() { return uploadMillis; }

    private String uploadNice;
    public String getUploadNice() { return uploadNice; }

    private String sourceAddress;
    public String getSourceAddress() { return sourceAddress; }

    private String fileTypeId;
    public String getFileTypeId() { return fileTypeId; }

    private String fileTypeName;
    public String getFileTypeName() { return fileTypeName; }

    protected RestSolution() {
        //  nothing to do here
    }

    public static RestSolution create(CtxSolution ctx) {
        final RestSolution solution = new RestSolution();

        solution.comment = ctx.solution.getComment();
        solution.id = ctx.solution.getCouchId();
        solution.name = ctx.solution.getName();
        solution.score = RestScore.create(ctx);
        solution.studentId = ctx.student.getId();
        solution.studentName = ctx.student.getName();
        solution.taskTypeId = ctx.tType.getId();
        solution.taskTypeName = ctx.tType.getName();
        solution.taskId = ctx.task.getId();
        solution.taskName = ctx.task.getName();
        solution.versionId = ctx.ver.getId();
        solution.versionName = ctx.ver.getName();
        solution.uploadMillis = ctx.solution.getStamp();
        solution.uploadNice = ctx.dateTimeNice(solution.uploadMillis);
        solution.sourceAddress = ctx.solution.getSourceAddress();

        final FileType fileType = IdNamed._.one(ctx.solution.getFileType());
        solution.fileTypeId = fileType.getId();
        solution.fileTypeName = fileType.getName();

        return solution;
    }
}
