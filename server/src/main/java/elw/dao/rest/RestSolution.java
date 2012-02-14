package elw.dao.rest;

import elw.dao.ctx.CtxSolution;
import elw.vo.FileType;
import elw.vo.IdNamed;
import org.codehaus.jackson.map.annotate.JsonSerialize;

/**
 * ReST representation of Solution information.
 */
public class RestSolution {
    private String id;
    public String getId() { return id; }

    private String name;
    public String getName() { return name; }

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

    private RestSlotInfo slotInfo = new RestSlotInfo();
    @JsonSerialize(include=JsonSerialize.Inclusion.NON_NULL)
    public RestSlotInfo getSlotInfo() { return slotInfo; }

    protected RestSolution() {
        //  nothing to do here
    }

    public static RestSolution create(
            final CtxSolution ctx,
            final boolean includeSlotInfo
    ) {
        if (ctx == null) {
            return null;
        }

        final RestSolution solution = new RestSolution();

        if (includeSlotInfo) {
            solution.slotInfo = RestSlotInfo.create(ctx);
        }

        solution.score = RestScore.create(ctx);
        solution.id = ctx.solution.getCouchId();
        solution.name = ctx.solution.getName();
        solution.comment = ctx.solution.getComment();
        solution.uploadMillis = ctx.solution.getStamp();
        solution.uploadNice = ctx.dateTimeNice(solution.uploadMillis);
        solution.sourceAddress = ctx.solution.getSourceAddress();

        final FileType fileType = IdNamed._.one(ctx.solution.getFileType());
        solution.fileTypeId = fileType.getId();
        solution.fileTypeName = fileType.getName();

        return solution;
    }

}
