package elw.vo;

public class Attachment extends FileBase {
    protected String[] extraPathElems = null;
    public static final String SCOPE = "v";

    @Override
    protected String[] pathElems() {
        if (extraPathElems == null || extraPathElems.length != 5) {
            throw new IllegalStateException(
                    "pathElems: " +
                            "courseId" + PATH_SEP + // 0
                            "tTypeId" + PATH_SEP + // 1
                            "taskId" + PATH_SEP + // 2
                            "verId" + PATH_SEP + // 3
                            "slotId" + PATH_SEP + // 4
                            "nameId" // 5
            );
        } else {
            return new String[] {
                    extraPathElems[0], extraPathElems[1], extraPathElems[2], extraPathElems[3],
                    extraPathElems[4], id
            };
        }
    }

    @Override
    public void setupPathElems(Ctx ctx, FileSlot slot) {
        extraPathElems = new String[] {
                ctx.getCourse().getId(),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slot.getId()
        };
    }
}
