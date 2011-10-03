package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

public class Solution extends FileBase {
	//	second pass properties, issued by validator (if any)
	private int testsPassed;
    public static final String SCOPE = "s";

    public int getTestsPassed() { return testsPassed; }
    public void setTestsPassed(int testsPassed) { this.testsPassed = testsPassed; }

	private int testsFailed;
    public int getTestsFailed() { return testsFailed; }
    public void setTestsFailed(int testsFailed) { this.testsFailed = testsFailed; }

	private long validatorStamp;
    public long getValidatorStamp() { return validatorStamp; }
    public void setValidatorStamp(long validatorStamp) { this.validatorStamp = validatorStamp; }

	//	injected via Queries
	private Score score;
    @JsonIgnore
    public Score getScore() { return score; }
    @JsonIgnore
    public void setScore(Score score) { this.score = score; }

    private String[] extraPathElems;

    @Override
    protected String[] pathElems() {
        if (extraPathElems == null || extraPathElems.length != 8) {
            throw new IllegalStateException(
                    "pathElems: " +
                            "groupId" + PATH_SEP + // 0
                            "studId" + PATH_SEP + // 1
                            "courseId" + PATH_SEP + // 2
                            "ctxIdx" + PATH_SEP + // 3
                            "tTypeId" + PATH_SEP + // 4
                            "taskId" + PATH_SEP + // 5
                            "verId" + PATH_SEP + // 6
                            "slotId" + PATH_SEP + // 7
                            "nameId"
            );
        } else {
            return new String[] {
                    extraPathElems[0], extraPathElems[1], extraPathElems[2], extraPathElems[3],
                    extraPathElems[4], extraPathElems[5], extraPathElems[6], extraPathElems[7],
                    id
            };
        }
    }

    @Override
    public void setupPathElems(Ctx ctx, FileSlot slot) {
        extraPathElems = new String[] {
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                String.valueOf(ctx.getIndex()),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slot.getId()
        };
    }

	@JsonIgnore
	public boolean isValidated() {
		return validatorStamp > 0;
	}

	@JsonIgnore
	public int getTotalTests() {
		return testsPassed + testsFailed;
	}

	@JsonIgnore
	public double getPassRatio() {
		final double totalTests = 0.0 + getTotalTests();

		if (totalTests == 0) {
			return 0.0;
		}

		return testsPassed / totalTests;
	}
}
