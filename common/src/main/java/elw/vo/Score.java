package elw.vo;

public class Score {
	public static enum Grade {
		NO_ERRORS_DETECTED, MINOR_STYLE_ISSUES, RESTYLING_REQUIRED, ESSENTIAL_ERROR, HEAVY_REUSE, FORMAT_ERROR, GRADE_PENDING
	}

	protected int[] studentIds;
	protected int classIndex;
	protected Grade grade;
	protected int score;
	protected long codeSubmissionStamp;
	protected long reportSubmissionStamp;
	protected long scoreStamp;

	public int getClassIndex() {
		return classIndex;
	}

	public void setClassIndex(int classIndex) {
		this.classIndex = classIndex;
	}

	public int[] getStudentIds() {
		return studentIds;
	}

	public void setStudentIds(int[] studentIds) {
		this.studentIds = studentIds;
	}

	public int getScore() {
		return score;
	}

	public void setScore(int score) {
		this.score = score;
	}

	public long getScoreStamp() {
		return scoreStamp;
	}

	public void setScoreStamp(long scoreStamp) {
		this.scoreStamp = scoreStamp;
	}

	public long getCodeSubmissionStamp() {
		return codeSubmissionStamp;
	}

	public void setCodeSubmissionStamp(long codeSubmissionStamp) {
		this.codeSubmissionStamp = codeSubmissionStamp;
	}

	public long getReportSubmissionStamp() {
		return reportSubmissionStamp;
	}

	public void setReportSubmissionStamp(long reportSubmissionStamp) {
		this.reportSubmissionStamp = reportSubmissionStamp;
	}

	public Grade getGrade() {
		return grade;
	}

	public void setGrade(Grade grade) {
		this.grade = grade;
	}
}
