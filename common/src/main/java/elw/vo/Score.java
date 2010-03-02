package elw.vo;

public class Score {
	public static enum Grade {
		NO_ERRORS_DETECTED, MINOR_STYLE_ISSUES, RESTYLING_REQUIRED, ESSENTIAL_ERROR, HEAVY_REUSE, FORMAT_ERROR, GRADE_PENDING
	}

	protected String enrollmentId;
	protected String[] studentIds;
	protected int classIndex;
	protected Grade grade;
	protected int score;
	protected long gradeStamp;
	protected long scoreStamp;

	public int getClassIndex() {
		return classIndex;
	}

	public void setClassIndex(int classIndex) {
		this.classIndex = classIndex;
	}

	public String[] getStudentIds() {
		return studentIds;
	}

	public void setStudentIds(String[] studentIds) {
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

	public long getGradeStamp() {
		return gradeStamp;
	}

	public void setGradeStamp(long gradeStamp) {
		this.gradeStamp = gradeStamp;
	}

	public Grade getGrade() {
		return grade;
	}

	public void setGrade(Grade grade) {
		this.grade = grade;
	}

	public String getEnrollmentId() {
		return enrollmentId;
	}

	public void setEnrollmentId(String enrollmentId) {
		this.enrollmentId = enrollmentId;
	}
}
