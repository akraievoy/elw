package elw.vo;

public class TaskScoring {
	protected int scoreBudget;
	protected int classFrom;
	protected int classCodeDue;
	protected int classReportDue;
	protected boolean requireClean;

	public boolean isRequireClean() {
		return requireClean;
	}

	public void setRequireClean(boolean requireClean) {
		this.requireClean = requireClean;
	}

	public int getClassCodeDue() {
		return classCodeDue;
	}

	public void setClassCodeDue(int classCodeDue) {
		this.classCodeDue = classCodeDue;
	}

	public int getClassFrom() {
		return classFrom;
	}

	public void setClassFrom(int classFrom) {
		this.classFrom = classFrom;
	}

	public int getClassReportDue() {
		return classReportDue;
	}

	public void setClassReportDue(int classReportDue) {
		this.classReportDue = classReportDue;
	}

	public int getScoreBudget() {
		return scoreBudget;
	}

	public void setScoreBudget(int scoreBudget) {
		this.scoreBudget = scoreBudget;
	}
}
