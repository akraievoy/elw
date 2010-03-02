package elw.vo;

public class TaskScoring {
	protected int scoreBudget;
	protected int classFrom;
	protected int classBefore;
	protected int classHold;
	protected boolean requireClean;

	public boolean isRequireClean() {
		return requireClean;
	}

	public void setRequireClean(boolean requireClean) {
		this.requireClean = requireClean;
	}

	public int getClassBefore() {
		return classBefore;
	}

	public void setClassBefore(int classBefore) {
		this.classBefore = classBefore;
	}

	public int getClassFrom() {
		return classFrom;
	}

	public void setClassFrom(int classFrom) {
		this.classFrom = classFrom;
	}

	public int getClassHold() {
		return classHold;
	}

	public void setClassHold(int classHold) {
		this.classHold = classHold;
	}

	public int getScoreBudget() {
		return scoreBudget;
	}

	public void setScoreBudget(int scoreBudget) {
		this.scoreBudget = scoreBudget;
	}
}
