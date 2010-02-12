package elw.vo;

public class Scoring {
	protected int scoreBudget;
	protected double penaltyHold;
	protected double penaltyLate;
	protected int classFrom;
	protected int classBefore;
	protected int classHold;
	protected boolean requireClean;
	protected int maxTeam;

	public int getClassBefore() {
		return classBefore;
	}

	public void setClassBefore(int classBefore) {
		this.classBefore = classBefore;
	}

	public int getClassHold() {
		return classHold;
	}

	public void setClassHold(int classHold) {
		this.classHold = classHold;
	}

	public int getClassFrom() {
		return classFrom;
	}

	public void setClassFrom(int classFrom) {
		this.classFrom = classFrom;
	}

	public int getScoreBudget() {
		return scoreBudget;
	}

	public void setScoreBudget(int scoreBudget) {
		this.scoreBudget = scoreBudget;
	}

	public double getPenaltyLate() {
		return penaltyLate;
	}

	public void setPenaltyLate(double penaltyLate) {
		this.penaltyLate = penaltyLate;
	}

	public double getPenaltyHold() {
		return penaltyHold;
	}

	public void setPenaltyHold(double penaltyHold) {
		this.penaltyHold = penaltyHold;
	}

	public boolean isRequireClean() {
		return requireClean;
	}

	public void setRequireClean(boolean requireClean) {
		this.requireClean = requireClean;
	}

	public int getMaxTeam() {
		return maxTeam;
	}

	public void setMaxTeam(int maxTeam) {
		this.maxTeam = maxTeam;
	}
}
