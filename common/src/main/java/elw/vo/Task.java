package elw.vo;

public class Task {
	protected String id;
	protected String name;
	protected int scoreBudget;
	protected int penaltyHold;
	protected int penaltyLate;
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

	public int getPenaltyLate() {
		return penaltyLate;
	}

	public void setPenaltyLate(int penaltyLate) {
		this.penaltyLate = penaltyLate;
	}

	public int getPenaltyHold() {
		return penaltyHold;
	}

	public void setPenaltyHold(int penaltyHold) {
		this.penaltyHold = penaltyHold;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
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
