package elw.vo;

public class BundleScoring {
	protected double penaltyHold;
	protected double penaltyLate;
	protected int maxTeam;

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

	public int getMaxTeam() {
		return maxTeam;
	}

	public void setMaxTeam(int maxTeam) {
		this.maxTeam = maxTeam;
	}
}
