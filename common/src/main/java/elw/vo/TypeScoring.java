package elw.vo;

public class TypeScoring extends IdName {
	protected static final String[] NONE = new String[0];

	protected double weight = 1.0;
	protected String[] auto = NONE;
	protected String[] manual = NONE;
	protected String[] applied = NONE;

	public String[] getApplied() {
		return applied;
	}

	public void setApplied(String[] applied) {
		this.applied = applied;
	}

	public String[] getAuto() {
		return auto;
	}

	public void setAuto(String[] auto) {
		this.auto = auto;
	}

	public String[] getManual() {
		return manual;
	}

	public void setManual(String[] manual) {
		this.manual = manual;
	}

	public double getWeight() {
		return weight;
	}

	public void setWeight(double weight) {
		this.weight = weight;
	}
}
