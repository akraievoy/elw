package elw.vo;

import java.util.Map;

public class Criteria extends IdName {
	String ratio = "1.0";
	String powDef = "";
	String powMax = "";

	public String getPowDef() {
		return powDef;
	}

	public void setPowDef(String powDef) {
		this.powDef = powDef;
	}

	public String getPowMax() {
		return powMax;
	}

	public void setPowMax(String powMax) {
		this.powMax = powMax;
	}

	public String getRatio() {
		return ratio;
	}

	public void setRatio(String ratio) {
		this.ratio = ratio;
	}

	public int resolvePowDef(Map<String, Double> vars) {
		for (String var : vars.keySet()) {
			if (powDef.trim().equalsIgnoreCase(var)) {
				return vars.get(var).intValue();
			}
		}

		return Integer.parseInt(powDef);
	}

	public double resolveRatio(Map<String, Double> vars) {
		for (String var : vars.keySet()) {
			if (ratio.trim().equalsIgnoreCase(var)) {
				return vars.get(var);
			}
		}

		return Double.parseDouble(ratio);
	}
}
