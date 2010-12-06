package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.Map;

public class Criteria extends IdName {
	String ratio = "1.0";
	String powDef = "";
	int powMax = 1;
	boolean auto = false;

	public String getPowDef() {
		return powDef;
	}

	public void setPowDef(String powDef) {
		this.powDef = powDef;
	}

	public int getPowMax() {
		return powMax;
	}

	public void setPowMax(int powMax) {
		this.powMax = powMax;
	}

	public String getRatio() {
		return ratio;
	}

	public void setRatio(String ratio) {
		this.ratio = ratio;
	}

	public boolean isAuto() {
		return auto;
	}

	public void setAuto(boolean auto) {
		this.auto = auto;
	}

	public Integer resolvePowDef(Map<String, Double> vars) {
		if (powDef.startsWith("$")) {
			if (vars != null) {
				for (String var : vars.keySet()) {
					if (powDef.trim().equalsIgnoreCase(var)) {
						return vars.get(var).intValue();
					}
				}
			}

			return null;
		}

		return Integer.parseInt(powDef);
	}

	public Double resolveRatio(Map<String, Double> vars) {
		if (ratio.startsWith("$")) {
			if (vars != null) {
				for (String var : vars.keySet()) {
					if (ratio.trim().equalsIgnoreCase(var)) {
						return vars.get(var);
					}
				}
			}

			return null;
		}

		return Double.parseDouble(ratio);
	}

	@JsonIgnore
	public int[] getPows() {
		final int[] pows = new int[powMax + 1];

		for (int i = 0; i < pows.length; i++) {
			pows[i] = i;
		}

		return pows;
	}
}
