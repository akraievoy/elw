package elw.vo;

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
}
