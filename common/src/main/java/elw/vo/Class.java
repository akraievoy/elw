package elw.vo;

public class Class {
	protected String date;
	protected String  fromTime;
	protected String toTime;
	protected String[] ipMask;

	public String getDate() {
		return date;
	}

	public void setDate(String date) {
		this.date = date;
	}

	public String getFromTime() {
		return fromTime;
	}

	public void setFromTime(String fromTime) {
		this.fromTime = fromTime;
	}

	public String[] getIpMask() {
		return ipMask;
	}

	public void setIpMask(String[] ipMasks) {
		this.ipMask = ipMasks;
	}

	public String getToTime() {
		return toTime;
	}

	public void setToTime(String toTime) {
		this.toTime = toTime;
	}
}
