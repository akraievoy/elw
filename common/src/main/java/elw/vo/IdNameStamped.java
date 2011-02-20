package elw.vo;

public class IdNameStamped extends IdName implements Stamped {
	private Stamp createStamp;
	private String[] path;

	public Stamp getCreateStamp() {
		return createStamp;
	}

	public void setCreateStamp(Stamp createStamp) {
		this.createStamp = createStamp;
	}

	public String[] getPath() {
		return path;
	}

	public void setPath(String[] path) {
		this.path = path;
	}
}
