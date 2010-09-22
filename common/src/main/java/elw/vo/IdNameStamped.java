package elw.vo;

public class IdNameStamped extends IdName implements Stamped {
	protected Stamp createStamp;
	protected String[] path;

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
