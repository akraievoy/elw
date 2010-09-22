package elw.vo;

public class File extends IdNameStamped {
	protected long length;
	protected String contentType;
	protected String fileSetId;

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getFileSetId() {
		return fileSetId;
	}

	public void setFileSetId(String fileSetId) {
		this.fileSetId = fileSetId;
	}

	public long getLength() {
		return length;
	}

	public void setLength(long length) {
		this.length = length;
	}
}
