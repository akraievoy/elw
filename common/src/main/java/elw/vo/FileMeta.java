package elw.vo;

public class FileMeta extends IdNameStamped {
	protected long length;
	protected String contentType;
	protected String fileSetId;
	protected String sourceAddress;
	protected String author;

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

	public String getSourceAddress() {
		return sourceAddress;
	}

	public void setSourceAddress(String sourceAddress) {
		this.sourceAddress = sourceAddress;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}
}
