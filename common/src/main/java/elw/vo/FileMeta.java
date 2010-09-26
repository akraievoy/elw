package elw.vo;

public class FileMeta extends IdNameStamped {
	protected String contentType;
	protected String nameNorm;
	protected String sourceAddress;
	protected String author;

	public FileMeta() {
	}

	public FileMeta(String name, String nameNorm, String contentType, String author, String sourceAddress) {
		this.name = name;
		this.nameNorm = nameNorm;
		this.author = author;
		this.contentType = contentType;
		this.sourceAddress = sourceAddress;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getNameNorm() {
		return nameNorm;
	}

	public void setNameNorm(String nameNorm) {
		this.nameNorm = nameNorm;
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