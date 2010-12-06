	package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

public class FileMeta extends IdNameStamped {
	protected String contentType;
	protected String nameNorm;	//	TODO wipe FileMeta.nameNorm as it`s now computed on the fly
	protected String comment;
	protected String sourceAddress;
	protected String author;

	//	second pass properties, issued by validator (if any)
	protected int testsPassed;
	protected int testsFailed;
	protected long validatorStamp;

	//	transient
	protected Score score;

	public FileMeta() {
	}

	public FileMeta(String name, String nameNorm, String contentType, String author, String sourceAddress) {
		this.name = name;
		this.nameNorm = nameNorm;
		this.author = author;
		this.contentType = contentType;
		this.sourceAddress = sourceAddress;
	}

	@Override
	public String getName() {
		return extractNameFromPath(super.getName());
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

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public int getTestsFailed() {
		return testsFailed;
	}

	public void setTestsFailed(int testsFailed) {
		this.testsFailed = testsFailed;
	}

	public int getTestsPassed() {
		return testsPassed;
	}

	public void setTestsPassed(int testsPassed) {
		this.testsPassed = testsPassed;
	}

	public long getValidatorStamp() {
		return validatorStamp;
	}

	public void setValidatorStamp(long validatorStamp) {
		this.validatorStamp = validatorStamp;
	}

	@JsonIgnore
	public boolean isValidated() {
		return validatorStamp > 0;
	}

	@JsonIgnore
	public int getTotalTests() {
		return testsPassed + testsFailed;
	}

	@JsonIgnore
	public double getPassRatio() {
		final double totalTests = 0.0 + getTotalTests();

		if (totalTests == 0) {
			return 0.0;
		}

		return testsPassed / totalTests;
	}

	@JsonIgnore
	public Score getScore() {
		return score;
	}

	@JsonIgnore
	public void setScore(Score score) {
		this.score = score;
	}

	public static String extractNameFromPath(final String name) {
		final int lastSlash = Math.max(name.lastIndexOf("\\"), name.lastIndexOf("/"));
		final String fName = lastSlash >= 0 ? name.substring(lastSlash + 1) : name;

		return fName;
	}
}
