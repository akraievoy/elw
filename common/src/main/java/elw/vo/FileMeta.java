package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.annotate.JsonIgnoreProperties;

@JsonIgnoreProperties("nameNorm")
public class FileMeta extends IdNameStamped {
	private String contentType;
	private String comment;
	private String sourceAddress;
	private String author;
	private boolean binary;

	//	second pass properties, issued by validator (if any)
	private int testsPassed;
	private int testsFailed;
	private long validatorStamp;

	//	transient
	private Score score;

	public FileMeta() {
	}

	public FileMeta(String name, String contentType, String author, String sourceAddress) {
		this.name = name;
		this.author = author;
		this.contentType = contentType;
		this.sourceAddress = sourceAddress;
	}

	@Override
	public String getName() {
		return extractNameFromPath(super.getName());
	}

	public boolean isBinary() {
		return binary;
	}

	public void setBinary(boolean binary) {
		this.binary = binary;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
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
		if (name == null) {
			return null;
		}

		final int lastSlash = Math.max(name.lastIndexOf("\\"), name.lastIndexOf("/"));
		final String fName = lastSlash >= 0 ? name.substring(lastSlash + 1) : name;

		return fName;
	}
}
