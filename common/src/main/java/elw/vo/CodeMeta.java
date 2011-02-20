package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class CodeMeta implements Stamped {
	private static final DateTimeFormatter FMT_DATETIME_NICE = DateTimeFormat.forPattern("EEE MMM dd HH:mm");
	private static final DateTimeFormatter FMT_DATETIME_BRIEF = DateTimeFormat.forPattern("MMM dd");

	private Stamp createStamp;
	private Stamp updateStamp;

	private String[] path;

	private int testsPassed;
	private int testsFailed;
	private long validatorStamp;
	private String sourceAddress;
	private String fileName;

	public Stamp getCreateStamp() {
		return createStamp;
	}

	public void setCreateStamp(Stamp createStamp) {
		this.createStamp = createStamp;
	}

	public void setUpdateStamp(Stamp updateStamp) {
		this.updateStamp = updateStamp;
	}

	public String[] getPath() {
		return path;
	}

	public void setPath(String[] path) {
		this.path = path;
	}

	public String getSourceAddress() {
		return sourceAddress;
	}

	public void setSourceAddress(String sourceAddress) {
		this.sourceAddress = sourceAddress;
	}

	public long getValidatorStamp() {
		return validatorStamp;
	}

	public void setValidatorStamp(long validatorStamp) {
		this.validatorStamp = validatorStamp;
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

	@JsonIgnore
	public String getFileName() {
		return fileName;
	}

	@JsonIgnore
	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	@JsonIgnore
	public double getPassRatio() {
		final double totalTests = 0.0 + testsPassed + testsFailed;

		if (totalTests == 0) {
			return 0.0;
		}

		return testsPassed / totalTests;
	}

	@JsonIgnore
	public String getNiceValidatorStamp() {
		return FMT_DATETIME_NICE.print(new DateTime(validatorStamp));
	}

	@JsonIgnore
	public String getNiceUploadStamp() {
		return FMT_DATETIME_NICE.print(new DateTime(updateStamp));
	}
	@JsonIgnore
	public String getBriefValidatorStamp() {
		return FMT_DATETIME_BRIEF.print(new DateTime(validatorStamp));
	}

	@JsonIgnore
	public String getBriefUploadStamp() {
		return FMT_DATETIME_BRIEF.print(new DateTime(updateStamp));
	}
}
