package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class ReportMeta implements Stamped {
	private static final DateTimeFormatter FMT_DATETIME_NICE = DateTimeFormat.forPattern("EEE MMM dd HH:mm");
	private static final DateTimeFormatter FMT_DATETIME_BRIEF = DateTimeFormat.forPattern("MMM dd");
	private static final DateTimeFormatter FMT_DATETIME_FNAME = DateTimeFormat.forPattern("MMddHHmm");

	protected Stamp createStamp;
	protected Stamp updateStamp;
	protected String[] path;
	protected String sourceAddress;
	protected int totalUploads;
	protected String fileName;
	protected Stamp scoreStamp;

	public Stamp getCreateStamp() {
		return createStamp;
	}

	public void setCreateStamp(Stamp createStamp) {
		this.createStamp = createStamp;
	}

	public Stamp getUpdateStamp() {
		return updateStamp;
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

	@JsonIgnore
	public int getTotalUploads() {
		return totalUploads;
	}

	@JsonIgnore
	public void setTotalUploads(int totalUploads) {
		this.totalUploads = totalUploads;
	}

	@JsonIgnore
	public String getNiceUploadStamp() {
		return FMT_DATETIME_NICE.print(new DateTime(createStamp));
	}

	@JsonIgnore
	public String getBriefUploadStamp() {
		return FMT_DATETIME_BRIEF.print(new DateTime(createStamp));
	}

	public static String getFileNameUploadStamp(final long uploadStamp) {
		return FMT_DATETIME_FNAME.print(new DateTime(uploadStamp));
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileName() {
		return fileName;
	}

	public Stamp getScoreStamp() {
		return scoreStamp;
	}

	public void setScoreStamp(Stamp scoreStamp) {
		this.scoreStamp = scoreStamp;
	}
}