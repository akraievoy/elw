package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class ReportMeta {
	private static final DateTimeFormatter FMT_DATETIME_NICE = DateTimeFormat.forPattern("EEE MMM dd HH:mm");
	private static final DateTimeFormatter FMT_DATETIME_BRIEF = DateTimeFormat.forPattern("MMM dd");
	private static final DateTimeFormatter FMT_DATETIME_FNAME = DateTimeFormat.forPattern("MM_dd_HH_mm");

	protected long uploadStamp;
	protected String sourceAddress;
	protected int totalUploads;
	protected String fileName;

	public long getUploadStamp() {
		return uploadStamp;
	}

	public void setUploadStamp(long uploadStamp) {
		this.uploadStamp = uploadStamp;
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
		return FMT_DATETIME_NICE.print(new DateTime(uploadStamp));
	}

	@JsonIgnore
	public String getBriefUploadStamp() {
		return FMT_DATETIME_BRIEF.print(new DateTime(uploadStamp));
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
}