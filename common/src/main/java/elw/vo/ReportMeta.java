package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class ReportMeta {
	private static final DateTimeFormatter FMT_DATETIME_NICE = DateTimeFormat.forPattern("EEE MMM dd HH:mm");

	protected long uploadStamp;
	protected String sourceAddress;
	protected int totalUploads;

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
}