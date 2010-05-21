package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.joda.time.DateTime;
import org.joda.time.Days;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class Class {
	private static final DateTimeFormatter FMT_DATE = DateTimeFormat.forPattern("yyyy-MM-dd");
	private static final DateTimeFormatter FMT_DATE_NICE = DateTimeFormat.forPattern("EEE MMM dd");
	private static final DateTimeFormatter FMT_TIME = DateTimeFormat.forPattern("HH:mm");

	protected String date;
	protected String fromTime;
	protected String toTime;
	protected String[] onSite;

	@JsonIgnore
	public boolean isCurrent() {
		return isStarted() && !isPassed();
	}

	@JsonIgnore
	public boolean isStarted() {
		final DateTime date = FMT_DATE.parseDateTime(getDate());
		final DateTime from = FMT_TIME.parseDateTime(getFromTime());
		final DateTime fromDate = new DateTime(
				date.getYear(), date.getMonthOfYear(), date.getDayOfMonth(),
				from.getHourOfDay(), from.getMinuteOfHour(),
				0, 0
		);
		return fromDate.isBeforeNow();
	}

	@JsonIgnore
	public boolean isPassed() {
		final DateTime date = FMT_DATE.parseDateTime(getDate());
		final DateTime to = FMT_TIME.parseDateTime(getToTime());
		final DateTime toDate = new DateTime(
				date.getYear(), date.getMonthOfYear(), date.getDayOfMonth(),
				to.getHourOfDay(), to.getMinuteOfHour(),
				0, 0
		);

		return toDate.isBeforeNow();
	}

	@JsonIgnore
	public boolean isToday() {
		final DateTime date = FMT_DATE.parseDateTime(getDate());
		final DateTime dateMidnight = new DateTime(
				date.getYear(), date.getMonthOfYear(), date.getDayOfMonth(),
				0, 0,
				0, 0
		);
		return dateMidnight.isBeforeNow() && dateMidnight.plusDays(1).isAfterNow();
	}

	public String getDate() {
		return date;
	}

	@JsonIgnore
	public String getNiceDate() {
		return FMT_DATE_NICE.print(FMT_DATE.parseDateTime(getDate()));
	}

	public int getDayDiff() {
		final DateTime date = FMT_DATE.parseDateTime(getDate());
		final DateTime dateMidnight = new DateTime(
				date.getYear(), date.getMonthOfYear(), date.getDayOfMonth(),
				0, 0,
				0, 0
		);
		if (dateMidnight.isAfterNow()) {
			return Days.daysBetween(new DateTime(), dateMidnight).getDays() + 1;
		} else if (dateMidnight.plusDays(1).isAfterNow()) {
			return 0;
		} else {
			return Days.daysBetween(dateMidnight, new DateTime()).getDays();
		}
	}

	public void setDate(String date) {
		this.date = date;
	}

	public String getFromTime() {
		return fromTime;
	}

	public void setFromTime(String fromTime) {
		this.fromTime = fromTime;
	}

	public String[] getOnSite() {
		return onSite;
	}

	public void setOnSite(String[] ipMasks) {
		this.onSite = ipMasks;
	}

	public String getToTime() {
		return toTime;
	}

	public void setToTime(String toTime) {
		this.toTime = toTime;
	}
}
