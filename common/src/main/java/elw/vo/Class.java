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
		return isStarted(new DateTime());
	}

	public boolean isStarted(final DateTime beforeTime) {
		final DateTime fromDateTime = getFromDateTime();
		return fromDateTime.isBefore(beforeTime);
	}

	@JsonIgnore
	public DateTime getFromDateTime() {
		final DateTime fromDate = FMT_DATE.parseDateTime(getDate());
		final DateTime fromTime = FMT_TIME.parseDateTime(getFromTime());
		final DateTime dateExact = new DateTime(
				fromDate.getYear(), fromDate.getMonthOfYear(), fromDate.getDayOfMonth(),
				fromTime.getHourOfDay(), fromTime.getMinuteOfHour(),
				0, 0
		);

		return dateExact;
	}

	@JsonIgnore
	public DateTime getToDateTime() {
		final DateTime fromDate = FMT_DATE.parseDateTime(getDate());
		final DateTime toTime = FMT_TIME.parseDateTime(getToTime());
		final DateTime dateExact = new DateTime(
				fromDate.getYear(), fromDate.getMonthOfYear(), fromDate.getDayOfMonth(),
				toTime.getHourOfDay(), toTime.getMinuteOfHour(),
				0, 0
		);

		return dateExact;
	}

	@JsonIgnore
	public boolean isPassed() {
		return isPassed(new DateTime());
	}

	public boolean isPassed(final DateTime beforeTime) {
		final DateTime toDateTime = getToDateTime();

		return toDateTime.isBefore(beforeTime);
	}

	@JsonIgnore
	public boolean isToday() {
		final DateTime fromDateTime = getFromDateTime();
		return fromDateTime.isBeforeNow() && fromDateTime.plusDays(1).isAfterNow();
	}

	public String getDate() {
		return date;
	}

	@JsonIgnore
	public String getNiceDate() {
		return FMT_DATE_NICE.print(getFromDateTime());
	}

	@JsonIgnore
	public int getDayDiff() {
		return getDayDiff(new DateTime());
	}

	public int getDayDiff(final DateTime toDate) {
		final DateTime date = getFromDateTime();
		final DateTime dateMidnight = new DateTime(
				date.getYear(), date.getMonthOfYear(), date.getDayOfMonth(),
				0, 0,
				0, 0
		);
		if (dateMidnight.isAfter(toDate)) {
			return Days.daysBetween(toDate, dateMidnight).getDays() + 1;
		} else if (dateMidnight.plusDays(1).isAfter(toDate)) {
			return 0;
		} else {
			return Days.daysBetween(dateMidnight, toDate).getDays();
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

	public double computeDaysOverdue(final Stamp uploadStamp) {
		return getDaysOverdue(new DateTime(uploadStamp.getTime()));
	}

	public double getDaysOverdue(DateTime uploadStamp) {
		final double overdue;
		if (isPassed(uploadStamp)) {
			overdue = getDayDiff(uploadStamp);
		} else {
			overdue = 20;
		}
		return overdue;
	}
}
