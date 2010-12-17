package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.joda.time.DateTime;
import org.joda.time.Days;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.util.regex.Pattern;

public class Class {
	private static final DateTimeFormatter FMT_DATE = DateTimeFormat.forPattern("yyyy-MM-dd");
	private static final DateTimeFormatter FMT_DATE_NICE = DateTimeFormat.forPattern("EEE MMM dd");
	private static final DateTimeFormatter FMT_TIME = DateTimeFormat.forPattern("HH:mm");

	protected String date;
	protected String fromTime;
	protected String toTime;
	protected Pattern[] onSitePatterns;
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

	public int computeToDiffStamp(Stamped stamped) {
		final DateTime time = stamped == null ? new DateTime() : new DateTime(stamped.getCreateStamp().getTime());
		return computeToDiff(time);
	}

	public int computeToDiff(DateTime time) {
		final DateTime toDateTime = getToDateTime();
		final DateTime toMidnight = new DateTime(
				toDateTime.getYear(), toDateTime.getMonthOfYear(), toDateTime.getDayOfMonth(),
				0, 0, 0, 0
		);
		if (toMidnight.isAfter(time)) {
			return -Days.daysBetween(time, toMidnight).getDays() - 1;
		} else if (toMidnight.plusDays(1).isAfter(time)) {
			return 0;
		} else {
			return Days.daysBetween(toMidnight, time).getDays();
		}
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

	public int computeDaysOverdue(final Stamp uploadStamp) {
		final DateTime stamp = uploadStamp == null ? new DateTime() : new DateTime(uploadStamp.getTime());
		return getDaysOverdue(stamp);
	}

	public int getDaysOverdue(DateTime uploadStamp) {
		final int overdue;
		if (isPassed(uploadStamp)) {
			overdue = getDayDiff(uploadStamp);
		} else {
			overdue = 0;
		}
		return overdue;
	}

	public boolean checkOnSite(String sourceAddress) {
		synchronized (this) {
			if (onSitePatterns == null) {
				onSitePatterns = new Pattern[onSite.length];
				for (int i = 0; i < onSite.length; i++) {
					onSitePatterns[i] = Pattern.compile(onSite[i]);
				}
			}
		}

		for (Pattern onSitePattern : onSitePatterns) {
			if (onSitePattern.matcher(sourceAddress).matches()) {
				return true;
			}
		}

		return false;
	}

	public boolean checkOnTime(Stamp createStamp) {
		final long instant = createStamp.getTime();
		final long min = getFromDateTime().getMillis();
		final long max = getToDateTime().getMillis();
		final int lateTolerance = 30 * 60 * 1000;

		return min <= instant && instant <= max + lateTolerance;
	}
}
