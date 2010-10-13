/*
 * ELW : e-learning workspace
 * Copyright (C) 2010  Anton Kraievoy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package elw.web;

import elw.miniweb.ViewJackson;
import org.joda.time.DateTime;
import org.joda.time.Period;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.PeriodFormatter;
import org.joda.time.format.PeriodFormatterBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.*;

/**
 * Inspired by @link{org.apache.velocity.tools.generic.FormatTool}, but has more specific and fine-tuned methods.
 */
public class FormatTool {
	private static final Logger log = LoggerFactory.getLogger(FormatTool.class);

	protected final static Map<Locale, FormatTool> cache = new HashMap<Locale, FormatTool>();

	protected static final long KB = 1024;
	protected static final long HALF_KB = 512;
	protected static final long MB = KB * KB;
	protected static final long HALF_MB = HALF_KB * KB;
	protected static final long MB_BOUNDARY = 10 * MB;
	protected static final long KB_BOUNDARY = 10 * KB;

	protected final Locale locale;
	protected final Map<String, DateTimeFormatter> dtfCache = new TreeMap<String, DateTimeFormatter>();
	protected final Map<String, PeriodFormatter> pfCache = new TreeMap<String, PeriodFormatter>();

	protected String pattern = "MMM d";
	protected String patternWeek = "EEE d HH:mm";
	protected String patternToday = "HH:mm";

	protected FormatTool(Locale locale) {
		this.locale = locale;
	}

	public static FormatTool forLocale(final Locale locale) {
		synchronized (cache) {
			final FormatTool cached = cache.get(locale);
			if (cached != null) {
				return cached;
			}

			final FormatTool created = new FormatTool(locale);
			cache.put(locale, created);

			return created;
		}
	}

	public void setPattern(String pattern) {
		this.pattern = pattern;
	}

	public void setPatternToday(String patternToday) {
		this.patternToday = patternToday;
	}

	public void setPatternWeek(String patternWeek) {
		this.patternWeek = patternWeek;
	}

	@SuppressWarnings({"UnusedDeclaration"})
	public String formatSize(long size) {
		if (size < 0) {
			return "";
		}

		if (size >= MB_BOUNDARY) {
			return Long.toString((size + HALF_MB) / MB) + " MB ";
		} else if (size >= KB_BOUNDARY) {
			return Long.toString((size + HALF_KB) / KB) + " KB ";
		} else {
			return Long.toString(size) + " bytes ";
		}
	}

	public String format(Date date) {
		if (date == null) {
			return "";
		}

		return format(date.getTime(), pattern, patternWeek, patternToday);
	}

	public String format(final long dateLong) {
		return format(dateLong, pattern, patternWeek, patternToday);
	}

	public String format(Date date, String pattern) {
		return format(date, pattern, pattern, pattern);
	}

	public String format(long dateLong, String pattern) {
		return format(dateLong, pattern, pattern, pattern);
	}

	public String format(Date date, String pattern, String patternWeek, String patternToday) {
		if (date == null) {
			return "";
		}

		return format(date.getTime(), pattern, patternWeek, patternToday);
	}

	public String format(final long dateLong, String pattern, String patternWeek, String patternToday) {
		final DateTimeFormatter formatter = lookupFormatter(pattern);
		final DateTimeFormatter formatterToday = lookupFormatter(patternToday);
		final DateTimeFormatter formatterWeek = lookupFormatter(patternWeek);

		final DateTime now = new DateTime(System.currentTimeMillis());
		final DateTime midnight = new DateTime(
				now.getYear(), now.getMonthOfYear(), now.getDayOfMonth(),
				0, 0, 0, 0
		);

		if (midnight.isBefore(dateLong) && midnight.plusDays(1).isAfter(dateLong)) {
			return formatterToday.print(dateLong);
		} else if (now.minusDays(7).isBefore(dateLong) && now.plusDays(7).isAfter(dateLong)) {
			return formatterWeek.print(dateLong);
		} else {
			return formatter.print(dateLong);
		}
	}

	protected DateTimeFormatter lookupFormatter(String pattern) {
		synchronized (dtfCache) {
			final DateTimeFormatter cached = dtfCache.get(pattern);
			if (cached != null) {
				return cached;
			}

			final DateTimeFormatter created = DateTimeFormat.forPattern(pattern).withLocale(locale);
			dtfCache.put(pattern, created);

			return created;
		}
	}

	protected PeriodFormatter lookupPeriodFormatter(String pattern) {
		synchronized (dtfCache) {
			final PeriodFormatter cached = pfCache.get(pattern);
			if (cached != null) {
				return cached;
			}

			final PeriodFormatter created = createDefaultPeriodFormatted(pattern).withLocale(locale);

			pfCache.put(pattern, created);

			return created;
		}
	}

	protected static PeriodFormatter createDefaultPeriodFormatted(String pattern) {
		String[] variants = {" ", ",", ",and ", ", and "};
		final PeriodFormatter formatter;
		if ("y".equals(pattern)) {
			formatter = new PeriodFormatterBuilder()
					.printZeroRarelyLast()
					.appendYears()
					.appendSuffix(" year", " years")
					.toFormatter();
		} else if ("M".equals(pattern)) {
			formatter = new PeriodFormatterBuilder()
					.printZeroNever()
					.appendYears()
					.appendSuffix(" year", " years")
					.printZeroRarelyLast()
					.appendMonths()
					.appendSuffix(" month", " months")
					.toFormatter();
		} else if ("d".equals(pattern)) {
			formatter = new PeriodFormatterBuilder()
					.printZeroNever()
					.appendYears()
					.appendSuffix(" year", " years")
					.appendSeparator(", ", " and ", variants)
					.appendMonths()
					.appendSuffix(" month", " months")
					.appendSeparator(", ", " and ", variants)
					.printZeroRarelyLast()
					.appendDays()
					.appendSuffix(" day", " days")
					.toFormatter();
		} else if ("H".equals(pattern)) {
			formatter = new PeriodFormatterBuilder()
					.printZeroNever()
					.appendYears()
					.appendSuffix(" year", " years")
					.appendSeparator(", ", " and ", variants)
					.appendMonths()
					.appendSuffix(" month", " months")
					.appendSeparator(", ", " and ", variants)
					.appendDays()
					.appendSuffix(" day", " days")
					.appendSeparator(", ", " and ", variants)
					.printZeroRarelyLast()
					.appendHours()
					.appendSuffix(" hour", " hours")
					.toFormatter();
		} else if ("m".equals(pattern)) {
			formatter = new PeriodFormatterBuilder()
					.printZeroNever()
					.appendYears()
					.appendSuffix(" year", " years")
					.appendSeparator(", ", " and ", variants)
					.appendMonths()
					.appendSuffix(" month", " months")
					.appendSeparator(", ", " and ", variants)
					.appendDays()
					.appendSuffix(" day", " days")
					.appendSeparator(", ", " and ", variants)
					.appendHours()
					.appendSuffix(" hour", " hours")
					.appendSeparator(", ", " and ", variants)
					.printZeroRarelyLast()
					.appendMinutes()
					.appendSuffix(" minute", " minutes")
					.toFormatter();
		} else if ("s".equals(pattern)) {
			formatter = new PeriodFormatterBuilder()
					.printZeroNever()
					.appendYears()
					.appendSuffix(" year", " years")
					.appendSeparator(", ", " and ", variants)
					.appendMonths()
					.appendSuffix(" month", " months")
					.appendSeparator(", ", " and ", variants)
					.appendDays()
					.appendSuffix(" day", " days")
					.appendSeparator(", ", " and ", variants)
					.appendHours()
					.appendSuffix(" hour", " hours")
					.appendSeparator(", ", " and ", variants)
					.appendMinutes()
					.appendSuffix(" minute", " minutes")
					.appendSeparator(", ", " and ", variants)
					.printZeroRarelyLast()
					.appendSeconds()
					.appendSuffix(" second", " seconds")
					.toFormatter();
		} else {
			throw new IllegalArgumentException("pattern: '" + pattern + "'");
		}

		return formatter;
	}

	@SuppressWarnings({"UnusedDeclaration"})
	public String formatAge(final Date time) {
		return formatAge(time.getTime());
	}

	public String formatAge(final long timeMillis) {
		return formatDuration(System.currentTimeMillis() - timeMillis);
	}

	public String formatDuration(final long timeMillis) {
		final Period period = new Period();
		final Period periodNorm = period.plusMillis((int) Math.abs(timeMillis)).normalizedStandard();

		if (periodNorm.getYears() > 0) {
			return lookupPeriodFormatter("y").print(periodNorm);
		} else if (periodNorm.getMonths() > 0) {
			return lookupPeriodFormatter("M").print(periodNorm);
		} else if (periodNorm.getDays() > 0) {
			return lookupPeriodFormatter("d").print(periodNorm);
		} else if (periodNorm.getHours() > 0) {
			return lookupPeriodFormatter("H").print(periodNorm);
		} else if (periodNorm.getMinutes() > 0) {
			return lookupPeriodFormatter("m").print(periodNorm);
		} else {
			//  LATER sometimes durations less than one second occur
			return lookupPeriodFormatter("s").print(periodNorm);
		}
	}

	public String json(final Object o) {
		try {
			return ViewJackson.MAPPER.writeValueAsString(o);
		} catch (IOException e) {
			if (o != null) {
				log.error("failed to render object of class: " + o.getClass().getSimpleName(), e);
			} else {
				log.error("failed to render null object", e);
			}

			return "";
		}
	}

	public String esc(final String str) {
		return str.replaceAll("&", "&amp;").replaceAll("\"", "&quot;").replaceAll(">", "&gt;").replaceAll("<", "&lt;");
	}

	public String lines(final String text) {
		final StringBuilder res = new StringBuilder(text == null ? "" : text);
		int lineNo = 1;
		int lineStart = 0;
		final char[] padding = {' ',' ',' ',' '};
		do {
			final String strLineNo = Integer.toString(lineNo);

			res.insert(lineStart, ' ');
			res.insert(lineStart, lineNo);
			final int lineNoLen = strLineNo.length();
			res.insert(lineStart, padding, 0, padding.length - lineNoLen);

			lineStart = Math.max(res.indexOf("\n", lineStart), res.indexOf("\r", lineStart+1+(Math.max(4, lineNoLen)))) + 1;
			lineNo+=1;
		} while (lineStart > 0);

		return res.toString();
	}

	
}
