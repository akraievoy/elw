package elw.dao.ctx;

import elw.vo.Course;
import elw.vo.Enrollment;
import elw.vo.Group;
import elw.vo.Student;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Days;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.util.TimeZone;

/**
 * Parameter Object for general Enrollment Context.
 */
public class CtxEnrollment {
    public static final DateTimeFormatter FMT_DATE_TIME_NICE =
            DateTimeFormat.forPattern("EEE MMM dd HH:mm");

    public final Enrollment enr;
    public final Group group;
    public final Course course;

    public CtxEnrollment(
            final Enrollment enr, final Course course, final Group group
    ) {
        this.group = group;
        this.course = course;
        this.enr = enr;
    }
    
    public CtxStudent student(
            final Student student
    ) {
        final CtxStudent ctxStudent = new CtxStudent(
                enr, course, group,
                student
        );

        return propagateTZCache(ctxStudent);
    }

    public int days(long anchorMillis, long diffMillis) {
        final DateTime anchorStartOfDay = startOfDay(anchorMillis);
        final DateTime anchorEndOfDay = anchorStartOfDay.plusDays(1);
        final DateTime diffDate = dateTime(diffMillis);

        //  the diffed instant is earlier that start of anchor day
        if (anchorStartOfDay.isAfter(diffDate)) {
            //  -1 is due to returning number of full days within the interval
            return -1 - Days.daysBetween(
                    diffDate,
                    anchorStartOfDay
            ).getDays();
        }

        //  within start-end of anchor day return 0 days difference
        if (anchorEndOfDay.isAfter(diffDate)) {
            return 0;
        }

        //  number of full days in the interval, hence +1
        return 1 + Days.daysBetween(anchorEndOfDay, diffDate).getDays();
    }

    public DateTime dateTime(long diffMillis) {
        return new DateTime(diffMillis, dateTimeZone());
    }

    public static String dateTimeNice(final DateTime fromDateTime) {
        return FMT_DATE_TIME_NICE.print(fromDateTime);
    }
    
    public String dateTimeNice(final long millis) {
        return dateTimeNice(dateTime(millis));
    }

    public DateTime startOfDay(long anchorMillis) {
        final DateTime date = dateTime(anchorMillis);
        final DateTime anchorStartOfDay = new DateTime(
                date.getYear(),
                date.getMonthOfYear(),
                date.getDayOfMonth(),
                0, 0, 0, 0,
                dateTimeZone()
        );

        return anchorStartOfDay;
    }
    
    public DateTime endOfDay(long anchorMillis) {
        final DateTime anchorEndOfDay = startOfDay(anchorMillis).plusDays(1);

        return anchorEndOfDay;
    }

    protected <C extends CtxEnrollment> C propagateTZCache(C ctx) {
        ctx.timeZoneCache = timeZone();
        ctx.dateTimeZoneCache = dateTimeZone();

        return ctx;
    }
    
    private DateTimeZone dateTimeZoneCache;
    
    public synchronized DateTimeZone dateTimeZone() {
        if (dateTimeZoneCache != null) {
            return dateTimeZoneCache;
        }
        
        dateTimeZoneCache = DateTimeZone.forTimeZone(timeZone());

        return dateTimeZoneCache;
    }
    
    private TimeZone timeZoneCache;

    public synchronized TimeZone timeZone() {
        if (timeZoneCache != null) {
            return timeZoneCache;
        }

        final String enrTZ = enr.getTimeZone();

        final boolean tzEmpty =
                enrTZ == null || enrTZ.trim().length() == 0;

        timeZoneCache =
                tzEmpty ? TimeZone.getDefault() : TimeZone.getTimeZone(enrTZ);

        return timeZoneCache;
    }
}
