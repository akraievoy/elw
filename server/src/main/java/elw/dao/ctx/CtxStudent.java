package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Days;

import java.util.TimeZone;

/**
 *  Parameter Object, storing the full enrollment context.
 */
public class CtxStudent {
    //  LATER replace with some enrollment-scoped property
    public static final TimeZone TZ = TimeZone.getDefault();

    public final Enrollment enr;
    public final Group group;
    public final Student student;
    public final Course course;

    public CtxStudent(Enrollment enr, Course course, Student student, Group group) {
        this.enr = enr;
        this.course = course;
        this.student = student;
        this.group = group;
    }

    public static int days(
            TimeZone tz,
            long anchorMillis,
            long diffMillis
    ) {
        final DateTimeZone dateTimeZone = DateTimeZone.forTimeZone(tz);
        final DateTime anchorDate = new DateTime(anchorMillis, dateTimeZone);
        final DateTime anchorStartOfDay = new DateTime(
                anchorDate.getYear(),
                anchorDate.getMonthOfYear(),
                anchorDate.getDayOfMonth(),
                0, 0, 0, 0,
                dateTimeZone
        );
        final DateTime anchorEndOfDay = anchorStartOfDay.plusDays(1);

        final DateTime diffDate = new DateTime(diffMillis, dateTimeZone);

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

    public CtxTask task(final int idxPos) {
        final IndexEntry idxEntry = enr.getIndex().get(idxPos);
        final String[] path = idxEntry.getPath();
        final String taskTypeId = path[0];
        final String taskId = path[1];

        final TaskType taskType = course.getTaskTypes().get(taskTypeId);
        final Task task = taskType.getTasks().get(taskId);
        final Version ver = Nav.resolveVersion(
                task, idxEntry, group, student.getId()
        );

        final CtxTask ctxTask = new CtxTask(
                enr, course, group, student,
                idxPos, task, taskType, ver
        );

        return ctxTask;
    }
}
