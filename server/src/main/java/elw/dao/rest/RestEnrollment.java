package elw.dao.rest;

import elw.dao.ctx.CtxEnrollment;
import elw.dao.ctx.CtxTask;
import elw.vo.*;
import elw.vo.Class;

import java.util.*;

/**
 * Enrollment index with additional computed properties.
 */
public class RestEnrollment {
    private SortedMap<String, RestIndexEntry> index =
            new TreeMap<String, RestIndexEntry>();
    public SortedMap<String, RestIndexEntry> getIndex() {
        return index;
    }

    private SortedMap<String, RestClass> classes =
            new TreeMap<String, RestClass>();
    public SortedMap<String, RestClass> getClasses() { return classes; }

    private RestClass currentClass;
    public RestClass getCurrentClass() { return currentClass; }

    private String groupId;
    public String getGroupId() { return groupId; }

    private String courseId;
    public String getCourseId() { return courseId; }

    private String id;
    public String getId() { return id; }

    private String name;
    public String getName() { return name; }

    private String timeZone;
    public String getTimeZone() { return timeZone; }

    public static RestEnrollment create(
            final CtxEnrollment ctxEnr,
            final String sourceAddress
    ) {
        final RestEnrollment restEnr = new RestEnrollment();

        restEnr.groupId = ctxEnr.group.getId();
        restEnr.courseId = ctxEnr.course.getId();
        restEnr.id = ctxEnr.enr.getId();
        restEnr.name = ctxEnr.enr.getName();
        restEnr.timeZone = ctxEnr.timeZone().getDisplayName();

        final SortedMap<String, Class> clazzMap = ctxEnr.enr.getClasses();
        for (elw.vo.Class clazz : clazzMap.values()) {
            final RestClass restClass =
                    RestClass.create(ctxEnr, clazz, sourceAddress);
            restEnr.classes.put(clazz.id(), restClass);

            if (restClass.isCurrent()) {
                restEnr.currentClass = restClass;
            }
        }

        final List<IndexEntry> enrIndex = ctxEnr.enr.getIndex();
        for (int i = 0, idxSize = enrIndex.size(); i < idxSize; i++) {
            final IndexEntry idxEntry = enrIndex.get(i);
            final String idxEntryId = String.valueOf(i);

            final RestIndexEntry restEntry = new RestIndexEntry();

            restEntry.id = idxEntryId;

            final String classFromKey =
                    CtxTask.classForKey(
                            ctxEnr.enr.getClasses(),
                            idxEntry.getClassFrom()
                    ).getId();
            restEntry.classFrom =
                    restEnr.classes.get(classFromKey);

            for (final Map.Entry<String, String> dueEntry :
                    idxEntry.getClassDue().entrySet()) {
                final String classDueKey =
                        CtxTask.classForKey(
                                ctxEnr.enr.getClasses(),
                                dueEntry.getValue()
                        ).getId();
                restEntry.classDue.put(
                        dueEntry.getKey(),
                        restEnr.classes.get(classDueKey)
                );
            }

            restEntry.scoreBudget = idxEntry.getScoreBudget();
            restEntry.verStep = idxEntry.getVerStep();
            restEntry.verAnchor = idxEntry.getVerAnchor();

            final String taskTypeId = idxEntry.getPath()[0];
            final String taskId = idxEntry.getPath()[1];
            try {
                restEntry.taskType =
                        ctxEnr.course.getTaskTypes().get(taskTypeId).clone();
                restEntry.task =
                        restEntry.taskType.getTasks().get(taskId).clone();

                //  we should not expose all tasks of the type via ReST
                //  the same applies to versions,
                //      LATER should be done by QueriesSecure
                restEntry.taskType.setTasks(
                        new TreeMap<String, Task>()
                );
            } catch (CloneNotSupportedException e) {
                throw new IllegalStateException(e);
            }

            restEnr.index.put(idxEntryId, restEntry);
        }

        return restEnr;
    }

    /**
     * Extended computed properties of {@link elw.vo.Class}.
     */
    public static class RestClass {
        private String id;
        public String getId() { return id; }

        private String name;
        public String getName() { return name; }

        private long fromMillis;
        public long getFromMillis() { return fromMillis; }

        private long toMillis;
        public long getToMillis() { return toMillis; }

        private String fromNice;
        public String getFromNice() { return fromNice; }

        private String toNice;
        public String getToNice() { return toNice; }

        private int days;
        public int getDays() { return days; }

        private boolean current;
        public boolean isCurrent() { return current; }

        private boolean started;
        public boolean isStarted() { return started; }

        private boolean completed;
        public boolean isCompleted() { return completed; }

        private boolean onsite;
        public boolean isOnsite() { return onsite; }

        public static RestClass create(
                CtxEnrollment ctxEnr, final Class clazz,
                final String sourceAddress
        ) {
            final RestClass restClass = new RestClass();

            restClass.id = clazz.id();
            // LATER no such field in the storage for now
            restClass.name = clazz.id();
            restClass.fromMillis = clazz.getFromDateTime().getMillis();
            restClass.toMillis = clazz.getToDateTime().getMillis();
            restClass.fromNice = ctxEnr.dateTimeNice(restClass.fromMillis);
            restClass.toNice = ctxEnr.dateTimeNice(restClass.toMillis);
            final long now = System.currentTimeMillis();
            restClass.days = ctxEnr.days(
                    now,
                    restClass.fromMillis
            );
            restClass.started =
                    restClass.fromMillis <= now;
            restClass.completed =
                    restClass.toMillis <= now;
            restClass.current =
                    restClass.started && !restClass.completed;
            restClass.onsite =
                    clazz.checkOnSite(sourceAddress);

            return restClass;
        }
    }

    /**
     * Extended computed properties of {@link elw.vo.IndexEntry}.
     */
    public static class RestIndexEntry {
        private String id;
        public String getId() { return id; }

        private TaskType taskType;
        public TaskType getTaskType() { return taskType; }

        private Task task;
        public Task getTask() { return task; }

        private RestClass classFrom;
        public RestClass getClassFrom() { return classFrom; }

        private Map<String, RestClass> classDue =
                new TreeMap<String, RestClass>();
        public Map<String, RestClass> getClassDue() { return classDue; }

        private int scoreBudget;
        public int getScoreBudget() { return scoreBudget; }

        private int verAnchor;
        public int getVerAnchor() { return verAnchor; }
        public void setVerAnchor(int verAnchor) { this.verAnchor = verAnchor; }

        private int verStep;
        public int getVerStep() { return verStep; }
        public void setVerStep(int verStep) { this.verStep = verStep; }
    }
}
