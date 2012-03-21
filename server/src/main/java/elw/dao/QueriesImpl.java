package elw.dao;

import base.pattern.Result;
import com.google.common.io.ByteStreams;
import com.google.common.io.InputSupplier;
import elw.dao.ctx.*;
import elw.dao.rest.*;
import elw.vo.*;
import elw.vo.Class;
import elw.vo.Score;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import org.akraievoy.couch.CouchDao;
import org.akraievoy.couch.Squab;

import static elw.vo.IdNamed._.*;
import static elw.vo.IdNamed._.resolve;

/**
 * Default implementation of data queries.
 */
public class QueriesImpl implements Queries {
    private static final Logger log = LoggerFactory.getLogger(QueriesImpl.class);

    private final Object courseResolutionMonitor = new Object();

    private CouchDao metaDao;
    public void setMetaDao(CouchDao metaDao) {
        this.metaDao = metaDao;
    }

    private CouchDao userDao;
    public void setUserDao(CouchDao userDao) {
        this.userDao = userDao;
    }

    private CouchDao attachmentDao;
    public void setAttachmentDao(CouchDao attachmentDao) {
        this.attachmentDao = attachmentDao;
    }

    private CouchDao solutionDao;
    public void setSolutionDao(CouchDao solutionDao) {
        this.solutionDao = solutionDao;
    }

    //  LATER proceed with auth
    @SuppressWarnings("FieldCanBeLocal")
    private CouchDao authDao;
    public void setAuthDao(CouchDao authDao) {
        this.authDao = authDao;
    }

    public QueriesImpl() {
    }
    
    public QueriesSecure secure(final QueriesSecure.Auth auth) {
        return new QueriesSecure(this, auth);
    }

    public RestEnrollmentSummary restScores(
            final String enrId,
            final Collection<String> studentIds
    ) {
        final Enrollment enr = enrollmentSome(enrId);

        if (enr == null) {
            return null;
        }

        final Group group = group(enr.getGroupId());

        final Collection<String> studIds =
                new ArrayList<String>(group.getStudents().keySet());
        if (studentIds != null) {
            studIds.retainAll(studentIds);
        }

        final Course course = course(enr.getCourseId());
        final RestEnrollmentSummary enrSummary = new RestEnrollmentSummary();
        final CtxEnrollment ctxEnr = new CtxEnrollment(enr, course, group);
        for (CtxStudent ctxStudent : ctxEnr.students) {
            final RestStudentSummary studentSummary =
                    new RestStudentSummary();

            for (CtxTask ctxTask : ctxStudent.tasks) {
                final RestTaskSummary taskSummary =
                        new RestTaskSummary();

                for (CtxSlot ctxSlot : ctxTask.slots) {
                    if (ctxSlot.slot.getScoreWeight() <= 0) {
                        continue;
                    }

                    final List<Solution> solutions =
                            solutions(ctxSlot);
                    final RestSlotSummary slotSummary =
                            RestSlotSummary.create(ctxSlot, solutions);

                    taskSummary.register(ctxSlot, slotSummary);
                }

                studentSummary.register(ctxTask, taskSummary);
            }

            enrSummary.register(ctxStudent, studentSummary);
        }

        return enrSummary;
    }

    //  LATER migrate all enrollment queries to this method
    public RestEnrollment restEnrollment(
            final String enrId,
            final String sourceAddress
    ) {
        final Enrollment enrRaw =
                userDao.findSome(Enrollment.class, null, null, enrId);

        if (enrRaw == null) {
            return null;
        }

        final Enrollment enr = markEnrollment(enrRaw);

        final CtxEnrollment ctxEnrollment = new CtxEnrollment(
                enr,
                course(enr.getCourseId()),
                group(enr.getGroupId())
        );

        final RestEnrollment restEnr =
                RestEnrollment.create(
                        ctxEnrollment,
                        sourceAddress
                );

        return restEnr;
    }

    public Map<String, RestSolution> restSolutions(
            final String enrId,
            final SolutionFilter filter
    ) {
        final Enrollment enrRaw = enrollmentSome(enrId);
        if (enrRaw == null) {
            return null;
        }
        final Enrollment enr = markEnrollment(enrRaw);

        final Group group = group(enr.getGroupId());
        final Course course = course(enr.getCourseId());
        final SortedMap<String, RestSolution> solutionsRes =
                new TreeMap<String, RestSolution>();

        final CtxEnrollment ctxEnr = new CtxEnrollment(enr, course, group);
        for (CtxStudent ctxStudent : ctxEnr.students) {
            if (!filter.preAllows(ctxStudent)) {
                continue;
            }

            for (CtxTask ctxTask : ctxStudent.tasks) {
                for (CtxSlot ctxSlot : ctxTask.slots) {
                    final List<Solution> solutions =
                            solutions(ctxSlot);

                    for (Solution solution : solutions) {
                        final CtxSolution ctxSolution = 
                                ctxSlot.solution(solution);
                        
                        if (filter.allows(ctxSolution)) {
                            solutionsRes.put(
                                    solution.getCouchId(),
                                    RestSolution.create(ctxSolution, true)
                            );
                        }
                    }

                }
            }
        }

        return solutionsRes;
    }

    public CtxResolutionState resolveSlot(
            final String enrollmentId,
            final String couchId,
            final StudentFilter studentFilter
    ) {
        final Enrollment enrRaw = enrollmentSome(enrollmentId);
        if (enrRaw == null) {
            return CtxResolutionState.failed(
                    couchId, "enrollment "+enrollmentId+" not found"
            );
        }
        final Enrollment enr = markEnrollment(enrRaw);

        final Squab.Path path;
        try {
            path = Squab.Path.fromId(couchId);
        } catch (IllegalStateException ise) {
            return CtxResolutionState.failed(
                    couchId, "not valid"
            );
        }

        final String prefix = Solution.class.getSimpleName();
        if (path.len() != 11 || !prefix.equals(path.elem(0))) {
            return CtxResolutionState.failed(
                    couchId, "has extra/missing entries"
            );
        }

        if (!enr.getGroupId().equals(path.elem(1))) {
            return CtxResolutionState.failed(
                    couchId, path, "refers to incorrect group"
            );
        }

        final Group group = group(enr.getGroupId());
        if (group == null) {
            return CtxResolutionState.failed(
                    couchId, path, "group not found"
            );
        }

        if (!enr.getCourseId().equals(path.elem(3))) {
            return CtxResolutionState.failed(
                    couchId, path, "refers to incorrect course"
            );
        }

        final Course course = course(enr.getCourseId());
        if (course == null) {
            return CtxResolutionState.failed(
                    couchId, path, "course not found"
            );
        }

        final CtxEnrollment ctxEnrollment =
                new CtxEnrollment(enr, course, group);

        final Student student = group.getStudents().get(path.elem(2));
        if (student == null) {
            return CtxResolutionState.failed(
                    couchId, path, "student not found"
            );
        }

        if (studentFilter != null && !studentFilter.allows(student)) {
            return CtxResolutionState.failed(
                    couchId, path, "student access denied"
            );
        }

        final CtxStudent ctxStudent =
                ctxEnrollment.student(student);

        final String indexKey = path.elem(4);
        final CtxTask ctxTask = ctxStudent.task(indexKey);

        if (ctxTask.tType == null) {
            return CtxResolutionState.failed(
                    couchId, path, "taskType not found"
            );
        }

        if (!ctxTask.tType.getId().equals(path.elem(5))) {
            return CtxResolutionState.failed(
                    couchId, path, "taskType id mismatch"
            );
        }

        if (ctxTask.task == null) {
            return CtxResolutionState.failed(
                    couchId, path, "task not found"
            );
        }

        if (!ctxTask.task.getId().equals(path.elem(6))) {
            return CtxResolutionState.failed(
                    couchId, path, "task id mismatch"
            );
        }

        if (ctxTask.ver == null) {
            return CtxResolutionState.failed(
                    couchId, path, "version not found"
            );
        }

        final CtxTask ctxTaskOverride;
        final String versionElem = path.elem(7);
        if (!ctxTask.ver.getId().equals(versionElem)) {
            final Version sharedVersion =
                    ctxTask.task.getVersions().get(versionElem);
            if (!sharedVersion.isShared()) {
                return CtxResolutionState.failed(
                        couchId, path, "version id mismatch"
                );
            }

            ctxTaskOverride = ctxTask.overrideToShared(sharedVersion);
        } else {
            ctxTaskOverride = ctxTask;
        }

        final FileSlot fileSlot = ctxTask.tType.getFileSlots().get(path.elem(8));
        if (fileSlot == null) {
            return CtxResolutionState.failed(
                    couchId, path, "file slot not found"
            );
        }
        final CtxSlot ctxSlot = ctxTaskOverride.slot(fileSlot);

        return new CtxResolutionState(path, ctxSlot);
    }

    //  FIXME proceed with upload processing

    public CtxSolution resolveSolution(
            final String enrollmentId,
            final String solutionCouchId,
            final StudentFilter studentFilter
    ) {
        final CtxResolutionState state =
                resolveSlot(enrollmentId, solutionCouchId, studentFilter);

        if (!state.complete()) {
            //  everything is already reported in logs, we just bail
            return null;
        }

        final String solutionId = state.path.elem(9);
        final long solutionStamp = Squab.Stamped.parse(state.path.elem(10));

        final Solution solutionCouch = solutionDao.findByStamp(
                solutionStamp,
                Solution.class,
                state.ctxSlot.group.getId(),
                state.ctxSlot.student.getId(),
                state.ctxSlot.course.getId(),
                state.ctxSlot.indexEntry.getId(),
                state.ctxSlot.tType.getId(),
                state.ctxSlot.task.getId(),
                state.ctxSlot.ver.getId(),
                state.ctxSlot.slot.getId(),
                solutionId
        );

        //  FIXME this somehow got duplicated
        if (solutionCouch != null) {
            final Score score = score(state.ctxSlot.solution(solutionCouch));
            solutionCouch.setScore(score);
        }

        final Solution solution = Nav.resolveFileType(
                solutionCouch,
                state.ctxSlot.course.getFileTypes()
        );

        return state.ctxSlot.solution(solution);
    }

    public RestSolution restSolution(
            final String enrollmentId,
            final String solId,
            final StudentFilter studentFilter
    ) {
        final CtxSolution ctxSolution = resolveSolution(
                enrollmentId, solId, studentFilter
        );

        return RestSolution.create(ctxSolution, true);
    }
    
    public boolean createSolution(
            final CtxSlot ctxSlot,
            final Solution solution,
            final String contentType,
            final InputSupplier<InputStream> inputSupplier
    ) {
        solution.setupPathElems(ctxSlot.pathForSolution());

        //  there's  no need to store full file type object,
        //      it's easily resolved on-load
        final TreeMap<String, FileType> emptyMap =
                new TreeMap<String, FileType>();
        emptyMap.put(IdNamed._.one(solution.getFileType()).getId(), null);
        solution.setFileType(emptyMap);

        final CouchDao.UpdateStatus createStatus =
                solutionDao.createOrUpdate(solution);
        solution.setCouchRev(createStatus.rev);
        solution.setStamp(createStatus.stamp);
        solutionDao.attachStream(
                solution, FileBase.CONTENT,
                contentType, inputSupplier
        );

        return true;
    }

    public Group group(final String groupId) {
        final Group group = userDao.findOne(Group.class, groupId);
        mark(group.getStudents());
        return group;
    }

    public List<Attachment> attachments(CtxSlot ctxSlot) {
        final List<Attachment> attachments = attachmentDao.findAll(
                Attachment.class,
                ctxSlot.course.getId(),
                ctxSlot.tType.getId(),
                ctxSlot.task.getId(),
                ctxSlot.ver.getId(),
                ctxSlot.slot.getId()
        );

        return Nav.resolveFileType(attachments, ctxSlot.course.getFileTypes());
    }

    public Attachment attachment(Ctx ctxVer, final String slotId, final String id) {
        final Attachment attachment = attachmentDao.findLast(
                Attachment.class,
                ctxVer.getCourse().getId(),
                ctxVer.getAssType().getId(),
                ctxVer.getAss().getId(),
                ctxVer.getVer().getId(),
                slotId,
                id
        );

        return Nav.resolveFileType(attachment, ctxVer.getCourse().getFileTypes());
    }

    public SortedMap<String, List<Solution>> solutions(Ctx ctx) {
        final TreeMap<String, List<Solution>> slotIdToFiles =
                new TreeMap<String, List<Solution>>();

        for (FileSlot slot : ctx.getAssType().getFileSlots().values()) {
            slotIdToFiles.put(slot.getId(), solutions(ctx.ctxSlot(slot)));
        }

        return slotIdToFiles;
    }

    public List<Solution> solutions(CtxSlot ctx) {
        final List<Solution> solutions = solutionDao.findAll(
                Solution.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                ctx.indexEntry.getId(),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId()
        );

        for (Solution solution : solutions) {
            setupScore(ctx, solution);
        }

        return Nav.resolveFileType(solutions, ctx.course.getFileTypes());
    }

    protected void setupScore(CtxSlot ctx, Solution solution) {
        final CtxSolution ctxSolution = ctx.solution(solution);

        //  fetch last of previously fixed scores for a given solution
        final Score fixedScore = score(ctxSolution);

        final Score currentScore;
        if (fixedScore == null) {
            //  okay, nothing yet set, compute preliminary one
            currentScore = ctxSolution.preliminary();
        } else {
            currentScore = fixedScore;
        }

        solution.setScore(currentScore);
    }

    public Solution solution(CtxSlot ctx, String fileId) {
        final Solution solution = solutionDao.findLast(
                Solution.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                ctx.indexEntry.getId(),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId(),
                fileId
        );

        if (solution != null) {
            setupScore(ctx, solution);
        }

        return Nav.resolveFileType(solution, ctx.course.getFileTypes());
    }

    public Result createFile(
            final Ctx ctx,
            final FileSlot slot,
            final FileBase file,
            final InputSupplier<? extends InputStream> inputSupplier,
            final String contentType
    ) {
        try {
            final Squab.CouchFile couchFile = new Squab.CouchFile();

            final byte[] content = ByteStreams.toByteArray(inputSupplier);
            couchFile.setData(content);
            couchFile.setContentType(contentType);
            couchFile.setLength((long) content.length);

            final TreeMap<String, Squab.CouchFile> couchFiles =
                    new TreeMap<String, Squab.CouchFile>();
            couchFiles.put(FileBase.CONTENT, couchFile);

            file.setCouchFiles(couchFiles);
            file.setupPathElems(ctx, slot);

            //  there's  no need to store full file type object,
            //      it's easily resolved on-load
            final TreeMap<String, FileType> emptyMap =
                    new TreeMap<String, FileType>();
            emptyMap.put(IdNamed._.one(file.getFileType()).getId(), null);
            file.setFileType(emptyMap);

            final CouchDao targetDao =
                    file instanceof Attachment ? attachmentDao : solutionDao;
            targetDao.createOrUpdate(file);

            return new Result("File stored successfully", true);
        } catch (IOException e) {
            log.warn("failed to store file", e);
            return new Result("Failed to store file", false);
        }
    }

    // LATER inline this
    public List<? extends FileBase> files(
            String scope, Ctx ctx, FileSlot slot
    ) {
        if (scope.equals(Attachment.SCOPE)) {
            return attachments(ctx.ctxSlot(slot));
        }

        return solutions(ctx.ctxSlot(slot));
    }

    public FileBase file(String scope, Ctx ctx, FileSlot slot, String id) {
        if (scope.equals(Attachment.SCOPE)) {
            return attachment(ctx, slot.getId(), id);
        }

        return solution(ctx.ctxSlot(slot), id);
    }

    public InputSupplier<InputStream> solutionInput(
            final @Nonnull CtxSolution ctxSolution,
            final @Nonnull String fileName
    ) {
        return solutionDao.couchFileGet(
                ctxSolution.solution.getCouchPath(),
                fileName
        );
    }

    public InputSupplier<InputStream> attachmentInput(
            final @Nonnull CtxAttachment ctxAttachment,
            final @Nonnull String fileName
    ) {
        return attachmentDao.couchFileGet(
                ctxAttachment.attachment.getCouchPath(),
                fileName
        );
    }

    public List<String> courseIds() {
        final List<List<String>> axes = 
                metaDao.axes(Course.class, (String) null);

        final List<String> courseIds =
                axes.get(1);

        return courseIds;
    }

    public Course course(final String courseId) {
        final Course course = metaDao.findOne(Course.class, courseId);

        synchronized (courseResolutionMonitor) {
            //  oh, that one was already resolved and published, so we bail
            if (course.isResolved()) {
                return course;
            }

            //  freshly loaded objects are not marked, hence
            //      not yet visible to http workers and safe to resolve without
            //      surprising any http workers unaware of our evilish design
            final String templateId = course.getTemplate();
            if (templateId != null && templateId.length() > 0) {
                final Course template = course(templateId);
                course.setCriterias(mark(extend(
                        template.getCriterias(),
                        course.getCriterias(),
                        new TreeMap<String, Criteria>()
                )));
                course.setFileTypes(mark(extend(
                        template.getFileTypes(),
                        course.getFileTypes(),
                        new TreeMap<String, FileType>()
                )));
            }
            mark(course.getTaskTypes());

            for (final TaskType tType : course.getTaskTypes().values()) {
                mark(tType.getFileSlots());
                for (final FileSlot fSlot : tType.getFileSlots().values()) {
                    fSlot.setFileTypes(mark(resolve(
                            new TreeMap<String, FileType>(fSlot.getFileTypes()),
                            course.getFileTypes()
                    )));
                    fSlot.setCriterias(mark(resolve(
                            new TreeMap<String, Criteria>(fSlot.getCriterias()),
                            course.getCriterias()
                    )));
                }
                tType.setTasks(mark(resolve(
                        new TreeMap<String, Task>(tType.getTasks()),
                        new IdNamed.Resolver<Task>() {
                            public Task resolve(String key) {
                                final Task task = metaDao.findOne(
                                        Task.class,
                                        course.getId(),
                                        tType.getId(),
                                        key
                                );
                                mark(task.getVersions());
                                return task;
                            }
                        }
                )));

                //  LATER for each task iterate over versions and pull the files
            }

            course.setResolved(true);

            return course;
        }
    }

    public Admin adminSome(String login) {
        return userDao.findSome(Admin.class, login);
    }

    public List<String> groupIds() {
        final List<List<String>> axes = 
                userDao.axes(Group.class, (String) null);

        final List<String> groupIds =
                axes.get(1);

        return groupIds;
    }

    public List<Group> groups() {
        final List<Group> groups = userDao.findAll(Group.class);

        for (Group group : groups) {
            mark(group.getStudents());
        }

        return groups;
    }

    public List<String> enrollmentIds() {
        final List<List<String>> axes =
                userDao.axes(Enrollment.class, null, null, null);

        final List<String> enrIds =
                axes.get(3);

        return enrIds;
    }

    public List<Enrollment> enrollments() {
        final List<Enrollment> allEnrs = userDao.findAll(Enrollment.class);

        final ArrayList<Enrollment> result =
                new ArrayList<Enrollment>();
        for (Enrollment enrs : allEnrs) {
            result.add(markEnrollment(enrs));
        }

        return result;
    }

    public Enrollment enrollmentSome(final String id) {
        final Enrollment enrSome =
                userDao.findSome(Enrollment.class, null, null, id);

        return markEnrollment(enrSome);
    }

    protected static Enrollment markEnrollment(Enrollment enrSome) {
        if (enrSome == null) {
            return null;
        }

        final Enrollment enrClone;
        try {
            enrClone = enrSome.clone();
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException("clone() was working recently", e);
        }

        IdNamed._.mark(enrClone.getClasses());
        IdNamed._.mark(enrClone.getIndex());

        return enrClone;
    }

    public Enrollment enrollment(final String id) {
        return markEnrollment(
                userDao.findOne(Enrollment.class, null, null, id)
        );
    }

    public List<Enrollment> enrollmentsForGroup(String groupId) {
        final List<Enrollment> groupEnrs =
                userDao.findAll(Enrollment.class, groupId);

        final ArrayList<Enrollment> result = new ArrayList<Enrollment>();

        for (Enrollment enr : groupEnrs) {
            result.add(markEnrollment(enr));
        }

        return result;
    }

    public SortedMap<Long, Score> scores(Ctx ctx, FileSlot slot, Solution file) {
        final SortedMap<Long, Score> stampToScore = solutionDao.findAllStamped(
                Score.class,
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                ctx.getIndexEntry().getId(),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slot.getId(),
                file.getId()
        );

        if (stampToScore.isEmpty()) {
            final TreeMap<Long, Score> preliminary =
                    new TreeMap<Long, Score>();

            preliminary.put(
                    Long.MAX_VALUE,
                    ctx.ctxSlot(slot).solution(file).preliminary()
            );

            return preliminary;
        }

        return stampToScore;
    }

    public Score score(CtxSolution ctx) {
        final Score lastScore = solutionDao.findLast(
                Score.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                ctx.indexEntry.getId(),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId(),
                ctx.solution.getId()
        );

        if (lastScore == null) {
            return ctx.preliminary();
        }


        return lastScore;
    }

    public Score score(CtxSolution ctx, Long stamp) {
        if (Long.MAX_VALUE == stamp) {
            return ctx.preliminary();
        }

        return solutionDao.findByStamp(
                stamp,
                Score.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                ctx.indexEntry.getId(),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId(),
                ctx.solution.getId()
        );
    }

    public long createScore(CtxSolution ctxSolution, Score score) {
        score.setupPathElems(ctxSolution.pathForScore());

        return solutionDao.createOrUpdate(score).stamp;
    }

    public void updateSolution(Solution solution) {
        solutionDao.createOrUpdate(solution, false);
    }

    public String fileText(
            FileBase file, final String attachment
    ) throws IOException {
        if (file instanceof Solution) {
            return solutionDao.fileText(file, attachment);
        }

        return attachmentDao.fileText(file, attachment);
    }

    public List<String> fileLines(
            FileBase file, final String attachment
    ) throws IOException {
        if (file instanceof Solution) {
            return solutionDao.fileLines(file, attachment);
        }

        return attachmentDao.fileLines(file, attachment);
    }
}
