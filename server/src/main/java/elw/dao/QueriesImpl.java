package elw.dao;

import base.pattern.Result;
import com.google.common.io.ByteStreams;
import com.google.common.io.InputSupplier;
import elw.dao.ctx.CtxStudent;
import elw.dao.ctx.CtxSlot;
import elw.dao.ctx.CtxSolution;
import elw.dao.ctx.CtxTask;
import elw.dao.rest.*;
import elw.vo.*;
import elw.vo.Class;
import elw.vo.Score;
import org.akraievoy.gear.G4Parse;
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
        final List<IndexEntry> index = enr.getIndex();

        final RestEnrollmentSummary enrSummary = new RestEnrollmentSummary();
        for (String studId : studIds) {
            final Student student =
                    group.getStudents().get(studId);

            final CtxStudent ctxStudent =
                    new CtxStudent(enr, course, student, group);

            final RestStudentSummary studentSummary =
                    new RestStudentSummary();

            for (
                    int idxPos = 0, idxSize = index.size();
                    idxPos < idxSize;
                    idxPos++
            ) {
                final CtxTask ctxTask =
                        ctxStudent.task(idxPos);
                final RestTaskSummary taskSummary =
                        new RestTaskSummary();

                for (
                        Map.Entry<String, FileSlot> fsEntry :
                        ctxTask.tType.getFileSlots().entrySet()
                ) {

                    if (fsEntry.getValue().getScoreWeight() > 0) {
                        final CtxSlot ctxSlot =
                                ctxTask.slot(fsEntry.getValue());
                        final List<Solution> solutions =
                                solutions(ctxSlot);

                        final RestSlotSummary slotSummary =
                                RestSlotSummary.create(
                                        ctxSlot,
                                        solutions
                                );

                        taskSummary.register(ctxSlot, slotSummary);
                    }
                }

                studentSummary.register(ctxTask, taskSummary);
            }

            enrSummary.register(ctxStudent, studentSummary);
        }

        return enrSummary;
    }

    //  LATER migrate to this method (requires investigation)
    public RestEnrollment restEnrollment(
            final String enrId,
            final String sourceAddress
    ) {
        final Enrollment enr =
                userDao.findSome(Enrollment.class, null, null, enrId);

        if (enr == null) {
            return null;
        }

        final Course course =
                course(enr.getCourseId());

        final RestEnrollment restEnr =
                RestEnrollment.create(
                        enr,
                        course,
                        sourceAddress
                );

        return restEnr;
    }

    public Group group(final String groupId) {
        final Group group = userDao.findOne(Group.class, groupId);
        mark(group.getStudents());
        return group;
    }

    public List<Attachment> attachments(Ctx ctxVer, final String slotId) {
        final List<Attachment> attachments = attachmentDao.findAll(
                Attachment.class,
                ctxVer.getCourse().getId(),
                ctxVer.getAssType().getId(),
                ctxVer.getAss().getId(),
                ctxVer.getVer().getId(),
                slotId
        );

        return Nav.resolveFileType(attachments, ctxVer.getCourse().getFileTypes());
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
            slotIdToFiles.put(slot.getId(), solutions(ctx.solutions(slot)));
        }

        return slotIdToFiles;
    }

    public List<Solution> solutions(CtxSlot ctx) {
        final List<Solution> solutions = solutionDao.findAll(
                Solution.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                String.valueOf(ctx.idx),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId()
        );

        for (Solution solution : solutions) {
            final CtxSolution scores = ctx.solution(solution);

            //  fetch last of previously fixed scores for a given solution
            final Score fixedScore = score(scores);

            final Score currentScore;
            if (fixedScore == null) {
                //  okay, nothing yet set, compute preliminary one
                currentScore = scores.preliminary();
            } else {
                currentScore = fixedScore;
            }

            solution.setScore(currentScore);
        }

        return Nav.resolveFileType(solutions, ctx.course.getFileTypes());
    }

    public Solution solution(CtxSlot ctx, String fileId) {
        final Solution solution = solutionDao.findLast(
                Solution.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                String.valueOf(ctx.idx),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId(),
                fileId
        );

        if (solution != null) {
            solution.setScore(score(ctx.solution(solution)));
        }

        return Nav.resolveFileType(solution, ctx.course.getFileTypes());
    }

    public Result createFile(
            Ctx ctx, FileSlot slot,
            FileBase file, InputSupplier<? extends InputStream> inputSupplier,
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
            targetDao.update(file);

            return new Result("File stored successfully", true);
        } catch (IOException e) {
            log.warn("failed to store file", e);
            return new Result("Failed to store file", false);
        }
    }

    public List<? extends FileBase> files(
            String scope, Ctx ctx, FileSlot slot
    ) {
        if (scope.equals(Attachment.SCOPE)) {
            return attachments(ctx, slot.getId());
        }

        return solutions(ctx.solutions(slot));
    }

    public FileBase file(String scope, Ctx ctx, FileSlot slot, String id) {
        if (scope.equals(Attachment.SCOPE)) {
            return attachment(ctx, slot.getId(), id);
        }

        return solution(ctx.solutions(slot), id);
    }

    public InputSupplier<InputStream> inputSupplier(
            final @Nonnull Squab squab, @Nonnull String fileName
    ) {
        if (squab instanceof Attachment) {
            return attachmentDao.couchFileGet(squab.getCouchPath(), fileName);
        }
        if (squab instanceof Solution) {
            return solutionDao.couchFileGet(squab.getCouchPath(), fileName);
        }

        throw new IllegalArgumentException(
                "no attachment streaming for: " + squab.getClass()
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
        return userDao.findAll(Enrollment.class);
    }

    public Enrollment enrollmentSome(final String id) {
        return userDao.findSome(Enrollment.class, null, null, id);
    }

    public Enrollment enrollment(final String id) {
        return userDao.findOne(Enrollment.class, null, null, id);
    }

    public List<Enrollment> enrollmentsForGroup(String groupId) {
        return userDao.findAll(Enrollment.class, groupId);
    }

    public SortedMap<Long, Score> scores(Ctx ctx, FileSlot slot, Solution file) {
        return solutionDao.findAllStamped(
                Score.class,
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                String.valueOf(ctx.getIndex()),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slot.getId(),
                file.getId()
        );
    }

    public Score score(CtxSolution ctx) {
        return solutionDao.findLast(
                Score.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                String.valueOf(ctx.idx),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId(),
                ctx.solution.getId()
        );
    }

    public Score score(CtxSolution ctx, Long stamp) {
        return solutionDao.findByStamp(
                stamp,
                Score.class,
                ctx.group.getId(),
                ctx.student.getId(),
                ctx.course.getId(),
                String.valueOf(ctx.idx),
                ctx.tType.getId(),
                ctx.task.getId(),
                ctx.ver.getId(),
                ctx.slot.getId(),
                ctx.solution.getId()
        );
    }

    public SortedMap<Long, Score> scoresAuto(Ctx ctx, FileSlot slot, Solution file) {
        final SortedMap<Long, Score> allScores =
                new TreeMap<Long, Score>(scores(ctx, slot, file));

        final Score newScore = updateAutos(ctx, slot.getId(), file, null);
        newScore.updateStamp();

        allScores.put(newScore.getStamp(), newScore);

        return allScores;
    }

    public static Score updateAutos(
            Ctx ctx, final String slotId, Solution file, final Score score
    ) {
        final Class classDue = ctx.cDue(slotId);
        final FileSlot slot = ctx.getAssType().getFileSlots().get(slotId);

        final Map<String, Double> vars = new TreeMap<String, Double>();
        final double overDue =
                classDue == null ?
                    0.0 :
                    (double) classDue.computeDaysOverdue(file);
        final double onTime = ctx.getEnr().checkOnTime(file) ? 1.0 : 0.0;
        final double onSite =
                ctx.cFrom().checkOnSite(file.getSourceAddress()) ?
                        1.0 :
                        0.0;
        vars.put("$overdue", overDue);
        vars.put("$ontime", onTime);
        vars.put("$onsite", onSite);
        vars.put("$offtime", 1 - onTime);
        vars.put("$offsite", 1 - onSite);
        vars.put("$rapid", ctx.cFrom().checkOnTime(file) ? 1.0 : 0.0);

        //	LATER the validator has to be wired via classname,
        //      not directly in spring context
        if (file.getTotalTests() > 0 || file.isValidated()) {
            vars.put("$passratio", file.getPassRatio());
        }

        final Score res = score == null ? new Score() : score.copy();
        for (Criteria c : slot.getCriterias().values()) {
            final Double ratio;
            final Integer powDef;

            if (res.contains(slot, c)) {
                continue;
            }

            if (!c.auto()) {
                ratio = G4Parse.parse(c.getRatio(), (Double) null);
                powDef = G4Parse.parse(c.getPowDef(), 0);
            } else {
                ratio = c.resolveRatio(vars);
                powDef = c.resolvePowDef(vars);
            }
            if (ratio != null && powDef != null) {
                final String id = Score.idFor(slot, c);
                final Map<String, Integer> pows =
                        new TreeMap<String, Integer>(res.getPows());
                final Map<String, Double> ratios =
                        new TreeMap<String, Double>(res.getRatios());

                pows.put(id, powDef);
                ratios.put(id, ratio);

                res.setPows(pows);
                res.setRatios(ratios);
            }
        }

        return res;
    }

    public long createScore(Score score) {
        return solutionDao.update(score);
    }

    public void updateFile(Solution solution) {
        solutionDao.update(solution, false);
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
