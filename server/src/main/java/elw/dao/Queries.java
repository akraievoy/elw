package elw.dao;

import base.pattern.Result;
import com.google.common.io.ByteStreams;
import com.google.common.io.InputSupplier;
import elw.vo.*;
import elw.vo.Class;
import org.akraievoy.gear.G4Parse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import static elw.vo.IdNamed._.*;

public class Queries {
    private static final Logger log = LoggerFactory.getLogger(Queries.class);

    private final CouchDao couchDao;

    public Queries(CouchDao couchDao) {
        this.couchDao = couchDao;
    }

    public CouchDao getCouchDao() {
        return couchDao;
    }

    //  FIXME resolve Solution.file from Course.fileTypes

    public Group group(final String groupId) {
        final Group group = couchDao.findOne(Group.class, groupId);
        mark(group.getStudents());
        return group;
    }

    public List<Attachment> attachments(Ctx ctxVer, final String slotId) {
        final List<Attachment> attachments = couchDao.findAll(
                Attachment.class,
                ctxVer.getCourse().getId(),
                ctxVer.getAssType().getId(),
                ctxVer.getAss().getId(),
                ctxVer.getVer().getId(),
                slotId
        );
        return attachments;
    }

    public Attachment attachment(Ctx ctxVer, final String slotId, final String id) {
        final Attachment attachment = couchDao.findLast(
                Attachment.class,
                ctxVer.getCourse().getId(),
                ctxVer.getAssType().getId(),
                ctxVer.getAss().getId(),
                ctxVer.getVer().getId(),
                slotId,
                id
        );
        return attachment;
    }

    public SortedMap<String, List<Solution>> solutions(Ctx ctx) {
        final TreeMap<String, List<Solution>> slotIdToFiles = new TreeMap<String, List<Solution>>();
        for (FileSlot slot : ctx.getAssType().getFileSlots().values()) {
            slotIdToFiles.put(slot.getId(), solutions(ctx, slot.getId()));
        }
        return slotIdToFiles;
    }

    public List<Solution> solutions(Ctx ctx, final String slotId) {
        return couchDao.findAll(
                Solution.class,
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                String.valueOf(ctx.getIndex()),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slotId
        );
    }

    public Solution solution(Ctx ctx, String slotId, String fileId) {
        return couchDao.findLast(
                Solution.class,
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                String.valueOf(ctx.getIndex()),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slotId,
                fileId
        );
    }

    //	TODO jackson hacks to forfeit content-length reporting to ensure in-place streaming
    public Result createFile(
            Ctx ctx, FileSlot slot,
            FileBase file, InputSupplier<? extends InputStream> inputSupplier, final String contentType
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

            //  there's  no need to store full file type object, it's easily resolved on-load
            final TreeMap<String, FileType> emptyMap = new TreeMap<String, FileType>();
            emptyMap.put(IdNamed._.one(emptyMap).getId(), null);
            file.setFileType(emptyMap);

            couchDao.update(file);

            return new Result("File stored successfully", true);
        } catch (IOException e) {
            log.warn("failed to store file", e);
            return new Result("Failed to store file", false);
        }
    }

    public List<? extends FileBase> files(String scope, Ctx ctx, FileSlot slot) {
        if (scope.equals(Attachment.SCOPE)) {
            return attachments(ctx, slot.getId());
        }

        return solutions(ctx, slot.getId());
    }

    public FileBase file(String scope, Ctx ctx, FileSlot slot, String id) {
        if (scope.equals(Attachment.SCOPE)) {
            return attachment(ctx, slot.getId(), id);
        }

        return solution(ctx, slot.getId(), id);
    }

    public Course course(final String courseId) {
        final Course course = couchDao.findOne(Course.class, courseId);

        final String templateId = course.getTemplate();
        if (templateId != null && templateId.length() > 0) {
            final Course template = course(templateId);
            course.setCriterias(mark(extend(
                    template.getCriterias(), course.getCriterias(), new TreeMap<String, Criteria>()
            )));
            course.setFileTypes(mark(extend(
                    template.getFileTypes(), course.getFileTypes(), new TreeMap<String, FileType>()
            )));
        }
        mark(course.getTaskTypes());

        for (final TaskType tType : course.getTaskTypes().values()) {
            mark(tType.getFileSlots());
            for (final FileSlot fSlot : tType.getFileSlots().values()) {
                fSlot.setFileTypes(mark(resolve(
                        new TreeMap<String, FileType>(fSlot.getFileTypes()), course.getFileTypes()
                )));
                fSlot.setCriterias(mark(resolve(
                        new TreeMap<String, Criteria>(fSlot.getCriterias()), course.getCriterias()
                )));
            }
            tType.setTasks(mark(resolve(
                    new TreeMap<String, Task>(tType.getTasks()),
                    new IdNamed.Resolver<Task>() {
                        public Task resolve(String key) {
                            final Task task = couchDao.findOne(Task.class, course.getId(), tType.getId(), key);
                            mark(task.getVersions());
                            return task;
                        }
                    }
            )));

            //  FIXME for each task iterate over versions and pull the files
        }

        return course;
    }

    public Admin adminSome(String login) {
        return couchDao.findSome(Admin.class, login);
    }

    public List<Group> groups() {
        final List<Group> groups = couchDao.findAll(Group.class);

        for (Group group : groups) {
            mark(group.getStudents());
        }

        return groups;
    }

    public List<Enrollment> enrollments() {
        return couchDao.findAll(Enrollment.class);
    }

    public Enrollment enrollmentSome(final String id) {
        return couchDao.findSome(Enrollment.class, null, null, id);
    }

    public List<Enrollment> enrollmentsForGroup(String groupId) {
        return couchDao.findAll(Enrollment.class, groupId);
    }

    public SortedMap<Long, Score> scores(Ctx ctx, FileSlot slot, Solution file) {
        return couchDao.findAllStamped(
                Score.class,
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                String.valueOf(ctx.getIndex()),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slot.getId(),
                file.getId(),
                String.valueOf(file.getStamp())
        );
    }

    public Score score(Ctx ctx, FileSlot slot, Solution file, Long stamp) {
        return couchDao.findByStamp(
                stamp,
                Score.class,
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                String.valueOf(ctx.getIndex()),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slot.getId(),
                file.getId(),
                String.valueOf(file.getStamp())
        );
    }

    public SortedMap<Long, Score> scoresAuto(Ctx ctx, FileSlot slot, Solution file) {
        final SortedMap<Long, Score> allScores = scores(ctx, slot, file);

        final Score newScore = updateAutos(ctx, slot.getId(), file, null);
        newScore.updateStamp();

        allScores.put(newScore.getStamp(), newScore);

        return allScores;
    }

    public static Score updateAutos(Ctx ctx, final String slotId, Solution file, final Score score) {
        final Class classDue = ctx.cDue(slotId);
        final FileSlot slot = ctx.getAssType().getFileSlots().get(slotId);

        final Map<String, Double> vars = new TreeMap<String, Double>();
        final double overDue = classDue == null ? 0.0 : (double) classDue.computeDaysOverdue(file);
        final double onTime = ctx.getEnr().checkOnTime(file) ? 1.0 : 0.0;
        final double onSite = ctx.cFrom().checkOnSite(file.getSourceAddress()) ? 1.0 : 0.0;
        vars.put("$overdue", overDue);
        vars.put("$ontime", onTime);
        vars.put("$onsite", onSite);
        vars.put("$offtime", 1 - onTime);
        vars.put("$offsite", 1 - onSite);
        vars.put("$rapid", ctx.cFrom().checkOnTime(file) ? 1.0 : 0.0);

        //	LATER the validator has to be wired via classname, not directly in spring context
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
                final String id = res.idFor(slot, c);
                final Map<String, Integer> pows = new TreeMap<String, Integer>(res.getPows());
                final Map<String, Double> ratios = new TreeMap<String, Double>(res.getRatios());

                pows.put(id, powDef);
                ratios.put(id, ratio);

                res.setPows(pows);
                res.setRatios(ratios);
            }
        }

        return res;
    }
}
