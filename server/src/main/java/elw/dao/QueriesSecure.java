package elw.dao;

import base.pattern.Result;
import com.google.common.io.InputSupplier;
import elw.dao.ctx.*;
import elw.dao.rest.RestEnrollment;
import elw.dao.rest.RestEnrollmentSummary;
import elw.dao.rest.RestSolution;
import elw.vo.*;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

public class QueriesSecure implements Queries {
    public static final String MODEL_KEY = "elw_queries";
    private final QueriesImpl queries;

    private final Auth auth;
    public Auth getAuth() { return auth; }

    private final List<String> visibleGroupIds = new ArrayList<String>(1);
    public List<String> getVisibleGroupIds() {
        return Collections.unmodifiableList(visibleGroupIds);
    }

    private final List<String> visibleEnrIds = new ArrayList<String>(2);
    public List<String> getVisibleEnrIds() {
        return Collections.unmodifiableList(visibleEnrIds);
    }

    private final List<String> visibleCourseIds = new ArrayList<String>(2);
    public List<String> getVisibleCourseIds() {
        return Collections.unmodifiableList(visibleCourseIds);
    }

    public QueriesSecure(QueriesImpl queries, Auth auth) {
        this.auth = auth;
        this.queries = queries;
    }

    public void precacheVisible() {
        visibleGroupIds.addAll(groupIds());
        visibleEnrIds.addAll(enrollmentIds());
        visibleCourseIds.addAll(courseIds());
    }

    public RestEnrollmentSummary restScores(
            final String enrId,
            final Collection<String> studentIds
    ) {
        if (auth.isAdm()) {
            return queries.restScores(enrId, studentIds);
        }

        if (!enrollmentIds().contains(enrId)) {
            return null;
        }

        final TreeSet<String> studIds = new TreeSet<String>();
        if (auth.isStud()) {
            studIds.add(auth.getStudent().getId());
        }
        if (studentIds != null) {
            studIds.retainAll(studentIds);
        }

        return queries.restScores(enrId, studIds);
    }

    public RestEnrollment restEnrollment(
            final String enrId,
            final String sourceAddress
    ) {
        if (auth.isAdm()) {
            return queries.restEnrollment(enrId, sourceAddress);
        }

        if (!enrollmentIds().contains(enrId)) {
            return null;
        }

        final RestEnrollment enrollment =
                queries.restEnrollment(enrId, sourceAddress);
        final Group group = 
                group(enrollment.getGroupId());

        for (RestEnrollment.RestIndexEntry entry :
                enrollment.getIndex().values()) {
            final TreeMap<String, Version> versions =
                    new TreeMap<String, Version>();

            //  hide all versions for not yet open tasks
            if (entry.getClassFrom().isStarted()) {
                //  hide all versions which are not shared
                for (Map.Entry<String, Version> verEntry :
                        entry.getTask().getVersions().entrySet()) {
                    if (verEntry.getValue().isShared()) {
                        versions.entrySet().add(verEntry);
                    }
                }

                //  hide all versions which are not assigned
                final Version version = Nav.resolveVersion(
                        entry.getTask(),
                        entry.getVerAnchor(),
                        entry.getVerStep(),
                        group.getStudents().keySet(),
                        auth.getStudent().getId()
                );

                versions.put(version.getId(), version);
            }

            entry.getTask().setVersions(versions);

            //  hide version anchors and steps
            entry.setVerStep(-1);
            entry.setVerAnchor(-1);
        }

        return enrollment;
    }

    public Map<String,RestSolution> restSolutions(
            final String enrId,
            final SolutionFilter filter
    ) {
        if (auth.isAdm()) {
            return queries.restSolutions(enrId, filter);
        }

        if (!enrollmentIds().contains(enrId)) {
            return null;
        }

        //  TODO a bit of null-safety here won't hurt anyone
        return queries.restSolutions(enrId, new SolutionFilter() {
            public boolean preAllows(CtxStudent ctxStudent) {
                final boolean sameStudent =
                        ctxStudent.student.equals(auth.getStudent());
                return sameStudent && filter.preAllows(ctxStudent);
            }

            public boolean allows(CtxSolution ctxSolution) {
                //  funnel call to the original filter,
                //      preAllows() did the job already
                return filter.allows(ctxSolution);
            }
        });
    }

    public RestSolution restSolution(
            final String enrollmentId,
            final String solutionId,
            final StudentFilter outer
    ) {
        if (auth.isAdm()) {
            return queries.restSolution(
                    enrollmentId, solutionId, outer
            );
        }

        if (!enrollmentIds().contains(enrollmentId)) {
            return null;
        }

        final StudentFilter inner = secureStudentFilter(outer);

        final RestSolution restSolution = queries.restSolution(
                enrollmentId, solutionId, inner
        );

        return restSolution;
    }

    protected StudentFilter secureStudentFilter(final StudentFilter outer) {
        return new StudentFilter() {
            public boolean allows(Student student) {
                final boolean outerAllowed =
                        outer == null || outer.allows(student);

                if (auth.isAdm()) {
                    return outerAllowed;
                }

                return outerAllowed && authStudent(student);
            }
        };
    }

    protected boolean authStudent(Student student) {
        return student.equals(auth.getStudent());
    }

    public CtxResolutionState resolveSlot(
            final String enrollmentId,
            final String couchId,
            final StudentFilter outer
    ) {
        if (auth.isAdm()) {
            return queries.resolveSlot(enrollmentId, couchId, outer);
        }

        if (!enrollmentIds().contains(enrollmentId)) {
            return CtxResolutionState.failed(
                    couchId,
                    "enrollment access denied"
            );
        }

        return queries.resolveSlot(
                enrollmentId, couchId, secureStudentFilter(outer)
        );
    }

    public CtxSolution resolveSolution(
            final String enrollmentId,
            final String couchId,
            final StudentFilter outer
    ) {
        if (auth.isAdm()) {
            return queries.resolveSolution(enrollmentId, couchId, outer);
        }

        if (!enrollmentIds().contains(enrollmentId)) {
            return null;
        }

        return queries.resolveSolution(
                enrollmentId, couchId, secureStudentFilter(outer)
        );
    }

    public boolean createSolution(
            final CtxSlot ctxSlot,
            final Solution solution,
            final String contentType,
            final InputSupplier<InputStream> inputSupplier
    ) {
        //  LATER admin should not be able to create solutions
        //      at least until impersonation is more or less functional
        if (auth.isAdm()) {
            return queries.createSolution(
                    ctxSlot, solution,
                    contentType, inputSupplier
            );
        }

        if (!enrollmentIds().contains(ctxSlot.enr.getId())) {
            return false;
        }

        if (!authStudent(ctxSlot.student)) {
            return false;
        }

        if (!ctxSlot.open()) {
            return false;
        }

        //noinspection SimplifiableIfStatement
        if (!ctxSlot.writable(ctxSlot.slot, stateForSlot(ctxSlot))) {
            return false;
        }

        return queries.createSolution(
                ctxSlot, solution,
                contentType, inputSupplier
        );
    }

    public Admin adminSome(String login) {
        if (auth.isAdm()) {
            return queries.adminSome(login);
        }

        return null;
    }


    public List<Admin> admins() {
        if (auth.isAdm()) {
            return queries.admins();
        }

        return Collections.emptyList();
    }


    public List<Attachment> attachments(final CtxSlot ctxSlot) {
        if (auth.isAdm()) {
            return queries.attachments(ctxSlot);
        }

        if (!enrollmentIds().contains(ctxSlot.enr.getId())) {
            return null;
        }

        if (!ctxSlot.open()) {
            return null;
        }

        //noinspection SimplifiableIfStatement
        if (!ctxSlot.readable(ctxSlot.slot, stateForSlot(ctxSlot))) {
            return null;
        }

        return queries.attachments(
                ctxSlot
        );
    }

    protected CtxTask.StateForSlot stateForSlot(final CtxSlot ctxSlot) {
        return new CtxTask.StateForSlot() {
            public State getState(FileSlot slot) {
                final CtxSlot ctxApproveSlot = ctxSlot.slot(slot);
                return ctxApproveSlot.state(solutions(ctxApproveSlot));
            }
        };
    }

    public Attachment attachment(Ctx ctxVer, String slotId, String id) {
        return null;  //	TODO review
    }

    public SortedMap<String, List<Solution>> solutions(Ctx ctx) {
        return null;  //	TODO review
    }

    public Score score(CtxSolution ctx) {
        return null;  //	TODO review
    }

    public List<Solution> solutions(CtxSlot ctx) {
        return null;  //	TODO review
    }

    public Score score(CtxSolution ctx, Long stamp) {
        return null;  //	TODO review
    }

    public Result createFile(Ctx ctx, FileSlot slot, FileBase file, InputSupplier<? extends InputStream> inputSupplier, String contentType) {
        return null;  //	TODO review
    }

    public List<? extends FileBase> files(String scope, Ctx ctx, FileSlot slot) {
        return null;  //	TODO review
    }

    public FileBase file(String scope, Ctx ctx, FileSlot slot, String id) {
        return null;  //	TODO review
    }

    public InputSupplier<InputStream> attachmentInput(
            final @Nonnull CtxAttachment ctxAttachment,
            final @Nonnull String fileName
    ) {
        if (auth.isAdm()) {
            return queries.attachmentInput(ctxAttachment, fileName);
        }

        if (!enrollmentIds().contains(ctxAttachment.enr.getId())) {
            return null;
        }

        if (!ctxAttachment.open()) {
            return null;
        }

        //noinspection SimplifiableIfStatement
        if (!ctxAttachment.readable(ctxAttachment.slot, stateForSlot(ctxAttachment))) {
            return null;
        }

        return queries.attachmentInput(
                ctxAttachment, fileName
        );
    }

    public InputSupplier<InputStream> solutionInput(
            final @Nonnull CtxSolution ctxSolution,
            final @Nonnull String fileName
    ) {
        if (auth.isAdm()) {
            return queries.solutionInput(ctxSolution, fileName);
        }

        if (!enrollmentIds().contains(ctxSolution.enr.getId())) {
            return null;
        }

        if (!authStudent(ctxSolution.student)) {
            return null;
        }

        if (!ctxSolution.open()) {
            return null;
        }

        //noinspection SimplifiableIfStatement
        if (!ctxSolution.readable(ctxSolution.slot, stateForSlot(ctxSolution))) {
            return null;
        }

        return queries.solutionInput(
                ctxSolution, fileName
        );
    }

    public List<Course> courses() {
        return queries.courses();
    }

    public List<String> courseIds() {
        //  check auth-scope cache
        final List<String> preCached = visibleCourseIds;

        if (!preCached.isEmpty()) {
            return preCached;
        }
        
        //  admin sees anything
        if (auth.isAdm()) {
            final List<String> courseIds =
                    queries.courseIds();

            return courseIds;
        }

        //  any other user sees courses he/she is enrolled to
        final List<Enrollment> enrollments =
                enrollments();

        final List<String> courseIds =
                new ArrayList<String>();
        
        for (Enrollment enr : enrollments) {
            final String courseId = enr.getCourseId();
            if (!courseIds.contains(courseId)) {
                courseIds.add(courseId);
            }
        }

        return Collections.unmodifiableList(courseIds);
    }

    public Course course(String courseId) {
        if (!courseIds().contains(courseId)) {
            return null;
        }

        return queries.course(courseId);
    }

    public List<String> groupIds() {
        //  check auth-scope cache
        final List<String> preCached = visibleGroupIds;

        if (!preCached.isEmpty()) {
            return preCached;
        }

        //  admin sees anything
        if (auth.isAdm()) {
            return queries.groupIds();
        }

        // any other user sees only groups he/she is a student of
        final List<String> groupIds = new ArrayList<String>();
        if (auth.isStud()) {
            groupIds.add(auth.getGroup().getId());
        }

        return Collections.unmodifiableList(groupIds);
    }

    public void invalidateCaches() {
        if (auth.isAdm()) {
            queries.invalidateCaches();
        }
    }

    public Group group(String groupId) {
        if (groupIds().contains(groupId)) {
            return queries.group(groupId);
        }

        return null;
    }

    public List<Group> groups() {
        //  admin sees anything
        if (auth.isAdm()) {
            return queries.groups();
        }

        //  check auth-scope explicit group listing
        final List<String> authExplicit = visibleGroupIds;
        if (!authExplicit.isEmpty()) {
            final List<Group> groups =
                    new ArrayList<Group>();

            for (String groupId : authExplicit) {
                groups.add(queries.group(groupId));
            }

            return Collections.unmodifiableList(groups);
        }

        // any other user sees only groups he/she is a student of
        final List<Group> groups =
                new ArrayList<Group>();

        groups.add(auth.getGroup());

        return Collections.unmodifiableList(groups);
    }

    public List<String> enrollmentIds() {
        //  check auth-scope cache
        final List<String> preCached = visibleEnrIds;

        if (!preCached.isEmpty()) {
            return preCached;
        }

        //  admin sees anything
        if (auth.isAdm()) {
            return queries.enrollmentIds();
        }

        // any other user sees only enrollments for respective groups
        final List<Group> groups = groups();

        final List<String> enrollmentIds =
                new ArrayList<String>();

        for (Group userGroup : groups) {
            final List<Enrollment> enrForGroup =
                    queries.enrollmentsForGroup(userGroup.getId());
            for (Enrollment enr : enrForGroup) {
                enrollmentIds.add(enr.getId());
            }
        }

        return Collections.unmodifiableList(enrollmentIds);
    }

    public List<Enrollment> enrollments() {
        //  admin sees anything
        if (auth.isAdm()) {
            return queries.enrollments();
        }

        //  check auth-scope cache
        final List<String> preCached = visibleEnrIds;
        if (!preCached.isEmpty()) {
            final List<Enrollment> enrollments =
                    new ArrayList<Enrollment>();

            for (String enrId : preCached) {
                enrollments.add(hideVersions(queries.enrollment(enrId)));
            }
            
            return Collections.unmodifiableList(enrollments);
        }

        // any other user sees only enrollments for respective groups
        final List<Group> groups = groups();
        final List<Enrollment> enrollments =
                new ArrayList<Enrollment>();

        for (Group userGroup : groups) {
            final List<Enrollment> enrForGroup =
                    queries.enrollmentsForGroup(userGroup.getId());

            for (Enrollment enr : enrForGroup) {
                enrollments.add(hideVersions(enr));
            }
        }

        return Collections.unmodifiableList(enrollments);
    }
    
    protected Enrollment hideVersions(Enrollment enr) {
        try {
            final Enrollment clone = enr.clone();

            for (IndexEntry entry : clone.getIndex().values()) {
                entry.setVerAnchor(1);
                entry.setVerStep(-1);
            }

            return clone;
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }
    }

    public Enrollment enrollment(String enrId) {
        if (auth.isAdm()) {
            return queries.enrollment(enrId);
        }

        if (enrollmentIds().contains(enrId)) {
            return hideVersions(queries.enrollment(enrId));
        }

        return null;
    }

    public Enrollment enrollmentSome(String enrId) {
        if (auth.isAdm()) {
            return queries.enrollmentSome(enrId);
        }

        if (enrollmentIds().contains(enrId)) {
            final Enrollment enrSome = queries.enrollmentSome(enrId);
            if (enrSome != null) {
                return hideVersions( enrSome);
            }
        }

        return null;
    }

    public List<Enrollment> enrollmentsForGroup(String groupId) {
        if (auth.isAdm()) {
            return queries.enrollmentsForGroup(groupId);
        }

        if (!groupIds().contains(groupId)) {
            return null;
        }

        final List<Enrollment> enr = queries.enrollmentsForGroup(groupId);
        for (int enrPos = 0; enrPos < enr.size(); enrPos++) {
            enr.set(enrPos, hideVersions(enr.get(enrPos)));
        }

        return enr;
    }

    public SortedMap<Long, Score> scores(Ctx ctx, FileSlot slot, Solution file) {
        return null;  //	TODO review
    }

    public long createScore(CtxSolution ctxSolution, Score score) {
        return 0;  //	TODO review
    }

    public void updateSolution(Solution solution) {
        //  log something sensible
    }

    public String fileText(FileBase file, String attachment) throws IOException {
        return null;  //	TODO review
    }

    public List<String> fileLines(FileBase file, String attachment) throws IOException {
        return null;  //	TODO review
    }

}
