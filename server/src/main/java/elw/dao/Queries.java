package elw.dao;

import base.pattern.Result;
import com.google.common.io.InputSupplier;
import elw.dao.ctx.CtxAttachment;
import elw.dao.ctx.CtxSlot;
import elw.dao.ctx.CtxSolution;
import elw.dao.rest.RestEnrollment;
import elw.dao.rest.RestEnrollmentSummary;
import elw.dao.rest.RestSolution;
import elw.vo.*;
import org.akraievoy.couch.Squab;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;

/**
 * Data access abstraction, which is implemented directly and
 * has security-related decorator.
 */
public interface Queries {

    Group group(String groupId);

    List<Attachment> attachments(final CtxSlot ctxSlot);

    Attachment attachment(Ctx ctxVer, String slotId, String id);

    SortedMap<String, List<Solution>> solutions(Ctx ctx);

    List<Solution> solutions(CtxSlot ctx);

    //  FIXME ensure in-place streaming with out-of-band PUT after the entity goes live
    Result createFile(
            Ctx ctx, FileSlot slot,
            FileBase file, InputSupplier<? extends InputStream> inputSupplier,
            String contentType
    );

    List<? extends FileBase> files(
            String scope, Ctx ctx, FileSlot slot
    );

    FileBase file(String scope, Ctx ctx, FileSlot slot, String id);

    InputSupplier<InputStream> solutionInput(
            @Nonnull CtxSolution ctxSolution, @Nonnull String fileName
    );

    List<String> courseIds();

    Course course(String courseId);

    Admin adminSome(String login);

    List<Admin> admins();

    List<Group> groups();

    List<Enrollment> enrollments();

    //  LATER all methods should be some-ish, as ReST is OK with returning 404s
    Enrollment enrollmentSome(String id);

    List<Enrollment> enrollmentsForGroup(String groupId);

    SortedMap<Long, Score> scores(Ctx ctx, FileSlot slot, Solution file);

    Score score(CtxSolution ctx);

    Score score(CtxSolution ctx, Long stamp);

    long createScore(CtxSolution ctxSolution, Score score);

    void updateSolution(Solution solution);

    String fileText(
            FileBase file, String attachment
    ) throws IOException;

    List<String> fileLines(
            FileBase file, String attachment
    ) throws IOException;

    List<String> groupIds();

    List<String> enrollmentIds();

    Enrollment enrollment(String id);

    RestEnrollmentSummary restScores(String enrId, Collection<String> studentIds);

    RestEnrollment restEnrollment(String enrId, final String sourceAddress);

    Map<String,RestSolution> restSolutions(String enrId, SolutionFilter filter);

    boolean createSolution(
            final CtxSlot ctxSlot,
            final Solution solution,
            final String contentType,
            final InputSupplier<InputStream> inputSupplier
    );

    /**
     * Way more straightforward context resolution/validation strategy.
     *
     * Sample couchId is in the form of: <code><pre>
     * Solution-ka95-04-aos_w11-3-lr-lr4-4-code-upload_gvovuo9f.txt-gvovuo9g-
     * </pre></code>
     *
     * There're lots of checks towards inconsistent DB, not only
     * checks against misbehaving injections.
     *
     * @param enrollmentId enrollment ID
     * @param couchId full Couch ID of the solution
     * @param studentFilter extra validation on resolved student object
     *
     * @return {@link elw.dao.Queries.CtxResolutionState} with SlotCtx and Squab.Path,
     *  for further resolution
     */
    CtxResolutionState resolveSlot(
            String enrollmentId,
            String couchId,
            StudentFilter studentFilter
    );

    /**
     * Proceed with resolution from Slot to Solution scope.
     * @see elw.dao.QueriesImpl#resolveSlot(String, String, elw.dao.Queries.StudentFilter)
     */
    CtxSolution resolveSolution(
            String enrollmentId,
            String couchId,
            StudentFilter studentFilter
    );

    InputSupplier<InputStream> attachmentInput(
            @Nonnull CtxAttachment ctxAttachment,
            @Nonnull String fileName
    );

    interface StudentFilter {
        boolean allows(Student student);
    }
    
    RestSolution restSolution(
            String enrollmentId,
            String solutionId,
            StudentFilter studentFilter
    );

    class CtxResolutionState {
        private static final Logger log =
                LoggerFactory.getLogger(CtxResolutionState.class);

        public static final CtxResolutionState FAILED =
                new CtxResolutionState(null, null);

        public final CtxSlot ctxSlot;
        public final Squab.Path path;

        public CtxResolutionState(
                final Squab.Path path,
                final CtxSlot ctxSlot
        ) {
            this.ctxSlot = ctxSlot;
            this.path = path;
        }

        public boolean complete() {
            return path != null && ctxSlot != null;
        }
        
        public static CtxResolutionState failed(
                final String couchId,
                final String message
        ) {
            log.warn("couchId '" + couchId + "': " + message);
            return FAILED;
        }

        public static CtxResolutionState failed(
                final String couchId,
                final Squab.Path path,
                final String message
        ) {
            log.warn("couchId '" + couchId + "': " + message);
            return new CtxResolutionState(path, null);
        }
    }
}
