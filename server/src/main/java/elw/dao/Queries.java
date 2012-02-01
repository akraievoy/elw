package elw.dao;

import base.pattern.Result;
import com.google.common.io.InputSupplier;
import elw.dao.ctx.Scores;
import elw.dao.ctx.Solutions;
import elw.dao.rest.EnrScores;
import elw.vo.*;
import org.akraievoy.couch.Squab;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.SortedMap;

/**
 * Data access abstraction, which is implemented directly and
 * has security-related decorator.
 */
public interface Queries {
    Group group(String groupId);

    List<Attachment> attachments(Ctx ctxVer, String slotId);

    Attachment attachment(Ctx ctxVer, String slotId, String id);

    SortedMap<String, List<Solution>> solutions(Ctx ctx);

    List<Solution> solutions(Solutions ctx);

    //  TODO hack jackson to forfeit content-length reporting
    //      to ensure in-place streaming
    Result createFile(
            Ctx ctx, FileSlot slot,
            FileBase file, InputSupplier<? extends InputStream> inputSupplier,
            String contentType
    );

    List<? extends FileBase> files(
            String scope, Ctx ctx, FileSlot slot
    );

    FileBase file(String scope, Ctx ctx, FileSlot slot, String id);

    InputSupplier<InputStream> inputSupplier(
            @Nonnull Squab squab, @Nonnull String fileName
    );

    List<String> courseIds();

    Course course(String courseId);

    Admin adminSome(String login);

    List<Group> groups();

    List<Enrollment> enrollments();

    //  LATER all methods should be some-ish, as ReST is OK with returning 404s
    Enrollment enrollmentSome(String id);

    List<Enrollment> enrollmentsForGroup(String groupId);

    SortedMap<Long, Score> scores(Ctx ctx, FileSlot slot, Solution file);

    Score score(Scores ctx);

    Score score(Scores ctx, Long stamp);

    SortedMap<Long, Score> scoresAuto(Ctx ctx, FileSlot slot, Solution file);

    long createScore(Score score);

    void updateFile(Solution solution);

    String fileText(
            FileBase file, String attachment
    ) throws IOException;

    List<String> fileLines(
            FileBase file, String attachment
    ) throws IOException;

    List<String> groupIds();

    List<String> enrollmentIds();

    Enrollment enrollment(String id);

    EnrScores enrScores(String enrId, Collection<String> studentIds);
}
