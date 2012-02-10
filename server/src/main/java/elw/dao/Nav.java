package elw.dao;

import elw.vo.*;
import elw.vo.Class;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Days;

import java.util.*;

import static elw.vo.IdNamed._.resolve;

/**
 * Common object resolution/navigation logic.
 */
public class Nav {
    public static Student findStudent(
            final Group group,
            final String authId
    ) {
        for (Student student : group.getStudents().values()) {
            if (authId.equalsIgnoreCase(student.getEmail())) {
                return student;
            }
        }

        return null;
    }
    
    public static List<Version> filterPrivateVersions(Task task) {
        if (task.getVersions().isEmpty()) {
            throw new IllegalStateException(
                    "task has no versions: " + task.getId()
            );
        }

        final List<Version> versions = new ArrayList<Version>(
            task.getVersions().values()
        );
        final List<Version> versionsPriv = new ArrayList<Version>(
                versions
        );
        for (
            Iterator<Version> verIt = versionsPriv.iterator();
            verIt.hasNext();
        ) {
            if (verIt.next().isShared()) {
                verIt.remove();
            }
        }

        return versionsPriv.isEmpty() ? versions : versionsPriv;
    }

    //  FIXME this should be stored in Couch
    //      along with first access to the task
    public static Version resolveVersion(
            final Task task,
            final int anchor, final int step,
            final Collection<String> studentIds, final String studentId
    ) {
        final List<Version> versionsEff =
                filterPrivateVersions(task);
        final ArrayList<String> studIds =
                new ArrayList<String>(studentIds);

        Collections.sort(studIds);

        final int studIdx = studIds.indexOf(studentId);
        final int vSize = versionsEff.size();
        final int verIdx = (anchor + (vSize + step) * studIdx) % vSize;

        return versionsEff.get(verIdx);
    }
    
    public static Version resolveVersion(
            final Task task,
            final IndexEntry indexEntry,
            final Group group,
            final String studentId
    ) {
        return resolveVersion(
                task,
                indexEntry.getVerAnchor(),
                indexEntry.getVerStep(),
                group.getStudents().keySet(),
                studentId
        );
    }

    public static <F extends FileBase> List<F> resolveFileType(
            final List<F> files,
            final SortedMap<String, FileType> fileTypes
    ) {
        for (F file : files) {
            resolveFileType(file, fileTypes);
        }
        return files;
    }

    public static <F extends FileBase> F resolveFileType(
            final F file,
            final SortedMap<String, FileType> fileTypes
    ) {
        file.setFileType(resolve(
                new TreeMap<String, FileType>(file.getFileType()),
                fileTypes
        ));

        return file;
    }
}
