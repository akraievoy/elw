package elw.dao;

import com.google.common.base.Strings;
import elw.vo.Admin;
import elw.vo.Group;
import elw.vo.Student;
import org.codehaus.jackson.map.annotate.JsonSerialize;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Authentication state of any given session.
 */
@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
public class Auth {
    public static final String SESSION_KEY = "elw_auth";
    public static final String MODEL_KEY = SESSION_KEY;

    private String sourceAddr;
    public String getSourceAddr() { return sourceAddr; }
    public void setSourceAddr(String sourceAddr) {
        this.sourceAddr = sourceAddr;
    }

    private List<String> verifiedOpenIds = new ArrayList<String>(1);
    public List<String> getVerifiedOpenIds() {
        return Collections.unmodifiableList(verifiedOpenIds);
    }
    public void setVerifiedOpenIds(List<String> verifiedOpenIds) {
        this.verifiedOpenIds.clear();
        if (verifiedOpenIds != null) {
            this.verifiedOpenIds.addAll(verifiedOpenIds);
        }
    }

    private List<String> verifiedEmails = new ArrayList<String>(1);
    public List<String> getVerifiedEmails() {
        return Collections.unmodifiableList(verifiedEmails);
    }
    public void setVerifiedEmails(List<String> verifiedEmails) {
        this.verifiedEmails.clear();
        if (verifiedEmails != null) {
            this.verifiedEmails.addAll(verifiedEmails);
        }
    }

    private Admin admin;
    public Admin getAdmin() { return admin; }
    public void setAdmin(Admin admin) { this.admin = admin; }

    private Student student;
    public Student getStudent() { return student; }
    public void setStudent(Student student) { this.student = student; }

    private Group group;
    public Group getGroup() { return group; }
    public void setGroup(Group group) { this.group = group; }

    public boolean isAdm() {
        return admin != null;
    }

    public boolean isStud() {
        return student != null && group != null;
    }

    public boolean isVerified() {
        final boolean studentOnlyAuth = admin == null && student != null;
        //noinspection SimplifiableIfStatement
        if (studentOnlyAuth && isVerificationSetupEmpty(student)) {
            return true;
        }

        return !getVerifiedOpenIds().isEmpty() || !getVerifiedEmails().isEmpty();
    }

    public static boolean isVerificationSetupEmpty(Student student) {
        return student.getOpenIds().isEmpty() && isEmailEmpty(student);
    }

    public static boolean isEmailEmpty(Student student) {
        //  couple of silly placeholders made it into the couch, so...
        return Strings.isNullOrEmpty(student.getEmail())
                || "1".equalsIgnoreCase(student.getEmail())
                || "undef@in.ed".equalsIgnoreCase(student.getEmail());
    }

    public boolean isEmpty() {
        return admin == null && (student == null || group == null);
    }

    public void renew(QueriesImpl queries) {
        this.admin = admin == null ? null : queries.adminSome(admin.getId());
        this.group = group == null ? null : queries.group(group.getId());
        this.student = isStud() ? group.getStudents().get(student.getId()) : null;
    }

    public String getId() {
        if (isAdm()) {
            return getAdmin().getId();
        }

        return getGroup().getId() + "-" + getStudent().getId();
    }

    public String getName() {
        if (isAdm()) {
            return getAdmin().getName();
        }

        return getStudent().getName() + " " + getGroup().getName();
    }
}
