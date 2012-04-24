package elw.web;

import elw.dao.Queries;
import elw.dao.QueriesImpl;
import elw.dao.QueriesSecure;
import elw.miniweb.Message;
import elw.vo.Admin;
import elw.vo.EmailAuth;
import elw.vo.Group;
import elw.vo.Student;
import elw.web.core.W;
import elw.webauth.AuthException;
import elw.webauth.ControllerAuth;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * Adding elw-specific auth handling.
 */
@Controller
@RequestMapping("/auth/**/*")
public class ControllerAuthElw extends ControllerAuth {
    public static final String SESSION_AUTH = "elw_auth";
    public static final String SESSION_QUERIES = "elw_queries";
    public static final String MODEL_AUTH = SESSION_AUTH;
    public static final String MODEL_QUERIES = SESSION_QUERIES;

    private final QueriesImpl queries;
    private final ElwServerConfig elwServerConfig;
    private final Random random;

    public ControllerAuthElw(
            final ElwServerConfig serverConfig,
            final QueriesImpl queries
    ) {
        super(serverConfig);

        this.elwServerConfig = serverConfig;
        this.queries = queries;
        this.random = new Random();
    }

    @Override
    protected String registerChallengeToken(
        final HttpServletRequest req, String emailAddress
    ) throws AuthException {

        checkEmailRegistered(emailAddress);

        final long targetDelayMillis =
            elwServerConfig.getMailTargetDelayMillis();

        final EmailAuth newAuth;
        synchronized (ControllerAuthElw.class) {
            final EmailAuth emailAuth = queries.lastAuth(emailAddress);

            if (emailAuth != null) {
                final long emailEnableMillis =
                        emailAuth.getStamp() + targetDelayMillis;
                if (emailEnableMillis > System.currentTimeMillis()) {
                    throw new AuthException(
                        "mailing " + emailAddress + " too often",
                        new IllegalStateException("email recently sent")
                    );
                }
            }

            newAuth = queries.createAuth(emailAddress);
        }

        return challengeToken(
            newAuth.getEmail(),
            newAuth.getStamp(),
            req.getSession(true).getId()
        );
    }

    protected void checkEmailRegistered(final String emailAddress) throws AuthException {
        try {
            final Admin admin = queries.adminSome(emailAddress);
            if (admin != null) {
                if (!admin.getOpenIds().isEmpty()) {
                   throw new AuthException(
                       "No such email OR OpenID is your way",
                       new IllegalStateException("OpenID is preferred over SMTP")
                   );
                }

                return;
            }

            final List<Group> groups = queries.groups();

            for (Group group : groups) {
                for (Student student : group.getStudents().values()) {
                    if (student.getEmail().equalsIgnoreCase(emailAddress)) {
                        if (student.getOpenIds().isEmpty()) {
                            return;
                        }

                        throw new AuthException(
                            "No such email OR OpenID is your way",
                            new IllegalStateException(
                                "OpenID is preferred over SMTP"
                            )
                        );
                    }
                }
            }
        } finally {
            //  probing admin emails via timing attack, hmmmm?..
            try {
                Thread.sleep(1000 + random.nextInt(500));
            } catch (InterruptedException e) {
                //  ignored
            }
        }

        throw new AuthException(
            "No such email OR OpenID is your way",
            new IllegalStateException(
                "No users with such an email"
            )
        );
    }

    @Override
    protected boolean activateAuth(
        final HttpServletRequest req,
        final String email,
        final String token
    ) throws AuthException {
        final EmailAuth emailAuth = queries.lastAuth(email);

        if (emailAuth == null) {
            return false;
        }

        if (emailAuth.isActivated()) {
            return false;
        }

        final long respTimeout = elwServerConfig.getMailResponseTimeoutMillis();
        if (emailAuth.getStamp() + respTimeout < System.currentTimeMillis()) {
            return false;
        }

        final String challengeToken = challengeToken(
            email,
            emailAuth.getStamp(),
            req.getSession(true).getId()
        );
        
        if (challengeToken.equals(token)) {
            queries.activateAuth(emailAuth);
            return true;
        }

        return false;
    }

    @Override
    protected void processAuthInfo(
        final HttpServletRequest req,
        final HttpSession session,
        final List<String> emails,
        final List<String> openIds
    ) {
        Message.addWarn(req, "openIds: " + openIds + " emails: " + emails);

        for (String email : emails) {
            final Admin admin = queries.adminSome(email);
            if (admin != null) {
                authAdm(req, admin);
                return;
            }
        }

        for (Admin admin : queries.admins()) {
            final List<String> commonOpenIds =
                    new ArrayList<String>(admin.getOpenIds());
            commonOpenIds.retainAll(openIds);

            if (!commonOpenIds.isEmpty()) {
                authAdm(req, admin);
                return;
            }
        }

        final List<Group> groups = queries.groups();
        for (Group group : groups) {
            for (Student student : group.getStudents().values()) {
                final List<String> commonOpenIds =
                    new ArrayList<String>(student.getOpenIds());
                commonOpenIds.retainAll(openIds);

                if (!commonOpenIds.isEmpty() ||
                    emails.contains(student.getEmail())) {

                    authStudent(req, student);
                    return;
                }
            }
        }
    }

    protected void authAdm(
            final HttpServletRequest req,
            final Admin admin
    ) {
        final QueriesSecure.Auth auth = new QueriesSecure.Auth();
        auth.setId(admin.getId());
        auth.setName(admin.getName());
        auth.setRoles(Collections.singletonList(QueriesSecure.Auth.ROLE_ADM));
        auth.setConfirmed(true);

        setupAuthCommon(req, auth);
    }

    protected void authStudent(
            final HttpServletRequest req,
            final Student student
    ) {
        final QueriesSecure.Auth auth = new QueriesSecure.Auth();
        auth.setId(student.getEmail());
        auth.setName(student.getName());
        auth.setRoles(Collections.singletonList(QueriesSecure.Auth.ROLE_STUD));
        auth.setConfirmed(true);

        setupAuthCommon(req, auth);
    }

    protected void setupAuthCommon(
            final HttpServletRequest req,
            final QueriesSecure.Auth auth
    ) {
        auth.setExpiry(
                System.currentTimeMillis() +
                        elwServerConfig.getSessionExpiryMillis()
        );
        auth.setSourceAddr(W.resolveRemoteAddress(req));

        final Queries queriesSecure = queries.secure(auth);
        auth.setGroupIds(queriesSecure.groupIds());
        auth.setEnrIds(queriesSecure.enrollmentIds());
        auth.setCourseIds(queriesSecure.courseIds());

        req.getSession().setAttribute(SESSION_AUTH, auth);
        req.getSession().setAttribute(SESSION_QUERIES, queriesSecure);
    }
}
