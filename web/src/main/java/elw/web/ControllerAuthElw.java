package elw.web;

import elw.dao.Auth;
import elw.dao.QueriesImpl;
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
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
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

    @RequestMapping(
            value = "nameresponse",
            method = RequestMethod.POST
    )
    public ModelAndView do_nameresponseGet(
            final HttpServletRequest req,
            final HttpServletResponse resp
    ) throws IOException {
        final String nameFirstParam = req.getParameter("name_first");
        if (nameFirstParam == null || nameFirstParam.trim().length() == 0) {
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "first name not set"
            );
            return null;
        }
        final String nameFirst = nameFirstParam.trim();

        final String nameLastParam = req.getParameter("name_last");
        if (nameLastParam == null || nameLastParam.trim().length() == 0) {
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "last name not set"
            );
            return null;
        }
        final String nameLast = nameLastParam.trim();

        final String nameFull = nameLast + ' ' + nameFirst;

        for (Admin admin : queries.admins()) {
            if (admin.getName().equalsIgnoreCase(nameFull)) {
                Message.addWarn(req, "Logged in as " + admin.getName() + ": READ-ONLY");
                authAdm(
                        req,
                        admin,
                        Collections.<String>emptyList(),
                        Collections.<String>emptyList()
                );
                return processAuthSuccess(req, resp);
            }
        }

        final List<Group> groups = queries.groups();
        for (Group group : groups) {
            for (Student student : group.getStudents().values()) {
                if (student.getName().equalsIgnoreCase(nameFull)) {
                    final Auth auth = ControllerElw.auth(req);
                    if (auth != null && auth.isAdm() && auth.isVerified()) {
                        //  impersonate, leaving admin credentials intact
                        Message.addInfo(req, "impersonated as " + student.getName());
                        auth.setGroup(group);
                        auth.setStudent(student);
                        return processAuthSuccess(req, resp);
                    }

                    if (Auth.isVerificationSetupEmpty(student)) {
                        Message.addInfo(req, "Logged in as " + student.getName() + ", no extra verifications required");
                    } else {
                        Message.addWarn(req, "Logged in as " + student.getName() + ": READ-ONLY");
                        if (!Auth.isEmailEmpty(student)) {
                            Message.addWarn(req, "Log-in via email " + student.getEmail() + " to gain full access");
                        }
                        if (!student.getOpenIds().isEmpty()) {
                            Message.addWarn(req, "Log-in via OpenID " + student.getOpenIds() + " to gain full access");
                        }
                    }
                    authStudent(
                            req, student, group,
                            Collections.<String>emptyList(),
                            Collections.<String>emptyList()
                    );
                    return processAuthSuccess(req, resp);
                }
            }
        }

        Message.addWarn(req, "No users found, please check your spelling?");
        resp.sendRedirect(elwServerConfig.getBaseUrl() + "auth.html");
        return null;
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
        Message.addInfo(req, "openIds: " + openIds + " emails: " + emails);

        for (String email : emails) {
            final Admin admin = queries.adminSome(email);
            if (admin != null) {
                authAdm(
                    req,
                    admin,
                    Collections.singletonList(email),
                    intersect(openIds, admin.getOpenIds())
                );
                return;
            }
        }

        for (Admin admin : queries.admins()) {
            final List<String> commonOpenIds =
                    intersect(openIds, admin.getOpenIds());

            if (!commonOpenIds.isEmpty()) {
                authAdm(
                    req,
                    admin,
                    openIds.size() == 1 ? emails : Collections.<String>emptyList(),
                    commonOpenIds
                );
                return;
            }
        }

        final List<Group> groups = queries.groups();
        for (Group group : groups) {
            for (Student student : group.getStudents().values()) {
                final List<String> commonOpenIds =
                        intersect(openIds, student.getOpenIds());

                if (!commonOpenIds.isEmpty()) {
                    authStudent(
                        req,
                        student,
                        group,
                        openIds.size() == 1 ? emails : Collections.<String>emptyList(),
                        commonOpenIds
                    );
                    return;
                }

                if (emails.contains(student.getEmail())) {
                    authStudent(
                        req,
                        student,
                        group,
                        Collections.singletonList(student.getEmail()),
                        commonOpenIds
                    );
                    return;
                }
            }
        }
    }

    protected List<String> intersect(
            final List<String> verified,
            final List<String> current) {
        final List<String> common =
                new ArrayList<String>(current);
        common.retainAll(verified);
        return common;
    }

    protected void authAdm(
            final HttpServletRequest req,
            final Admin admin,
            final List<String> verifiedEmails,
            final List<String> verifiedOpenIds
    ) {
        final Auth auth = new Auth();
        auth.setAdmin(admin);

        setupAuthCommon(req, auth, verifiedEmails, verifiedOpenIds);
    }

    protected void authStudent(
            final HttpServletRequest req,
            final Student student,
            Group group, final List<String> verifiedEmails,
            final List<String> verifiedOpenIds
    ) {
        final Auth auth = new Auth();
        auth.setStudent(student);
        auth.setGroup(group);

        setupAuthCommon(req, auth, verifiedEmails, verifiedOpenIds);
    }

    protected void setupAuthCommon(
            final HttpServletRequest req,
            final Auth auth,
            final List<String> verifiedEmails,
            final List<String> verifiedOpenIds
    ) {
        auth.setSourceAddr(W.resolveRemoteAddress(req));
        auth.setVerifiedOpenIds(verifiedOpenIds);
        auth.setVerifiedEmails(verifiedEmails);

        req.getSession(true).setAttribute(Auth.SESSION_KEY, auth);
        req.getSession(true).setMaxInactiveInterval(
                (int) (elwServerConfig.getSessionExpiryMillis() / 1000)
        );
    }
}
