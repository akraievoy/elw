package elw.web;

import elw.dao.*;
import elw.dao.rest.RestEnrollment;
import elw.dao.rest.RestEnrollmentSummary;
import elw.miniweb.ViewJackson;
import elw.vo.Course;
import elw.vo.Group;
import elw.vo.Solution;
import elw.web.core.Core;
import elw.web.core.W;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.*;

/*
* /auth    //  LATER specify PUT for admin impersonation
* /auths    //  LATER specify
*
* /challenges //   LATER specify
*
* /challenge
*   GET /iasa@akraievoy.org/13989823443
*      -> 200 {"status": "response sent", "challenge": "SHA512", "expiry": 13989823443} (response sent to email)
*   GET /iasa@akraievoy.org/13989823443
*   -> 400 {"status": "failed to send response"}
*   GET /iasa@akraievoy.org/13989823443
*   -> 400 {"status": "flood/bruteforce protection", "expiry": 139898298989}
*   POST /iasa@akraievoy.org {"challenge": "SHA512", "expiry": 13989823443, "response": "SHA512"}
*      -> 200 {"status": "logged in"}
*      -> 400 {"status": "challenge failed"}
*      challenge = SHA512(salt, email, millis)
*      response = SHA512(salt, email, sessionID, request.sourceAddr, expiry, challenge)
*
* {{{
* /enrollment/enrId/files
*   GET /
*      -> {}
* /enrollment/enrId/solutions
*   GET /
*   PUT /
* }}}
*
* FIXME: SCORES
*
* FIXME: COMMENTS
*/
@Controller
@RequestMapping("/rest/**/*")
public class ControllerRest extends ControllerElw {
    public static enum ListStyle{IDS, MAP}

    public static final String SESSION_AUTH = "elw_auth";

    public static final String MODEL_AUTH = SESSION_AUTH;
    public static final String MODEL_QUERIES = "elw_queries";

    private final boolean devMode;
    private long testAuthSwitch = 0;

    public ControllerRest(Core core) {
        super(core);

        //  LATER devMode inferencing booster
        devMode = "w".equals(System.getProperty("user.name"));
    }

    @InitBinder
    public void initBinder(WebDataBinder binder) {
        binder.registerCustomEditor(
                ListStyle.class,
                new EnumPropertyEditor(ListStyle.values())
        );
    }

    @Override
    protected HashMap<String, Object> auth(
            HttpServletRequest req,
            HttpServletResponse resp,
            String pathToRoot
    ) throws IOException {
        final HttpSession session = req.getSession(true);

        QueriesSecure.Auth auth = (QueriesSecure.Auth) session.getAttribute(SESSION_AUTH);
        QueriesSecure queriesSecure = null;

        //  FIXME this shall be true for nginx-proxied remote requests
        boolean loopback = "127.0.0.1".equals(req.getRemoteAddr());
        if (devMode && loopback) {
            auth = new QueriesSecure.Auth();
            queriesSecure = core.getQueries().secure(auth);

            long testAuth = testAuthSwitch++;
            if (testAuth % 3 == 0) {
                auth.setId("iasa@akraievoy.org");
                auth.setName("AK");
                auth.setRoles(Collections.singletonList(QueriesSecure.Auth.ROLE_ADM));
                auth.setOnSite(false);
            } else if (testAuth % 3 == 1) {
                auth.setId("vasya@poopkeen.org");
                auth.setName("Vuneedlo");
                auth.setRoles(Collections.singletonList(QueriesSecure.Auth.ROLE_STUD));
                auth.setOnSite(true);
            } else if (testAuth % 3 == 2) {
                auth.setId("elw@akraievoy.org");
                auth.setName("anon");
                auth.setRoles(Collections.singletonList(QueriesSecure.Auth.ROLE_GUEST));
                auth.setOnSite(null);
            }

            auth.setExpiry(System.currentTimeMillis() + 30 * 60 * 1000);
            auth.setSourceAddr(req.getRemoteAddr());

            auth.setGroupIds(queriesSecure.groupIds());
            auth.setEnrIds(queriesSecure.enrollmentIds());
            auth.setCourseIds(queriesSecure.courseIds());

            session.setAttribute(SESSION_AUTH, auth);
        }

        if (auth == null) {
            resp.sendError(
                    HttpServletResponse.SC_NOT_IMPLEMENTED,
                    "Nope: auth not yet implemented. Better luck next time."
            );
            return null;
        }

        if (queriesSecure == null) {
            queriesSecure = core.getQueries().secure(auth);
        }

        final HashMap<String, Object> model = prepareDefaultModel(req);

        model.put(MODEL_AUTH, auth);
        model.put(MODEL_QUERIES, queriesSecure);

        return model;
    }

    @RequestMapping(
            value = "auth",
            method = RequestMethod.GET
    )
    public ModelAndView do_authGet(
            final HttpServletRequest req,
            final HttpServletResponse resp
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        return new ModelAndView(ViewJackson.data(model.get(MODEL_AUTH)));
    }

    @RequestMapping(
            value = "courses/{listStyle}",
            method = RequestMethod.GET
    )
    public ModelAndView do_coursesGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("listStyle") final ListStyle listStyle
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final List<String> courseIds = queries.courseIds();

        if (ListStyle.IDS == listStyle) {
            return new ModelAndView(ViewJackson.data(courseIds));
        }

        final Map<String, Course> courses =
                new TreeMap<String, Course>();
        for (String courseId : courseIds) {
            courses.put(courseId, queries.course(courseId));
        }

        return new ModelAndView(ViewJackson.data(courses));
    }

    @RequestMapping(
            value = "course/{courseId}",
            method = RequestMethod.GET
    )
    public ModelAndView do_courseGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("courseId") final String courseId
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final Course course = queries.course(courseId);

        if (course == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
            return null;
        }

        return new ModelAndView(ViewJackson.data(course));
    }

    @RequestMapping(
            value = "groups/{listStyle}",
            method = RequestMethod.GET
    )
    public ModelAndView do_groupsGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("listStyle") final ListStyle listStyle
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final List<String> groupIds = queries.groupIds();

        if (ListStyle.IDS == listStyle) {
            return new ModelAndView(ViewJackson.data(groupIds));
        }

        final Map<String, Group> groups =
                new TreeMap<String, Group>();
        for (String groupId : groupIds) {
            groups.put(groupId, queries.group(groupId));
        }

        return new ModelAndView(ViewJackson.data(groups));
    }

    @RequestMapping(
            value = "group/{groupId}",
            method = RequestMethod.GET
    )
    public ModelAndView do_groupGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("groupId") final String groupId
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final Group group = queries.group(groupId);

        if (group == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
            return null;
        }

        return new ModelAndView(ViewJackson.data(group));
    }

    @RequestMapping(
            value = "enrollments/{listStyle}",
            method = RequestMethod.GET
    )
    public ModelAndView do_enrollmentsGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("listStyle") final ListStyle listStyle
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final List<String> enrIds = queries.enrollmentIds();

        if (ListStyle.IDS == listStyle) {
            return new ModelAndView(ViewJackson.data(enrIds));
        }

        final Map<String, RestEnrollment> enrollments =
                new TreeMap<String, RestEnrollment>();
        for (String enrId : enrIds) {
            final RestEnrollment restEnrollment =
                    queries.restEnrollment(enrId, W.resolveRemoteAddress(req));
            enrollments.put(enrId, restEnrollment);
        }

        return new ModelAndView(ViewJackson.data(enrollments));
    }

    @RequestMapping(
            value = "enrollment/{enrId}",
            method = RequestMethod.GET
    )
    public ModelAndView do_enrollmentGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("enrId") final String enrId
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final RestEnrollment restEnrollment = 
                queries.restEnrollment(enrId, W.resolveRemoteAddress(req));

        if (restEnrollment == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
            return null;
        }

        return new ModelAndView(ViewJackson.data(restEnrollment));
    }

    @RequestMapping(
            value = "enrollment/{enrId}/scoring",
            method = RequestMethod.GET
    )
    public ModelAndView do_enrollmentScoringGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("enrId") final String enrId
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final RestEnrollmentSummary enrSummary =
                queries.restScores(enrId, null);

        if (enrSummary == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
            return null;
        }

        return new ModelAndView(ViewJackson.data(enrSummary));
    }

    @RequestMapping(
            value = "enrollment/{enrId}/solutions/{listStyle}",
            method = RequestMethod.GET
    )
    public ModelAndView do_enrollmentSolutionsGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("enrId") final String enrId,
            @PathVariable("listStyle") final ListStyle listStyle
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final elw.dao.SolutionFilter filter = RestSolutionFilter.fromRequest(req);
        if (filter == null) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST);
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final Map<String, Solution> enrSolutions =
                queries.restSolutions(enrId, filter);

        if (enrSolutions == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
            return null;
        }

        if (listStyle == ListStyle.IDS) {
            return new ModelAndView(ViewJackson.data(enrSolutions.keySet()));
        }

        return new ModelAndView(ViewJackson.data(enrSolutions));
    }

    //  http://stackoverflow.com/questions/3686808/spring-3-requestmapping-get-path-value

}
