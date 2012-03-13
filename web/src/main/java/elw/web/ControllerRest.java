package elw.web;

import com.google.common.base.Strings;
import com.google.common.io.ByteStreams;
import com.google.common.io.InputSupplier;
import elw.dao.Queries;
import elw.dao.QueriesSecure;
import elw.dao.ctx.CtxSlot;
import elw.dao.ctx.CtxSolution;
import elw.dao.rest.RestEnrollment;
import elw.dao.rest.RestEnrollmentSummary;
import elw.dao.rest.RestSolution;
import elw.miniweb.ViewJackson;
import elw.vo.*;
import elw.web.core.Core;
import elw.web.core.W;
import org.akraievoy.gear.G4mat;
import org.apache.commons.fileupload.FileItemIterator;
import org.apache.commons.fileupload.FileItemStream;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/*
* /auth    //  LATER specify PUT for admin impersonation
* /auths    //  LATER specify
*
* /challenges //   LATER specify
*
*
* // TODO auth
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
*
* TODO: SCORES
*   get last score for solution
*   get all scores for solution
*   list solutions of the same version across the sustem
*   get reference to next checked solution: switching strategies
*
* LATER: COMMENTS
*/
@Controller
@RequestMapping("/rest/**/*")
public class ControllerRest extends ControllerElw {
    public static enum ListStyle{IDS, MAP}

    public static final String SESSION_AUTH = "elw_auth";

    public static final String MODEL_AUTH = SESSION_AUTH;
    public static final String MODEL_QUERIES = "elw_queries";

    private static final Logger log =
            LoggerFactory.getLogger(ControllerRest.class);
    private static final DiskFileItemFactory fileItemFactory
            = createFileItemFactory();

    private final boolean devMode;

    private long testAuthSwitch = 0;

    public ControllerRest(Core core) {
        super(core);

        //  LATER devMode inferencing booster
        devMode = "w".equals(System.getProperty("user.name"));
    }

    private static DiskFileItemFactory createFileItemFactory() {
        final DiskFileItemFactory fileItemFactory =
                new DiskFileItemFactory();

        fileItemFactory.setRepository(
                new java.io.File(System.getProperty("java.io.tmpdir"))
        );

        fileItemFactory.setSizeThreshold(1 * 1024 * 1024);

        return fileItemFactory;
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

    //  TODO this quite likely is ok to be admin-only
    //  TODO otherwise it should be proxied via clone and filtered for students
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
            value = "enrollment/{enrId}/scores",
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

        final Map<String, RestSolution> enrSolutions =
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

    //  Regex is required here: solId may contain '.'
    //  http://stackoverflow.com/questions/3526523/spring-mvc-pathvariable-getting-truncated
    @RequestMapping(
            value = "enrollment/{enrId}/solution/{solId:.+}",
            method = RequestMethod.GET
    )
    public ModelAndView do_enrollmentSolutionGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("enrId") final String enrId,
            @PathVariable("solId") final String solId
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final RestSolution solution =
                queries.restSolution(enrId, solId, null);
        
        if (solution == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
            return null;
        }

        return new ModelAndView(ViewJackson.data(solution));
    }

    //  PUTs are not supported for HTML file uploads
    //      http://stackoverflow.com/questions/812711/how-do-you-do-an-http-put
    //      http://stackoverflow.com/questions/2006900/browser-based-webdav-client
    //  but there's hope they would be someday
    //      https://www.w3.org/Bugs/Public/show_bug.cgi?id=10671
    @RequestMapping(
            value = "enrollment/{enrId}/solution/{solId:.+}",
            method = RequestMethod.POST
    )
    public ModelAndView do_enrollmentSolutionPost(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("enrId") final String enrId,
            @PathVariable("solId") final String solId
    ) throws IOException, FileUploadException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);
        final QueriesSecure.Auth auth =
                (QueriesSecure.Auth) model.get(MODEL_AUTH);

        final Queries.CtxResolutionState stateSlot =
                queries.resolveSlot(enrId, solId, null);
        
        if (!stateSlot.complete()) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST);
            return null;
        }
        
        final CtxSlot ctxSlot = stateSlot.ctxSlot;

        final SortedMap<String, FileType> allTypes =
                ctxSlot.slot.getFileTypes();
        final SortedMap<String, FileType> validTypes =
                new TreeMap<String, FileType>(allTypes);

        //  we may safely assume order over the elements sent
        //      http://stackoverflow.com/questions/7449861/multipart-upload-form-is-order-guaranteed
        //  see the w3 spec
        //      http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4

        //  not used for streaming/api calls but
        //      still usable for content detection
        final int length = req.getContentLength();

        final Solution solution = new Solution();
        solution.setAuthor(auth.getName());
        solution.setSourceAddress(W.resolveRemoteAddress(req));

        final ServletFileUpload sfu =
                new ServletFileUpload(fileItemFactory);
        final FileItemIterator fii = 
                sfu.getItemIterator(req);

        if (!fii.hasNext()) {
            //  here we'll have to employ XHR and
            //      http status message parsing
            //  http://api.jquery.com/jQuery.ajax/
            //      esp. see the error(jqXHR, textStatus, errorThrown)
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "No upload fields found"
            );
            return null;
        }

        final FileItemStream comm = fii.next();
        if (!comm.isFormField() || !"comment".equals(comm.getFieldName())) {
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "No comment with your upload"
            );
            return null;
        }
        solution.setComment(fieldText(comm));

        if (!fii.hasNext()) {
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "No file stream field found"
            );
            return null;
        }
        final FileItemStream file = fii.next();
        if (file.isFormField()) {
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "Not a file stream field as expected"
            );
            return null;
        }

        solution.setName(Strings.nullToEmpty(extractNameFromPath(file)));
        
        final String contentType = Strings.nullToEmpty(file.getContentType());
        InputSupplier<InputStream> inputSupplier =
                supplierForFileItem(file);

        FileType._.filterByLength(validTypes, length);
        if (validTypes.isEmpty()) {
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "Size " + G4mat.formatMem(length) + " exceeds size limits"
            );
            return null;
        }

        FileType._.filterByName(validTypes, solution.getName().toLowerCase());
        if (validTypes.isEmpty()) {
            resp.sendError(
                    HttpServletResponse.SC_BAD_REQUEST,
                    "File name failed all regex checks"
            );
            return null;
        }

        if (validTypes.size() > 1) {
            log.warn(
                    "More than one valid file type: {} yields {}",
                    solution.getName(),
                    validTypes.keySet()
            );
        }

        final FileType fileType = validTypes.get(validTypes.firstKey());
        if (!fileType.getContentTypes().contains(contentType)) {
            log.warn(
                    "contentType {} not listed in the file type",
                    contentType,
                    fileType.getId()
            );
        }
        solution.setFileType(IdNamed._.singleton(fileType));

        if (!fileType.isBinary()) {
            if (length > FileBase.DETECT_SIZE_LIMIT) {
                resp.sendError(
                        HttpServletResponse.SC_BAD_REQUEST,
                        "Non-binary file is too big for content check"
                );
                return null;
            }

            final byte[] bytes = ByteStreams.toByteArray(inputSupplier);
            //	implemented as per this SO answer:
            //	http://stackoverflow.com/questions/277521/identify-file-binary/277568#277568
            for (byte b : bytes) {
                if (b >= 0 && b < 9 || b > 13 && b < 32) {
                    resp.sendError(
                            HttpServletResponse.SC_BAD_REQUEST,
                            "Non-binary file contains binary data"
                    );
                    return null;
                }
            }
            final InputSupplier<? extends InputStream> baisSupplier =
                    ByteStreams.newInputStreamSupplier(bytes);
            //noinspection unchecked
            inputSupplier = (InputSupplier<InputStream>) baisSupplier;
        } else {
            //  LATER validate binary headers of content here
        }

        //  TODO there's no simple harness for this method
        //      maybe there's a way to trigger this one properly with curl?
        boolean created = queries.createSolution(
                ctxSlot, solution,
                contentType, inputSupplier
        );
        if (!created) {
            resp.sendError(
                    HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Failed to save the file"
            );
        }

        return null;
    }

    @RequestMapping(
            value = "enrollment/{enrId}/solution/{solId:.+}/{fileName:.+}",
            method = RequestMethod.GET
    )
    public ModelAndView do_enrollmentSolutionGet(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            @PathVariable("enrId") final String enrId,
            @PathVariable("solId") final String solId,
            @PathVariable("fileName") final String fileName
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, null);
        if (model == null) {
            return null;
        }

        final Queries queries =
                (Queries) model.get(MODEL_QUERIES);

        final CtxSolution ctxSolution =
                queries.resolveSolution(enrId, solId, null);

        if (ctxSolution == null) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST);
            return null;
        }

        final InputSupplier<InputStream> contentSupplier =
                queries.solutionInput(ctxSolution, FileBase.CONTENT);

        //  LATER ideally we should check that fileName is not empty and
        //      conforms to restrictions of its content type / file type

        if (contentSupplier != null) {
            storeContentHeaders(ctxSolution.solution, resp);
            ByteStreams.copy(contentSupplier, resp.getOutputStream());
        } else {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
        }

        return null;
    }

    //  not currently used, but may be useful for some of streaming requests
    //  http://stackoverflow.com/questions/3686808/spring-3-requestmapping-get-path-value
}
