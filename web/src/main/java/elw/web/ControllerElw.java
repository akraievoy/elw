/*
 * ELW : e-learning workspace
 * Copyright (C) 2010  Anton Kraievoy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package elw.web;

import base.pattern.Result;
import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.io.ByteStreams;
import com.google.common.io.CharStreams;
import com.google.common.io.InputSupplier;
import elw.dao.*;
import elw.dao.Ctx;
import elw.miniweb.Message;
import elw.vo.*;
import elw.vo.Class;
import elw.web.core.Core;
import elw.web.core.W;
import elw.webauth.ControllerAuth;
import org.akraievoy.base.Parse;
import org.akraievoy.couch.Squab;
import org.apache.commons.fileupload.FileItemIterator;
import org.apache.commons.fileupload.FileItemStream;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;
import org.springframework.web.servlet.support.RequestContextUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import static elw.dao.Auth.SESSION_KEY;

public abstract class ControllerElw extends MultiActionController implements WebSymbols {
    private static final Logger log = LoggerFactory.getLogger(ControllerElw.class);

    private static final DiskFileItemFactory fileItemFactory = createFileItemFactory();

    protected final Core core;
    protected final ElwServerConfig elwServerConfig;

    protected ControllerElw(
            final Core core,
            final ElwServerConfig elwServerConfig
    ) {
        this.core = core;
        this.elwServerConfig = elwServerConfig;
    }

    private static DiskFileItemFactory createFileItemFactory() {
        DiskFileItemFactory fileItemFactory = new DiskFileItemFactory();
        fileItemFactory.setRepository(new java.io.File(System.getProperty("java.io.tmpdir")));
        fileItemFactory.setSizeThreshold(2 * 1024 * 1024);

        return fileItemFactory;
    }

    //  LATER move to ControllerAuth (required direct instance-level call dispatch)
    public HashMap<String, Object> noAuth(
            HttpServletRequest req,
            HttpServletResponse resp,
            boolean page,
            final String message
    ) throws IOException {
        if (page) {
            Message.addWarn(req, message);
            ControllerAuth.storeSuccessRedirect(req);
            resp.sendRedirect(elwServerConfig.getBaseUrl() + "auth.html");
        } else {
            resp.sendError(
                    HttpServletResponse.SC_FORBIDDEN,
                    message
            );
        }

        return null;
    }

    //  TODO abstract away from returning concrete hashmap class
    protected HashMap<String, Object> prepareDefaultModel(
            final HttpServletRequest req,
            final Auth auth,
            final Ctx ctx
    ) {
        final HashMap<String, Object> model = new HashMap<String, Object>();

        model.put(S_MESSAGES, Message.drainMessages(req));
        model.put(
                FormatTool.MODEL_KEY,
                FormatTool.forLocale(RequestContextUtils.getLocale(req))
        );
        model.put(VelocityTemplates.MODEL_KEY, core.getTemplates());
        model.put(ElwUri.MODEL_KEY, core.getUri());
        model.put(Auth.MODEL_KEY, auth);
        model.put(QueriesSecure.MODEL_KEY, core.getQueries().secure(auth));

        return model;
    }

    protected static Auth auth(HttpServletRequest req) {
        return (Auth) req.getSession(true).getAttribute(SESSION_KEY);
    }

    protected static Auth auth(Map<String, Object> model) {
        return (Auth) model.get(Auth.MODEL_KEY);
    }

    protected static QueriesSecure queries(Map<String, Object> model) {
        return (QueriesSecure) model.get(QueriesSecure.MODEL_KEY);
    }

    //  TODO better to avoid returning nulls and throw a specific exception
    protected HashMap<String, Object> auth(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            final boolean page,
            final boolean verified
    ) throws IOException {
        final HttpSession session = req.getSession(true);
        final Auth auth = (Auth) session.getAttribute(SESSION_KEY);

        if (auth == null) {
            return noAuth(req, resp, page, "Auth required");
        }
        
        if (!W.resolveRemoteAddress(req).equals(auth.getSourceAddr())) {
            return noAuth(req, resp, page, "Source address changed");
        }

        auth.renew(core.getQueries());

        if (auth.isEmpty()) {
            return noAuth(req, resp, page, "Non-empty Auth required");
        }

        if (verified && !auth.isVerified()) {
            return noAuth(req, resp, page, "Verified Auth required");
        }

        session.removeAttribute(ControllerAuth.SESSION_SUCCESS_REDIRECT);

        return prepareDefaultModel(req, auth, null);
    }

    protected static interface WebMethodScore {
        ModelAndView handleScore(
                HttpServletRequest req, HttpServletResponse resp,
                Ctx ctx, FileSlot slot, Solution file, Long stamp,
                Map<String, Object> model
        ) throws IOException;
    }

    //  FIXME some of file VT parameters are broken
    protected ModelAndView wmScore(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            final boolean page,
            final boolean verified,
            final WebMethodScore wm
    ) throws IOException {
        final HashMap<String, Object> model =
                auth(req, resp, page, verified);
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);
        if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
            return null;
        }

        final String slotId = req.getParameter("sId");
        if ((slotId == null || slotId.trim().length() == 0)) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
            return null;
        }

        final FileSlot slot = ctx.getAssType().getFileSlots().get(slotId);
        if (slot == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND, "slot for id " + slotId + " not found");
            return null;
        }

        final String fileId = req.getParameter("fId");
        if ((fileId == null || fileId.trim().length() == 0)) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no fileId (fId) defined");
            return null;
        }

        final Solution file = core.getQueries().solution(ctx.ctxSlot(slot), fileId);
        if (file == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND, "file for id " + fileId + " not found");
            return null;
        }

        final Long stamp = Parse.oneLong(req.getParameter("stamp"), null);

        return wm.handleScore(req, resp, ctx, slot, file, stamp, model);
    }

    protected static abstract class WebMethodCtx {
        protected HttpServletRequest req;
        protected HttpServletResponse resp;
        protected Ctx ctx;
        protected Map<String, Object> model;

        protected void init(
                HttpServletRequest req,
                HttpServletResponse resp,
                Ctx ctx,
                Map<String, Object> model
        ) {
            this.ctx = ctx;
            this.model = model;
            this.req = req;
            this.resp = resp;
        }

        public abstract ModelAndView handleCtx() throws IOException;
    }

    protected ModelAndView wmECG(
            HttpServletRequest req, HttpServletResponse resp,
            final boolean page,
            final boolean verified,
            final WebMethodCtx wm
    ) throws IOException {
        return wm(req, resp, Ctx.STATE_ECG, wm, page, verified);
    }

    private ModelAndView wm(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            final String ctxResolveState,
            final WebMethodCtx wm,
            final boolean page,
            final boolean verified
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, page, verified);
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);
        if (!ctx.resolved(ctxResolveState)) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
            return null;
        }

        wm.init(req, resp, ctx, model);
        return wm.handleCtx();
    }

    protected ModelAndView wmFile(
            final HttpServletRequest req,
            final HttpServletResponse resp,
            final String scopeForced,
            final boolean page,
            final boolean verified,
            final WebMethodFile wm
    ) throws IOException {
        final HashMap<String, Object> model = auth(req, resp, page, verified);
        if (model == null) {
            return null;
        }

        final Ctx ctx = (Ctx) model.get(R_CTX);

        wm.init(req, resp, ctx, model);
        wm.setScopeForced(scopeForced);

        return wm.handleCtx();
    }

    protected static abstract class WebMethodFile extends WebMethodCtx {
        protected String scopeForced = null;

        private void setScopeForced(String scopeForced) {
            this.scopeForced = scopeForced;
        }

        public ModelAndView handleCtx() throws IOException {
            final String scope = scopeForced == null ? req.getParameter("s") : scopeForced;
            if (scope == null) {
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "scope not set");
                return null;
            }

            if (Attachment.SCOPE.equals(scope)) {
                if (!ctx.resolved(Ctx.STATE_CTAV)) {
                    resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
                    return null;
                }
            } else if (Solution.SCOPE.equals(scope)) {
                if (!ctx.resolved(Ctx.STATE_EGSCIV)) {
                    resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
                    return null;
                }
            } else {
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "bad scope: " + scope);
                return null;
            }

            final String slotId = req.getParameter("sId");
            if (slotId == null || slotId.trim().length() == 0) {
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
                return null;
            }

            final FileSlot slot = ctx.getAssType().getFileSlots().get(slotId);
            if (slot == null) {
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "slot '" + slotId + "' not found");
                return null;
            }

            return handleFile(scope, slot);
        }

        protected abstract ModelAndView handleFile(String scope, FileSlot slot) throws IOException;

        protected void retrieveFile(
                FileBase fileBase, FileSlot slot, final Queries queries
        ) throws IOException {
            final InputSupplier<InputStream> fileInput;
            if (fileBase instanceof Solution) {
                fileInput = queries.solutionInput(
                        ctx.ctxSlot(slot).solution((Solution) fileBase),
                        FileBase.CONTENT
                );
            } else if (fileBase instanceof Attachment) {
                fileInput = queries.attachmentInput(
                        ctx.ctxSlot(slot).attachment((Attachment) fileBase),
                        FileBase.CONTENT
                );
            } else {
                throw new IllegalStateException(
                        "not supported fileBase instance: " + fileBase
                );
            }

            storeContentHeaders(fileBase, resp);
            ByteStreams.copy(fileInput, resp.getOutputStream());
        }

        public ModelAndView storeFile(
                FileSlot slot, String refreshUri, String failureUri, String authorName,
                Queries queries, final FileBase file
        ) throws IOException {
            //  FIXME upload page should contain full listing of validation rules
            final SortedMap<String, FileType> allTypes = slot.getFileTypes();
            final SortedMap<String, FileType> validTypes = new TreeMap<String, FileType>(allTypes);
            final boolean put = "PUT".equalsIgnoreCase(req.getMethod());
            final int length = req.getContentLength();

            file.setAuthor(authorName);

            InputSupplier<? extends InputStream> inputSupplier = null;
            String contentType = null;
            if (put) {
                //  FIXME remove this extra variant with non-encoded put uploads
                inputSupplier = supplierForRequest(req);
                contentType = "text/plain";
                //  urgent fix: code uploads collide on the name
                final String stamp = Long.toString(
                        Squab.Stamped.genStamp(), 36
                );
                file.setName("upload_" + stamp + ".txt");
            } else {
                try {
                    final ServletFileUpload sfu = new ServletFileUpload(fileItemFactory);
                    final FileItemIterator fii = sfu.getItemIterator(req);
                    while (fii.hasNext()) {
                        final FileItemStream item = fii.next();
                        if (item.isFormField()) {
                            final String fieldName = item.getFieldName();
                            if ("comment".equals(fieldName)) {
                                file.setComment(fieldText(item));
                            }
                            if (auth(req).getAdmin() != null) {
                                if ("sourceAddr".equals(fieldName)) {
                                    final String sourceAddr = fieldText(item);
                                    if (!Strings.isNullOrEmpty(sourceAddr)) {
                                        file.setSourceAddress(sourceAddr);
                                    }
                                } else if ("dateTime".equals(fieldName)) {
                                    final String dateTime = fieldText(item);
                                    if (!Strings.isNullOrEmpty(dateTime)) {
                                        final String[] parts =
                                            dateTime.trim().split("\\s+");

                                        final String date = parts[0];
                                        final String time = 
                                            parts.length > 1 ? parts[1] : "11:00";

                                        final DateTime instant =
                                            Class.parseDateTime(date, time);

                                        file.setStamp(instant.getMillis());
                                    }
                                }
                            }
                            continue;
                        }

                        file.setName(Strings.nullToEmpty(extractNameFromPath(item)));
                        contentType = item.getContentType();
                        inputSupplier = supplierForFileItem(item);
                        //  TODO find length of this particular item, if implied by protocol
                        break;
                    }
                } catch (FileUploadException e) {
                    throw new IOException(e);
                }
                if (inputSupplier == null) {
                    return fail(put, failureUri, "File being uploaded not found in the form");
                }
                if (contentType == null) {
                    return fail(put, failureUri, "Content Type not reported in upload");
                }
            }

            if (Strings.isNullOrEmpty(file.getSourceAddress())) {
                file.setSourceAddress(W.resolveRemoteAddress(req));
            }
            if (length == -1) {
                final String message = "Upload size not reported";
                return fail(put, failureUri, message);
            }
            FileType._.filterByLength(validTypes, length);
            if (validTypes.isEmpty()) {
                return fail(put, failureUri, "Size " + org.akraievoy.base.Format.formatMem(length) + " exceeds size limits");
            }
            FileType._.filterByName(validTypes, file.getName().toLowerCase());
            if (validTypes.isEmpty()) {
                return fail(put, failureUri, "File name failed all regex checks");
            }
            if (validTypes.size() > 1) {
                Message.addWarn(req, "More than one valid file type left: " + validTypes.keySet());
            }
            final FileType fileType = validTypes.get(validTypes.firstKey());
            if (!fileType.getContentTypes().contains(Strings.nullToEmpty(contentType))) {
                Message.addWarn(req, "contentType '" + contentType + "': not listed in the file type");
            }
            file.setFileType(IdNamed._.singleton(fileType));
            if (!fileType.isBinary()) {
                if (length > FileBase.DETECT_SIZE_LIMIT) {
                    return fail(put, failureUri, "Non-binary file is too big for content check");
                }
                final byte[] bytes = ByteStreams.toByteArray(inputSupplier);
                //	implemented as per this SO answer:
                //	http://stackoverflow.com/questions/277521/identify-file-binary/277568#277568
                for (byte b : bytes) {
                    if (b >= 0 && b < 9 || b > 13 && b < 32) {
                        return fail(put, failureUri, "Non-binary file contains binary data");
                    }
                }
                inputSupplier = ByteStreams.newInputStreamSupplier(bytes);
            }

            //  TODO validate headers and binary content here

            final Result result = queries.createFile(ctx, slot, file, inputSupplier, contentType);
            if (result.isSuccess()) {
                return succeed(put, refreshUri, result);
            } else {
                return fail(put, failureUri, result.getMessage());
            }
        }

        protected ModelAndView succeed(boolean put, String refreshUri, Result result) throws IOException {
            if (!put) {
                Message.addResult(req, result);
                resp.sendRedirect(refreshUri);
            }
            return null;
        }

        protected ModelAndView fail(boolean put, String failureUri, String message) throws IOException {
            if (put) {
                resp.sendError(HttpServletResponse.SC_BAD_REQUEST, message);
            } else {
                Message.addWarn(req, message);
                resp.sendRedirect(failureUri);
            }
            return null;
        }

    }

    public static void storeContentHeaders(
            final FileBase fileBase,
            final HttpServletResponse resp
    ) {
        final FileType fileType =
                IdNamed._.one(fileBase.getFileType());
        final String contentType;
        if (!fileType.getContentTypes().isEmpty()) {
            contentType =
                    fileType.getContentTypes().get(0);
        } else {
            contentType =
                    fileType.isBinary() ? "binary/octet-stream" : "text/plain";
        }

        if (fileType.isBinary()) {
            resp.setContentType(contentType);
        } else {
            resp.setContentType(contentType + "; charset=UTF-8");
            resp.setCharacterEncoding("UTF-8");
        }

        resp.setContentLength(
                fileBase.getCouchFile(FileBase.CONTENT).getLength().intValue()
        );
        resp.setHeader("Content-Disposition", "attachment;");
    }

    protected static String fieldText(
            final FileItemStream item
    ) throws IOException {
        return CharStreams.toString(
            CharStreams.newReaderSupplier(
                supplierForFileItem(item),
                Charsets.UTF_8
            )
        );
    }

    protected static String extractNameFromPath(FileItemStream item) {
        final String name = item.getName();
        if (name == null) {
            return null;
        }

        final int lastSlash =
                Math.max(name.lastIndexOf("\\"), name.lastIndexOf("/"));
        final String fName =
                lastSlash >= 0 ? name.substring(lastSlash + 1) : name;

        return fName;
    }

    protected static InputSupplier<InputStream> supplierForRequest(
            final HttpServletRequest myReq
    ) {
        return new InputSupplier<InputStream>() {
            public InputStream getInput() throws IOException {
                return myReq.getInputStream();
            }
        };
    }

    protected static InputSupplier<InputStream> supplierForFileItem(
            final FileItemStream item
    ) {
        return new InputSupplier<InputStream>() {
            public InputStream getInput() throws IOException {
                return item.openStream();
            }
        };
    }
}
