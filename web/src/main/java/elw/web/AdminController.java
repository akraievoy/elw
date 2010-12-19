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
import elw.dao.*;
import elw.dp.mips.MipsValidator;
import elw.miniweb.Message;
import elw.miniweb.ViewJackson;
import elw.vo.*;
import elw.web.core.Core;
import elw.web.core.LogFilter;
import elw.web.core.W;
import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Parse;
import org.akraievoy.gear.G4Str;
import org.akraievoy.gear.G4mat;
import org.apache.commons.fileupload.FileItemIterator;
import org.apache.commons.fileupload.FileItemStream;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

@Controller
@RequestMapping("/a/**/*")
public class AdminController extends ControllerElw {
	private static final Logger log = LoggerFactory.getLogger(AdminController.class);
	
	protected final CourseDao courseDao;
	protected final GroupDao groupDao;
	protected final EnrollDao enrollDao;
	protected final ScoreDao scoreDao;
	protected final FileDao fileDao;

	protected final AdminDao adminDao;
	protected final SortedSet<String> validNonces = new TreeSet<String>();

	protected final ObjectMapper mapper = new ObjectMapper();
	protected final long cacheBustingToken = System.currentTimeMillis();

	protected final DiskFileItemFactory fileItemFactory;

	public AdminController(
			CourseDao courseDao, EnrollDao enrollDao, GroupDao groupDao, ScoreDao scoreDao, FileDao fileDao,
			AdminDao adminDao,
			Core core
	) {
		super(core);

		this.courseDao = courseDao;
		this.enrollDao = enrollDao;
		this.groupDao = groupDao;
		this.scoreDao = scoreDao;
		this.fileDao = fileDao;

		this.adminDao = adminDao;

		fileItemFactory = new DiskFileItemFactory();
		fileItemFactory.setRepository(new java.io.File(System.getProperty("java.io.tmpdir")));
		fileItemFactory.setSizeThreshold(2 * 1024 * 1024);
	}

	protected HashMap<String, Object> auth(final HttpServletRequest req, final HttpServletResponse resp, final String pathToRoot) throws IOException {
		final HttpSession session = req.getSession(true);
		final Admin admin = (Admin) session.getAttribute(S_ADMIN);

		if (admin == null) {
			if (pathToRoot != null) {
				Message.addWarn(req, "Admin authentication required");
				if (req.getQueryString() != null) {
					session.setAttribute("loginTo", req.getRequestURI() + "?" + req.getQueryString());
				} else {
					session.setAttribute("loginTo", req.getRequestURI());
				}
				resp.sendRedirect(pathToRoot + "login");
			} else {
				resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Admin authentication required");
			}

			return null;
		}

		session.removeAttribute("loginTo");	//	LATER extract constant

		final HashMap<String, Object> model = prepareDefaultModel(req);
		model.put(S_ADMIN, admin);
		model.put(R_CTX, Ctx.fromString(req.getParameter(R_CTX)).resolve(enrollDao, groupDao, courseDao));

		return model;
	}

	@RequestMapping(value = "login", method = RequestMethod.GET)
	public ModelAndView do_login(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = prepareDefaultModel(req);

		final String nonce = Long.toString(System.currentTimeMillis(), 36);
		synchronized (validNonces) {
			validNonces.add(nonce);
		}
		model.put("nonce", nonce);

		return new ModelAndView("a/login", model);
	}

	@RequestMapping(value = "loginPost", method = RequestMethod.POST)
	public ModelAndView do_loginPost(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final String login = req.getParameter("login");
		final String nonce = req.getParameter("nonce");
		final String hash = req.getParameter("hash");

		final HttpSession session = req.getSession(true);
		if (
				nonce == null || nonce.trim().length() == 0 ||
				hash == null || hash.trim().length() == 0 ||
				login == null || login.trim().length() == 0
		) {
			Message.addWarn(req, "nonce/hash parameters NOT set");
		} else {
			boolean nonceValid;
			synchronized (validNonces) {
				nonceValid = validNonces.remove(nonce);
				validNonces.headSet(nonce).clear();
			}

			if (nonceValid) {
				final Admin admin = adminDao.findAdminById(login);
				final String hashExpected;
				if (admin != null) {
					hashExpected = Ctx.digest(nonce + "-->" + login + "-->" + admin.getPassword());
				} else {
					hashExpected = null;
				}

				if (hashExpected != null && hashExpected.equalsIgnoreCase(hash)) {
					session.setAttribute(S_ADMIN, admin);
					session.setMaxInactiveInterval(300);
					Message.addInfo(req, "Admin area : logged on");
					final Object loginToAttr = session.getAttribute("loginTo");
					if (loginToAttr instanceof String) {
						session.removeAttribute("loginTo");
						resp.sendRedirect((String) loginToAttr);
					} else {
						resp.sendRedirect("index");
					}
					return null;
				} else {
					Message.addWarn(req, "terribly wrong login/password, please retry");
				}
			} else {
				Message.addWarn(req, "nonce expired, please retry authentication");
			}
		}

		resp.sendRedirect("login");
		return null;
	}

	@RequestMapping(value = "logout", method = RequestMethod.GET)
	public ModelAndView do_logout(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		req.getSession(true).invalidate();
		resp.sendRedirect("index");
		return null;
	}

	@RequestMapping(value = "index", method = RequestMethod.GET)
	public ModelAndView do_index(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		return new ModelAndView("a/index", model);
	}

	@RequestMapping(value = "courses", method = RequestMethod.GET)
	public ModelAndView do_courses(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		model.put("courses", courseDao.findAllCourses());

		return new ModelAndView("a/courses", model);
	}

	@RequestMapping(value = "course", method = RequestMethod.GET)
	public ModelAndView do_course(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_C)) {
			Message.addWarn(req, "Course not found");
			resp.sendRedirect("courses");
			return null;
		}

		if ("true".equalsIgnoreCase(req.getParameter("test"))) {
			final HashMap<String, Result> testResults = new HashMap<String, Result>();
			final MipsValidator validator = new MipsValidator();
			for (AssignmentType assType : ctx.getCourse().getAssTypes()) {
				for (Assignment ass : assType.getAssignments()) {
					for (Version ver : ass.getVersions()) {
						final Result[] resRef = {new Result("unknown", false)};
/*
						validator.batch(resRef, ver, ver.getSolution(), null);
*/
						testResults.put(
								ctx.extendTAV(assType, ass, ver).toString(),
								resRef[0]
						);
					}
				}
			}

			model.put("testResults", testResults);
		}

		return new ModelAndView("a/course", model);
	}

	@RequestMapping(value = "launch", method = RequestMethod.GET)
	public ModelAndView do_launch(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_CIV)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final StringWriter verSw = new StringWriter();
		mapper.writeValue(verSw, ctx.getVer());

		//	LATER use HTTP instead of applet parameter for passing the problem/code to applet
		model.put("verJson", verSw.toString().replaceAll("&", "&amp;").replaceAll("\"", "&quot;"));
		model.put("upHeader", "JSESSIONID=" + req.getSession(true).getId());
		model.put("cacheBustingToken", cacheBustingToken);

		return new ModelAndView("a/launch", model);
	}

	@RequestMapping(value = "enrolls", method = RequestMethod.GET)
	public ModelAndView do_enrolls(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Enrollment[] enrolls = enrollDao.findAllEnrollments();
		final Map<String, Group> groups = new HashMap<String, Group>();
		final Map<String, Course> courses = new HashMap<String, Course>();
		for (Enrollment enr : enrolls) {
			groups.put(enr.getGroupId(), groupDao.findGroup(enr.getGroupId()));
			courses.put(enr.getCourseId(), courseDao.findCourse(enr.getCourseId()));
		}
		model.put("enrolls", enrolls);
		model.put("groups", groups);
		model.put("courses", courses);

		return new ModelAndView("a/enrolls", model);
	}

	@RequestMapping(value = "summary", method = RequestMethod.GET)
	public ModelAndView do_summary(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		return wmECG(req, resp, "", new WebMethodCtx() {
			@Override public ModelAndView handleCtx() throws IOException {
				W.storeFilter(req, model);
				W.filterDefault(model, "f_mode", "a");
				final LogFilter filter = W.parseFilter(req);

				final SortedMap<String, Map<String, List<Entry<FileMeta>>>> fileMetas = new TreeMap<String, Map<String, List<Entry<FileMeta>>>>();
				final Map<String, Double> ctxToScore = new HashMap<String, Double>();

				core.tasksData(ctx, filter, true, fileMetas, ctxToScore, null);

				model.put("fileMetas", fileMetas);
				model.put("ctxToScore", ctxToScore);
				model.put("totalBudget", ctx.getEnr().cmpTotalBudget());

				return new ModelAndView("a/summary", model);
			}
		});
	}

	@RequestMapping(value = "log", method = RequestMethod.GET)
	public ModelAndView do_log(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_ECG)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
			return null;
		}

		W.storeFilter(req, model);

		return new ModelAndView("a/log", model);
	}

	@RequestMapping(value = "rest/log", method = RequestMethod.GET)
	public ModelAndView do_restLog(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, null);
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (!ctx.resolved(Ctx.STATE_ECG)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Path problem, please check the logs");
			return null;
		}

		final Format format = (Format) model.get(FormatTool.MODEL_KEY);

		final List<Object[]> logData = core.log(
				ctx, format, W.parseFilter(req), true
		);

		return new ModelAndView(ViewJackson.success(logData));
	}

	@RequestMapping(value = "tasks", method = RequestMethod.GET)
	public ModelAndView do_tasks(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		return wmECG(req, resp, "", new WebMethodCtx() {
			public ModelAndView handleCtx() throws IOException {
				W.storeFilter(req, model);
				return new ModelAndView("a/tasks", model);
			}
		});
	}

	@RequestMapping(value = "rest/tasks", method = RequestMethod.GET)
	public ModelAndView do_restTasks(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		return wmECG(req, resp, null, new WebMethodCtx() {
			public ModelAndView handleCtx() throws IOException {
				final Format format = (Format) model.get(FormatTool.MODEL_KEY);

				final List<Object[]> logData = core.tasks(
						ctx, W.parseFilter(req), format, true
				);

				return new ModelAndView(ViewJackson.success(logData));
			}
		});
	}

	@RequestMapping(value = "rest/scoreLog", method = RequestMethod.GET)
	public ModelAndView do_restScoreLog(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		return wmScore(req, resp, null, new WebMethodScore() {
			public ModelAndView handleScore(
					HttpServletRequest req, HttpServletResponse resp, Ctx ctx, FileSlot slot, Entry<FileMeta> file, Stamp stamp, Map<String, Object> model
			) {
				final Format f = (Format) model.get(FormatTool.MODEL_KEY);

				final ScoreDao scoreDaoLocal = scoreDao;
				final SortedMap<Stamp, Entry<Score>> allScores = scoreDaoLocal.findAllScoresAuto(ctx, slot, file);

				final String mode = req.getParameter("f_mode");

				final List<Object[]> logData = core.logScore(allScores, ctx, slot, file, f, mode, stamp);

				return new ModelAndView(ViewJackson.success(logData));
			}
		});
	}

	@RequestMapping(value = "rest/index", method = RequestMethod.GET)
	public ModelAndView do_restIndex(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, null);
		if (model == null) {
			return null;
		}

		final Enrollment[] enrolls = enrollDao.findAllEnrollments();

		final List<Object[]> indexData = core.index(enrolls);

		return new ModelAndView(ViewJackson.success(indexData));
	}

	@RequestMapping(value = "approve", method = RequestMethod.GET)
	public ModelAndView do_approve(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		return wmScore(req, resp, "", new WebMethodScore() {
			public ModelAndView handleScore(
					HttpServletRequest req, HttpServletResponse resp, Ctx ctx, FileSlot slot, Entry<FileMeta> file, Stamp stamp, Map<String, Object> model
			) {
				final SortedMap<Stamp, Entry<Score>> allScores = scoreDao.findAllScores(ctx, slot.getId(), file.getMeta().getId());

				final Entry<Score> lastScoreEntry = allScores.isEmpty() ? null : allScores.get(allScores.lastKey());
				final Entry<Score> scoreEntry = stamp == null ? lastScoreEntry : allScores.get(stamp);
				final Score score = ScoreDao.updateAutos(ctx, slot.getId(), file, scoreEntry == null ? null : scoreEntry.getMeta());

				model.put("stamp", stamp);
				model.put("score", score);
				model.put("slot", slot);
				model.put("file", file.getMeta());

				return new ModelAndView("a/approve", model);
			}
		});
	}

	@RequestMapping(value = "approve", method = RequestMethod.POST)
	public ModelAndView do_approvePost(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		return wmScore(req, resp, "", new WebMethodScore() {
			public ModelAndView handleScore(
					HttpServletRequest req, HttpServletResponse resp, Ctx ctx, FileSlot slot, Entry<FileMeta> file, Stamp stamp, Map<String, Object> model
			) throws IOException {
				final Score scoreByStamp;
				final String fileId = file.getMeta().getId();
				if (stamp == null) {
					scoreByStamp = null;
				} else {
					scoreByStamp = scoreDao.findScoreByStamp(ctx, stamp, slot.getId(), fileId).getMeta();
				}
				final Score score = ScoreDao.updateAutos(ctx, slot.getId(), file, scoreByStamp);

				final String action = req.getParameter("action");
				if ("next".equalsIgnoreCase(action) || "approve".equalsIgnoreCase(action) || "decline".equalsIgnoreCase(action)) {
					if ("approve".equalsIgnoreCase(action) || "decline".equalsIgnoreCase(action)) {
						score.setApproved("approve".equalsIgnoreCase(action));

						final Criteria[] cris = slot.getCriterias();
						for (Criteria cri : cris) {
							final int powDef = score.getPow(slot, cri);
							final double ratioDef = score.getRatio(slot, cri);

							final String idFor = score.idFor(slot, cri);
							final String powReq = req.getParameter(idFor);
							final String ratioReq = req.getParameter(idFor+"--ratio");

							score.getPows().put(idFor, G4Parse.parse(powReq, powDef));
							score.getRatios().put(idFor, G4Parse.parse(ratioReq, ratioDef));
						}

						score.setComment(req.getParameter("comment"));
						scoreDao.createScore(ctx, slot.getId(), fileId, score);
					}

					FileMeta epF = null;	//	earliest pending
					Ctx epCtx = null;

					final Ctx ctxEnr = Ctx.forEnr(ctx.getEnr()).resolve(enrollDao, groupDao,  courseDao);
					//	LATER oh this pretty obviously looks like we REALLY need some rdbms from now on... :D
					for (Student stud : ctx.getGroup().getStudents()) {
						final Ctx ctxStud = ctxEnr.extendStudent(stud);
						for (int index = 0; index < ctx.getEnr().getIndex().size(); index++) {
							final Ctx ctxVer = ctxStud.extendIndex(index);
							if (!ctxVer.getAssType().getId().equals(ctx.getAssType().getId())) {
								continue;	//	other ass types out of scope
							}
							for (FileSlot s : ctxVer.getAssType().getFileSlots()) {
								if (!s.getId().equals(slot.getId())) {
									continue;	//	other slots out of scope
								}
								final Entry<FileMeta>[] uploads = fileDao.findFilesFor(FileDao.SCOPE_STUD, ctxVer, slot.getId());
								if (uploads != null && uploads.length > 0) {
									for (int i = uploads.length - 1; i >= 0; i--) {
										final FileMeta f = uploads[i].getMeta();
										if (f.getScore() == null) {
											if ((epF == null || epF.getCreateStamp().getTime() > f.getCreateStamp().getTime())) {
												epF = f;
												epCtx = ctxVer;
											}
											break;	//	don't look into earlier pending versions before this one is approved
										} else if (Boolean.TRUE.equals(f.getScore().getApproved())) {
											break;	//	don't look into earlier pending versions before this one is approved
										}
									}
								}
							}
						}
					}

					if (epCtx != null) {
						resp.sendRedirect("approve?elw_ctx=" + epCtx.toString() + "&sId=" + slot.getId() + "&fId=" + epF.getId());
					} else {
						resp.sendRedirect("log?elw_ctx=" + ctxEnr.toString() + "&f_slot=" + slot.getId());
					}
				} else {
					resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "Bad action: " + action);
				}

				return null;
			}
		});
	}

	@RequestMapping(value = "groups", method = RequestMethod.GET)
	public ModelAndView do_groups(final HttpServletRequest req, final HttpServletResponse resp) throws IOException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Group[] groups = groupDao.findAllGroups();

		model.put("groups", groups);

		return new ModelAndView("a/groups", model);
	}

	@RequestMapping(value = "ul", method = RequestMethod.POST)
	public ModelAndView do_ul(final HttpServletRequest req, final HttpServletResponse resp) throws IOException, FileUploadException {
		final HashMap<String, Object> model = auth(req, resp, "");
		if (model == null) {
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		final String refreshUri = "course?" + WebSymbols.R_CTX + "=" + Ctx.forCourse(ctx.getCourse()).toString();

		return processUpload(req, resp, null, model, refreshUri, "akraievoy");
	}

	public ModelAndView processUpload(
			HttpServletRequest req, HttpServletResponse resp, String scopeForced,
			HashMap<String, Object> model, String refreshUri, String authorName
	) throws IOException, FileUploadException {
		final String scope = scopeForced == null ? req.getParameter("s") : scopeForced;
		if (scope == null) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "scope not set");
			return null;
		}

		final Ctx ctx = (Ctx) model.get(R_CTX);
		if (
				FileDao.SCOPE_ASS_TYPE.equals(scope) && !ctx.resolved(Ctx.STATE_CT) ||
				FileDao.SCOPE_ASS.equals(scope) && !ctx.resolved(Ctx.STATE_CTA) ||
				FileDao.SCOPE_VER.equals(scope) && !ctx.resolved(Ctx.STATE_CTAV) ||
				FileDao.SCOPE_STUD.equals(scope) && !ctx.resolved(Ctx.STATE_EGSCIV)
		) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "context path problem, please check the logs");
			return null;
		}

		final String slotId = req.getParameter("sId");
		if ((slotId == null || slotId.trim().length() == 0)) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "no slotId (sId) defined");
			return null;
		}

		final FileSlot slot = ctx.getAssType().findSlotById(slotId);
		if (slot == null) {
			resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "slot '" + slotId + "' not found");
			return null;
		}

		final boolean put = "PUT".equalsIgnoreCase(req.getMethod());
		if (req.getContentLength() == -1) {
			if (!put) {
				Message.addWarn(req, "upload size not reported");
				resp.sendRedirect(refreshUri);
			} else {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, "upload size not reported");
			}
			return null;
		}

		if (req.getContentLength() > slot.getLengthLimit()) {
			final String message = "upload exceeds " + G4mat.formatMem(slot.getLengthLimit());
			if (!put) {
				Message.addWarn(req, message);
				resp.sendRedirect(refreshUri);
			} else {
				resp.sendError(HttpServletResponse.SC_BAD_REQUEST, message);
			}
			return null;
		}

		final boolean binary = slot.isBinary() ;
		if (put) {
			//	LATER add some sensible defaults to the FileSlot entity
			final String name = "upload" + (binary ? ".bin" : ".txt");
			final FileMeta fileMeta = new FileMeta(
					name,
					name,
					binary ? "application/octet-stream" : "text/plain",
					authorName,
					W.resolveRemoteAddress(req)
			);

			fileDao.createFileFor(
					scope,
					ctx,
					slot.getId(),
					fileMeta,
					binary ? new BufferedInputStream(req.getInputStream()) : null,
					binary ? null : new BufferedReader(new InputStreamReader(req.getInputStream(), "UTF-8"))
			);

			return null;
		}

		final ServletFileUpload sfu = new ServletFileUpload(fileItemFactory);
		final FileItemIterator fii = sfu.getItemIterator(req);
		int fileCount = 0;
		String comment = null;
		while (fii.hasNext()) {
			final FileItemStream item = fii.next();
			if (item.isFormField()) {
				if ("comment".equals(item.getFieldName())) {
					comment = G4Io.dumpToString(item.openStream());
				}
				if ("expandTriggers".equals(item.getFieldName())) {	//	FIXME review
					req.getSession().setAttribute("course.expandTriggers", G4Io.dumpToString(item.openStream()));
				}
				continue;
			}

			if (fileCount > 0) {
				Message.addWarn(req, "one file per upload, please");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String fName = FileMeta.extractNameFromPath(item.getName());
			if (!Pattern.compile(slot.getNameRegex()).matcher(fName.toLowerCase()).matches()) {
				Message.addWarn(req, "check the file name: regex check failed");
				resp.sendRedirect(refreshUri);
				return null;
			}

			final String contentType = item.getContentType();
			if (G4Str.indexOf(contentType, slot.getContentTypes()) < 0) {
				Message.addWarn(req, "contentType '" + contentType + "': not listed in the slot");
			}

			final FileMeta fileMeta = new FileMeta(
					fName,
					fName,
					contentType,
					authorName,
					W.resolveRemoteAddress(req)
			);
			if (comment != null) {
				fileMeta.setComment(comment);
			}

			fileDao.createFileFor(
					scope,
					ctx,
					slot.getId(),
					fileMeta,
					binary ? new BufferedInputStream(item.openStream()) : null,
					binary ? null : new BufferedReader(new InputStreamReader(item.openStream(), "UTF-8"))
			);
			fileCount++;
		}

		if (fileCount == 1) {
			Message.addInfo(req, "Your upload has succeeded");
			resp.sendRedirect(refreshUri);
		} else {
			Message.addWarn(req, "Multiple uploads not allowed");
			resp.sendRedirect(refreshUri);
		}

		return null;
	}

}

