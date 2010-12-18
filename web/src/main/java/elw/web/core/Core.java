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

package elw.web.core;

import elw.dao.*;
import elw.vo.*;
import elw.web.VelocityUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Core {
	protected final CourseDao courseDao;
	protected final GroupDao groupDao;
	protected final EnrollDao enrollDao;
	protected final ScoreDao scoreDao;
	protected final FileDao fileDao;

	public Core(CourseDao courseDao, EnrollDao enrollDao, FileDao fileDao, GroupDao groupDao, ScoreDao scoreDao) {
		this.courseDao = courseDao;
		this.enrollDao = enrollDao;
		this.fileDao = fileDao;
		this.groupDao = groupDao;
		this.scoreDao = scoreDao;
	}

	public List<Object[]> log(
			Ctx ctx, Format format, VelocityUtils u, LogFilter logFilter, final boolean adm
	) {
		final List<Object[]> logData = new ArrayList<Object[]>();

		if ("s".equalsIgnoreCase(logFilter.getScopePath()[0])) {
			logStud(ctx, format, u, logFilter, logData, adm);
		} else if ("c".equalsIgnoreCase(logFilter.getScopePath()[0])) {
			logCourse(ctx, format, u, logFilter, logData, adm);
		}

		return logData;
	}

	protected void logCourse(
			Ctx ctx, Format format, VelocityUtils u, LogFilter lf,
			List<Object[]> logData, final boolean adm
	) {
		for (int i = 0; i < ctx.getEnr().getIndex().size(); i++) {
			final Ctx ctxAss = ctx.extendIndex(i);

			final AssignmentType aType = ctxAss.getAssType();
			final FileSlot[] slots = aType.getFileSlots();
			for (FileSlot slot : slots) {
				if (W.excluded(lf.getSlotId(), aType.getId(), slot.getId())) {
					continue;
				}
				if (!lf.cDue(ctxAss, slot)) {
					continue;
				}
				if (!adm && !ctxAss.cFrom().isStarted()) {
					continue;
				}

				final Entry<FileMeta>[] uploadsAss = fileDao.findFilesFor(FileDao.SCOPE_ASS, ctxAss, slot.getId());
				int total = uploadsAss.length;
				if (lf.cScopeOne('a') && lf.cVer(ctxAss)) {
					logRows(format, u, lf, logData, i, ctxAss, slot, uploadsAss, FileDao.SCOPE_ASS, adm);
				}

				final Version[] versions = adm ? ctxAss.getAss().getVersions() : new Version[] {ctxAss.getVer()};
				for (Version ver : versions) {
					final Ctx ctxVer = ctxAss.extendVer(ver);
					final Entry<FileMeta>[] uploadsVer = fileDao.findFilesFor(FileDao.SCOPE_VER, ctxVer, slot.getId());
					total += uploadsVer.length;
					if (lf.cScopeOne('v') && lf.cVer(ctxVer)) {
						logRows(format, u, lf, logData, i, ctxVer, slot, uploadsVer, FileDao.SCOPE_VER, adm);
					}
				}

				if (lf.cScopeOpen() && lf.cVer(ctxAss) && total == 0) {
					logData.add(logRow(format, u, lf.getMode(), logData, i, ctxAss, slot, null, FileDao.SCOPE_ASS, adm));
				}
			}
		}
	}

	protected void logStud(Ctx ctx, Format f, VelocityUtils u, LogFilter lf, List<Object[]> logData, boolean adm) {
		if (adm) {
			for (Student stud : ctx.getGroup().getStudents()) {
				if (W.excluded(lf.getStudId(), stud.getId())) {
					continue;
				}

				final Ctx ctxStud = ctx.extendStudent(stud);
				logStudForStud(ctx, f, u, lf, logData, ctxStud, adm);
			}
		} else {
			if (ctx.getStudent() != null) {
				logStudForStud(ctx, f, u, lf, logData, ctx, adm);
			}
		}
	}

	protected void logStudForStud(
			Ctx ctx, Format f, VelocityUtils u,
			LogFilter lf, List<Object[]> logData, Ctx ctxStud,
			boolean adm) {
		for (int index = 0; index < ctx.getEnr().getIndex().size(); index++) {
			final Ctx ctxVer = ctxStud.extendIndex(index);
			if (!adm && !ctxVer.cFrom().isStarted()) {
				continue;
			}
			if (!lf.cVer(ctxVer)) {
				continue;
			}
			final AssignmentType aType = ctxVer.getAssType();
			final FileSlot[] slots = aType.getFileSlots();

			for (FileSlot slot : slots) {
				if (W.excluded(lf.getSlotId(), aType.getId(), slot.getId())) {
					continue;
				}

				final Entry<FileMeta>[] uploads = fileDao.findFilesFor(FileDao.SCOPE_STUD, ctxVer, slot.getId());
				if (!lf.cDue(ctxVer, slot)) {
					continue;
				}

				logRows(f, u, lf, logData, index, ctxVer, slot, uploads, FileDao.SCOPE_STUD, adm);
				if (uploads.length == 0 && lf.cScopeStud(slot, null)) {
					logData.add(logRow(f, u, lf.getMode(), logData, index, ctxVer, slot, null, FileDao.SCOPE_STUD, adm));
				}
			}
		}
	}

	protected int logRows(Format format, VelocityUtils u, LogFilter logFilter, List<Object[]> logData, int index, Ctx ctxVer, FileSlot slot, Entry<FileMeta>[] uploads, String scope, boolean adm) {
		int shown = 0;
		for (int i = 0, uploadsLength = uploads.length; i < uploadsLength; i++) {
			final boolean last = i + 1 == uploadsLength;
			if (!logFilter.cLatest(last)) {
				continue;
			}
			final Entry<FileMeta> e = uploads[i];
			if (FileDao.SCOPE_STUD.equals(scope) && !logFilter.cScopeStud(slot, e)) {
				continue;
			}
			shown += 1;
			logData.add(logRow(format, u, logFilter.getMode(), logData, index, ctxVer, slot, e, scope, adm));
		}
		return shown;
	}

	protected Object[] logRow(
			Format f, VelocityUtils u, final String mode, List<Object[]> data,
			int index, Ctx ctx, FileSlot slot, Entry<FileMeta> e, String scope,
			boolean adm) {
		final long time = e == null ? System.currentTimeMillis() : e.getMeta().getCreateStamp().getTime();

		final IndexEntry iEntry = ctx.getIndexEntry();
		final String nameNorm;
		if (e == null) {
			nameNorm = "";
		} else {
			if (FileDao.SCOPE_STUD.equalsIgnoreCase(scope)) {
				nameNorm = iEntry.normName(
						ctx.getEnr(), ctx.getStudent(), ctx.getAss(),
						ctx.getVer(), slot, e.getMeta(), f
				);
			} else {
				nameNorm = e.getMeta().getName();
			}
		}
		final StringBuilder q = new StringBuilder(
				"?elw_ctx=" + ctx.toString() + "&s=" + scope + "&sId=" + slot.getId()
		);
		if (e != null) {
			q.append("&fId=").append(e.getMeta().getId());
		}

		final String dlRef;
		if (e != null) {
			dlRef = (adm ? "../s/" : "") + "dl/" + nameNorm + q;
		} else {
			dlRef = null;
		}

		//	FIXME proper UL URI here
		final String ulRef;
		if (adm) {
			if (!FileDao.SCOPE_STUD.equals(scope)) {
				ulRef = "ul" + q;
			} else {
				ulRef = null;
			}
		} else {
			if (FileDao.SCOPE_STUD.equals(scope) && ctx.getVer().checkWrite(ctx.getAssType(), ctx.getAss(), slot.getId(), fileDao.loadFilesStud(ctx))) {
				ulRef = "ul" + q;
			} else {
				ulRef = null;
			}
		}

		final String authorName;
		if (e == null) {
			if (FileDao.SCOPE_STUD.equalsIgnoreCase(scope)) {
				authorName = ctx.getStudent().getName();
			} else {
				authorName = "Admin";
			}
		} else {
			authorName = e.getMeta().getAuthor();
		}


		final Map<String, String> status = u.status(f, mode, scope, ctx, slot, e);
		final Object[] dataRow = {
				/* 0 index */ data.size(),
				/* 1 upload millis */ time,
				/* 2 nice date - full */ e  == null ? "" : f.format(time, "EEE d HH:mm"),
				/* 3 nice date - nice */ e  == null ? "" : f.format(time),
				/* 4 author.id */ FileDao.SCOPE_STUD.equalsIgnoreCase(scope) ? ctx.getStudent().getId() : "admin",
				/* 5 author.name */ authorName,
				/* 6 class.index */ index,
				/* 7 class.name */ ctx.getAss().getName(),
				/* 8 slot.id */ ctx.getAssType().getId() + "--" + slot.getId(),
				/* 9 slot.name */ ctx.getVer() != null ? ctx.getVer().getName() + " / " + slot.getName() : slot.getName(),
				/* 10 comment */ e  == null ? "" : e.getMeta().getComment(),
				/* 11 status sort*/ status.get("sort"),
				/* 12 status text*/ status.get("text"),
				/* 13 status classes */ status.get("classes"),
				/* 14 source ip */ e  == null ? "" : e.getMeta().getSourceAddress(),
				/* 15 size bytes */ e  == null ? "" : e.computeSize(),
				/* 16 size */ e  == null ? "" : f.formatSize(e.computeSize()),
				/* 17 approve ref */ "approve" + q,
				/* 18 dl ref */ dlRef,
				/* 19 ul ref */ ulRef,
				/* 20 comment ref */ adm ? null : "#"	//	TODO comment edit url/page/method
		};
		return dataRow;
	}

	public List<Object[]> index(Enrollment[] enrolls) {
		final List<Object[]> indexData = new ArrayList<Object[]>();
		for (Enrollment enr : enrolls) {
			indexData.add(indexRow(indexData, enr));
		}
		return indexData;
	}

	protected Object[] indexRow(List<Object[]> indexData, Enrollment enr) {
		final Group group = groupDao.findGroup(enr.getGroupId());
		final Course course = courseDao.findCourse(enr.getCourseId());

		final String uploadsBase = "log?elw_ctx=e--" + enr.getId();
		final Object[] arr = {
				/* 0 index - */ indexData.size(),
				/* 1 enr.id - */ enr.getId(),
				/* 2 group.id - */ group.getId(),
				/* 3 group.name 0*/ group.getName(),
				/* 4 course.id - */ course.getId(),
				/* 5 course.name 1 */ course.getName(),
				/* 6 summary ref 2 */ "enroll?elw_ctx=e--"+enr.getId(),
				/* 7 students ref 3 */ "#",
				/* 8 tasks ref 4 */ "#",
				/* 9 classes ref 5 */ "#",
				/* 10 uploads ref 6 */ uploadsBase +"&f_scope=s--p--&f_due=today&f_mode=dd",
				/* 11 uploads-open ref 7 */ uploadsBase + "&f_scope=s--o--&f_due=twoweeks&f_mode=dd",
				/* 12 uploads-course ref 8 */ uploadsBase + "&f_scope=c--av--&f_due=any",
		};
		return arr;
	}
}
