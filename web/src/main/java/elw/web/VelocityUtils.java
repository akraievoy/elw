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

import elw.dao.Ctx;
import elw.dao.FileDao;
import elw.vo.*;
import elw.vo.Class;
import org.joda.time.DateTime;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class VelocityUtils {
	public static final VelocityUtils INSTANCE = new VelocityUtils();

	public static final String MODEL_KEY = "u";

	public <E> Ref<E> ref(E e) {
		return new Ref<E>(e);
	}

	public Entry<FileMeta> bestOrLastOrNull(List<Entry<FileMeta>> entries) {
		if (entries == null || entries.isEmpty()) {
			return null;
		}

		for (Entry<FileMeta> entry : entries) {
			if (entry.getMeta().getScore() != null && entry.getMeta().getScore().isBest()) {
				return entry;
			}
		}

		return entries.get(entries.size() - 1);
	}

	public Map<String, String> status(
			Format f, String mode,
			String scope, Ctx ctx, FileSlot slot, Entry<FileMeta> file
	) {
		return status(f, mode, scope, ctx, slot, file, file != null ? file.getMeta().getScore() : null);
	}

	public Map<String, String> status(
			Format f, String mode,
			String scope, Ctx ctx, FileSlot slot, Entry<FileMeta> file, Score score
	) {
		final StringBuilder text = new StringBuilder();
		final StringBuilder cls = new StringBuilder();
		final StringBuilder sort = new StringBuilder();

		final elw.vo.Class classFrom = ctx.getEnr().getClasses().get(ctx.getIndexEntry().getClassFrom());
		final elw.vo.Class classDue;
		final Integer classDueIdx = ctx.getIndexEntry().getClassDue().get(slot.getId());
		if (classDueIdx != null) {
			classDue = ctx.getEnr().getClasses().get(classDueIdx);
		} else {
			classDue = null;
		}

		if (FileDao.SCOPE_STUD.equalsIgnoreCase(scope)) {
			if ("a".equalsIgnoreCase(mode)) {
				if (!classFrom.isStarted()) {
					text.append("Closed");
	//				text.append("; Opens ").append(f.format(classFrom.getFromDateTime().getMillis()));
					cls.append("elw_closed");
					sort.append(-1);
				} else if (file == null) {
					text.append("Open");
					cls.append("elw_open");
					sort.append(1);
				} else if (score == null || score.getApproved() == null) {
					text.append("Pending");
					cls.append("elw_pending");
					sort.append(0);
				} else if (score.getApproved()) {
					text.append("Approved");
					cls.append("elw_approved");
					sort.append(-2);
				} else {
					text.append("Declined");
					cls.append("elw_declined");
					sort.append(2);
				}
			} else if ("d".equalsIgnoreCase(mode)) {
				if (classDue == null) {
					text.append("No Due Date");
					sort.append(0);
				} else {
					text.append("Due ").append(f.format(classDue.getToDateTime().getMillis()));
					sort.append(classDue.getToDateTime().getMillis());
				}
			} else if ("dd".equalsIgnoreCase(mode)) {
				if (classDue == null) {
					text.append("No Due Date");
					sort.append(0);
				} else {
					int dueDiff = classDue.computeToDiffStamp(file == null ? null : file.getMeta());
					if (dueDiff > 0) {
						text.append(dueDiff).append("d overdue");
					} else if (dueDiff == 0) {
						text.append("Same day");
					} else {
						text.append(-dueDiff).append("d ahead");
					}
					sort.append(dueDiff);
				}
			} else if ("p".equalsIgnoreCase(mode)) {
				if (!classFrom.isStarted()) {
					text.append("Closed");
					cls.append("elw_closed");
					sort.append(0);
				} else {
					if (ctx.getIndexEntry().getScoreBudget() > 0) {
						final double scoreStud;
						if (file != null && score != null) {
							scoreStud = ctx.getIndexEntry().computePoints(score, slot);
						} else {
							scoreStud = 0;
						}
						final double scoreSlot = ctx.getIndexEntry().computePoints(slot);
						text.append(f.format2(scoreStud)).append(" of ").append(f.format2(scoreSlot));
						sort.append(scoreStud);
					} else {
						sort.append(0);
					}

					if (file == null) {
						text.insert(0, "<span class=\"elw_open\">O</span> ");
					} else if (score == null || score.getApproved() == null) {
						final String commentSafe = score == null ? "Preliminary": score.getComment();
						text.insert(0, "<span class=\"elw_pending\" title=\""+f.esc(commentSafe)+"\">P</span> ");
					} else if (score.getApproved()) {
						text.insert(0, "<span class=\"elw_approved\" title=\""+f.esc(score.getComment())+"\">A</span> ");
					} else {
						text.insert(0, "<span class=\"elw_declined\" title=\""+f.esc(score.getComment())+"\">D</span> ");
					}
				}
			} else if ("s".equalsIgnoreCase(mode)) {
				if (!classFrom.isStarted()) {
					text.append("Closed");
					cls.append("elw_closed");
					sort.append(0);
				} else {
					if (ctx.getIndexEntry().getScoreBudget() > 0) {
						final double scoreStud;
						final ScoreTerm[] terms;
						if (file != null && score != null) {
							scoreStud = ctx.getIndexEntry().computePoints(score, slot);
							terms = score.getTerms(ctx.getAssType(), false);
						} else {
							scoreStud = 0;
							terms = new ScoreTerm[0];
						}
						sort.append(scoreStud);
						text.append(f.format2(scoreStud));
						if (terms.length > 0) {
							text.append(":");
							for (ScoreTerm term : terms) {
								text.append(" <span class=\"elw_").append(term.getRatio() < 1 ? "neg" : "pos")
										.append("Term\" title=\"").append(f.esc(term.getCriteria().getName()))
										.append(" x ").append(term.getPow()).append("\">")
										.append(term.getNiceRatio()).append("</span>");
							}
						}
					} else {
						sort.append(0);
					}

					if (file == null) {
						text.insert(0, "<span class=\"elw_open\">O</span> ");
					} else if (score == null || score.getApproved() == null) {
						final String commentSafe = score == null ? "Preliminary": score.getComment();
						text.insert(0, "<span class=\"elw_pending\" title=\""+f.esc(commentSafe)+"\">P</span> ");
					} else if (score.getApproved()) {
						text.insert(0, "<span class=\"elw_approved\" title=\""+f.esc(score.getComment())+"\">A</span> ");
					} else {
						text.insert(0, "<span class=\"elw_declined\" title=\""+f.esc(score.getComment())+"\">D</span> ");
					}
				}
			} else if ("dta".equalsIgnoreCase(mode)) {
				if (!classFrom.isStarted()) {
					text.append("Closed");
					cls.append("elw_closed");
					sort.append(0);
				} else if (file == null) {
					text.append("Open");
					cls.append("elw_open");
					sort.append(0);
				} else {
					final DateTime scoreStamp;
					if (score == null || score.getCreateStamp() == null) {
						scoreStamp = new DateTime();
					} else {
						scoreStamp = new DateTime(score.getCreateStamp().getTime());
					}
					final int dta = classFrom.computeToDiff(scoreStamp) - classFrom.computeToDiffStamp(file.getMeta());
					text.append(dta).append(" days");
					sort.append(dta);
				}
			} else if ("dtu".equalsIgnoreCase(mode)) {
				if (!classFrom.isStarted()) {
					text.append("Closed");
					cls.append("elw_closed");
					sort.append(0);
				} else if (file == null) {
					text.append("Open");
					cls.append("elw_open");
					sort.append(0);
				} else {
					final int dtu = classFrom.computeToDiffStamp(file.getMeta());
					text.append(dtu).append(" days");
					sort.append(dtu);
				}
			} else if ("v".equalsIgnoreCase(mode)) {
				if (!classFrom.isStarted()) {
					text.append("Closed");
					cls.append("elw_closed");
					sort.append(-1);
				} else {
					text.append("<span title=\"").append(ctx.getVer().getName()).append("\">").append(ctx.getVer().getId()).append("</span>");
					sort.append(ctx.getVer().getId());
				}
			} else if ("ip".equalsIgnoreCase(mode)) {
				int sortVal = -1;
				if (file != null) {
					sortVal++;
					text.append(file.getMeta().getSourceAddress());
					if (!classFrom.checkOnSite(file.getMeta().getSourceAddress())) {
						sortVal+=4;
						cls.append(" elw_offsite");
					}
					if (!ctx.getEnr().checkOnTime(file.getMeta().getCreateStamp())) {
						sortVal+=2;
						cls.append(" elw_offtime");
					}
					if (!classFrom.checkOnTime(file.getMeta().getCreateStamp())) {
						sortVal++;
						cls.append(" elw_rapid");
					}
				}
				sort.append(sortVal);
			}

			overdueClasses(file, classDue, cls);
		} else {
			if (file == null) {
				text.append("Open @ Course");
				sort.append(-2);
			} else if (FileDao.SCOPE_ASS_TYPE.equalsIgnoreCase(scope)) {
				text.append("Type @ Course");
				sort.append(-1);
			} else if (FileDao.SCOPE_ASS.equalsIgnoreCase(scope)) {
				text.append("Task @ Course");
				sort.append(0);
			} else if (FileDao.SCOPE_VER.equalsIgnoreCase(scope)) {
				text.append("Ver @ Course");
				sort.append(1);
			}
		}

		final Map<String, String> res = new TreeMap<String, String>();
		res.put("text", text.toString());
		res.put("classes", cls.toString());
		res.put("sort", sort.toString());
		return res;
	}

	protected static void overdueClasses(Entry<FileMeta> file, Class classDue, StringBuilder cls) {
		if (classDue != null) {
			if (classDue.isPassed()) {
				cls.append(" elw_due_passed");

				if (file == null || classDue.computeDaysOverdue(file.getMeta().getCreateStamp()) > 0) {
					cls.append(" elw_overdue");
				}
			}
		} else {
			cls.append(" elw_nodue");
		}
	}
}
