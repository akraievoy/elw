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
			Ctx ctx, FileSlot slot, Entry<FileMeta> file
	) {
		return status(f, mode, ctx, slot, file, file != null ? file.getMeta().getScore() : null);
	}

	public Map<String, String> status(
			Format f, String mode,
			Ctx ctx, FileSlot slot, Entry<FileMeta> file, Score score
	) {
		final StringBuilder text = new StringBuilder();
		final StringBuilder cls = new StringBuilder();

		final elw.vo.Class classFrom = ctx.getEnr().getClasses().get(ctx.getIndexEntry().getClassFrom());
		final elw.vo.Class classDue;
		if (ctx.getIndexEntry().getClassDue() != null) {
			final Integer classDueIdx = ctx.getIndexEntry().getClassDue().get(slot.getId());
			classDue = ctx.getEnr().getClasses().get(classDueIdx);
		} else {
			classDue = null;
		}

		if ("a".equalsIgnoreCase(mode)) {
			if (!classFrom.isStarted()) {
				text.append("Closed");
//				text.append("; Opens ").append(f.format(classFrom.getFromDateTime().getMillis()));
				cls.append("elw_closed");
			} else if (file == null) {
				text.append("Open");
				cls.append("elw_open");
			} else if (score == null || score.getApproved() == null) {
				text.append("Pending");
				cls.append("elw_pending");
			} else if (score.getApproved()) {
				text.append("Approved");
				cls.append("elw_approved");
			} else {
				text.append("Declined");
				cls.append("elw_declined");
			}
		} else if ("d".equalsIgnoreCase(mode)) {
			if (classDue == null) {
				text.append("No Due Date");
			} else {
				text.append("Due ").append(f.format(classDue.getToDateTime().getMillis()));
			}
		} else if ("dd".equalsIgnoreCase(mode)) {
			if (classDue == null) {
				text.append("No Due Date");
			} else {
				int dueDiff = classDue.computeToDiffStamp(file == null ? null : file.getMeta());
				if (dueDiff > 0) {
					text.append(dueDiff).append("d overdue");
				} else if (dueDiff == 0) {
					text.append("Same day");
				} else {
					text.append(-dueDiff).append("d ahead");
				}
			}
		} else if ("p".equalsIgnoreCase(mode)) {
			if (!classFrom.isStarted()) {
				text.append("Closed");
				cls.append("elw_closed");
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
				}

				if (file == null) {
					text.insert(0, "<span class=\"elw_open\">O</span> ");
				} else if (score == null || score.getApproved() == null) {
					text.insert(0, "<span class=\"elw_pending\">P</span> ").append(" ?");
				} else if (score.getApproved()) {
					text.insert(0, "<span class=\"elw_approved\">A</span> ");
				} else {
					text.insert(0, "<span class=\"elw_declined\">D</span> ").append(" !");
				}
			}
		} else if ("s".equalsIgnoreCase(mode)) {
			if (!classFrom.isStarted()) {
				text.append("Closed");
				cls.append("elw_closed");
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
				}

				if (file == null) {
					text.insert(0, "<span class=\"elw_open\">O</span> ");
				} else if (score == null || score.getApproved() == null) {
					text.insert(0, "<span class=\"elw_pending\">P</span> ").append(" ?");
				} else if (score.getApproved()) {
					text.insert(0, "<span class=\"elw_approved\">A</span> ");
				} else {
					text.insert(0, "<span class=\"elw_declined\">D</span> ").append(" !");
				}
			}
		} else if ("dta".equalsIgnoreCase(mode)) {
			if (!classFrom.isStarted()) {
				text.append("Closed");
				cls.append("elw_closed");
			} else if (file == null) {
				text.append("Open");
				cls.append("elw_open");
			} else {
				final DateTime scoreStamp;
				if (score == null || score.getCreateStamp() == null) {
					scoreStamp = new DateTime();
				} else {
					scoreStamp = new DateTime(score.getCreateStamp().getTime());
				}
				final int dta = classFrom.computeToDiff(scoreStamp) - classFrom.computeToDiffStamp(file.getMeta());
				text.append(dta).append(" days");
			}
		} else if ("dtu".equalsIgnoreCase(mode)) {
			if (!classFrom.isStarted()) {
				text.append("Closed");
				cls.append("elw_closed");
			} else if (file == null) {
				text.append("Open");
				cls.append("elw_open");
			} else {
				final int dtu = classFrom.computeToDiffStamp(file.getMeta());
				text.append(dtu).append(" days");
			}
		} else if ("v".equalsIgnoreCase(mode)) {
			if (!classFrom.isStarted()) {
				text.append("Closed");
				cls.append("elw_closed");
			} else {
			  text.append("<span title=\"").append(ctx.getVer().getName()).append("\">").append(ctx.getVer().getId()).append("</span>");
			}
		}

		final Map<String, String> res = new TreeMap<String, String>();
		res.put("text", text.toString());
		res.put("classes", cls.toString());
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
