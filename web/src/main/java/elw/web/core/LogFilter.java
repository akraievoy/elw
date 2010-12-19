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

import elw.dao.Ctx;
import elw.vo.Entry;
import elw.vo.FileMeta;
import elw.vo.FileSlot;
import elw.vo.Score;
import org.joda.time.DateTime;

public class LogFilter {
	final String due;
	final boolean latest;
	final String slotId;
	final String studId;
	final String verId;
	final String mode;
	final String scope;
	final String[] scopePath;

	public LogFilter() {
		this(null, null, null, "any", "a", "", false);
	}

	public LogFilter(
			String slotId, String studId, String verId, String due, String mode, String scope, boolean latest
	) {
		this.due = due;
		this.latest = latest;
		this.slotId = slotId;
		this.studId = studId;
		this.verId = verId;
		this.mode = mode;
		this.scope = scope;

		final String[] scopePathArr = scope.split("--");
		this.scopePath = new String[] {
				scopePathArr.length > 0 ? scopePathArr[0] : "",
				scopePathArr.length > 1 ? scopePathArr[1] : ""
		};
	}

	public String getDue() {
		return due;
	}

	public boolean isLatest() {
		return latest;
	}

	public String getSlotId() {
		return slotId;
	}

	public String getStudId() {
		return studId;
	}

	public String getVerId() {
		return verId;
	}

	public String getMode() {
		return mode;
	}

	public String getScope() {
		return scope;
	}

	public String[] getScopePath() {
		return scopePath;
	}

	public boolean cScopeOpen() {
		return cScopeOne('o');
	}

	public boolean cScopeOne(final char ch) {
		return getScopePath()[1].length() == 0 || getScopePath()[1].indexOf(ch) >= 0;
	}

	public boolean cScopeStud(FileSlot slot, Entry<FileMeta> e) {
		final FileMeta file = e != null ? e.getMeta() : null;
		final Score score = file != null ? file.getScore() : null;

		if (file == null) {
			return slot.isWritable() && cScopeOne('o');
		} else if (score == null || score.getApproved() == null) {
			return cScopeOne('p');
		} else if (score.getApproved()) {
			return cScopeOne('a');
		} else {
			return cScopeOne('d');
		}
	}

	public boolean cLatest(boolean last) {
		return !isLatest() || last;
	}

	public boolean cDue(Ctx ctxAss, FileSlot slot) {
		if ("any".equalsIgnoreCase(due)) {
			return true;
		}

		final elw.vo.Class classDue = ctxAss.cDue(slot.getId());
		if ("none".equalsIgnoreCase(due)) {
			return classDue == null;
		}
		if ("set".equalsIgnoreCase(due)) {
			return classDue != null;
		}
		if (classDue == null) {
			return false;
		}

		final int dayDiff = classDue.computeToDiff(new DateTime());

		if ("over".equalsIgnoreCase(due)) {
			return dayDiff > 0;
		}

		if ("today".equalsIgnoreCase(due)) {
			return dayDiff >= 0;
		}

		//noinspection SimplifiableIfStatement
		if ("twoweeks".equalsIgnoreCase(due)) {
			return dayDiff >= -14;
		}

		return true;
	}

	protected boolean cVer(Ctx ctx) {
		return !W.excluded(getVerId(), ctx.getAss().getId(), ctx.getVer() == null ? "" : ctx.getVer().getId());
	}
}
