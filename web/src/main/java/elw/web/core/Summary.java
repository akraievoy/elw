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

import java.util.Map;

public class Summary {
	private final int openNum;
	private final int pendingNum;
	private final int approvedNum;
	private final int declinedNum;
	private final Long earliestDue;

	public Summary(int approvedNum, int declinedNum, int openNum, int pendingNum, Long earliestDue) {
		this.approvedNum = approvedNum;
		this.declinedNum = declinedNum;
		this.openNum = openNum;
		this.pendingNum = pendingNum;
		this.earliestDue = earliestDue;
	}

	public int getApprovedNum() {
		return approvedNum;
	}

	public int getDeclinedNum() {
		return declinedNum;
	}

	public int getOpenNum() {
		return openNum;
	}

	public int getPendingNum() {
		return pendingNum;
	}

	public Long getEarliestDue() {
		return earliestDue;
	}

	private Summary inc(Summary d) {
		return new Summary(
				this.approvedNum + d.approvedNum,
				this.declinedNum + d.declinedNum,
				this.openNum + d.openNum,
				this.pendingNum + d.pendingNum,
				minDue(earliestDue, d)
		);
	}

	private Long minDue(final Long thisDue, Summary thatDue) {
		if (thisDue != null && thatDue.earliestDue != null) {
			return Math.min(thisDue, thatDue.earliestDue);
		}

		if (thisDue != null) {
			return thisDue;
		}

		if (thatDue.earliestDue != null) {
			return thatDue.earliestDue;
		}

		return null;
	}

	public static void increment(final Map<String, Summary> summaryMap, final String path, final Summary sum) {
		if (summaryMap == null) {
			return;
		}

		if (summaryMap.get(path) == null) {
			summaryMap.put(path, sum);
		} else  {
			summaryMap.put(path, summaryMap.get(path).inc(sum));
		}
	}

	protected static void increment(final Map<String, Double> stats, final String path, final double amt) {
		if (stats.get(path) == null) {
			stats.put(path, amt);
		} else {
			stats.put(path, stats.get(path) + amt);
		}
	}

	public static Summary forScore(Long classDueStamp, Boolean approved) {
		Summary sum;
		if (Boolean.TRUE.equals(approved)) {
			sum = new Summary(1, 0, 0, 0, null);
		} else if (Boolean.FALSE.equals(approved)) {
			sum = new Summary(0, 1, 0, 0, classDueStamp);
		} else {
			sum = new Summary(0, 0, 0, 1, null);
		}
		return sum;
	}
}
