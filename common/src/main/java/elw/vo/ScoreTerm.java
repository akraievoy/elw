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

package elw.vo;

import org.akraievoy.gear.G4mat;

public class ScoreTerm {
	private final String id;
	private final double ratio;
	private final int pow;
	private final FileSlot slot;
	private final Criteria criteria;

	public ScoreTerm(String id, double ratio, int pow, final FileSlot slot, final Criteria criteria) {
		this.id = id;
		this.ratio = ratio;
		this.pow = pow;
		this.slot = slot;
		this.criteria = criteria;
	}

	public String getId() {
		return id;
	}

	public double getRatio() {
		return ratio;
	}

	public int getPow() {
		return pow;
	}

	public String getNiceRatio() {
		return getNiceRatio(ratio);
	}

	private static String getNiceRatio(final double ratio) {
		if (Math.abs(ratio - 1) < 1e-2) {
			return "";
		}

		final double percentage = Math.round(ratio * 1000) / 10.0;

		if (percentage < 100) {
			return "-" + G4mat.format2(100 - percentage) + "%";
		}

		return "+" + G4mat.format2(percentage - 100) + "%";
	}

	public boolean isIdentity() {
		return Math.abs(ratio - 1) < 1e-2;
	}

	public boolean isPositive() {
		return ratio > 1;
	}

	public Criteria getCriteria() {
		return criteria;
	}

	public FileSlot getSlot() {
		return slot;
	}
}
