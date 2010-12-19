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

public class VtTuple {
	protected final String text;
	protected final String classes;
	protected final String sort;

	public VtTuple(String text, String classes, String sort) {
		this.classes = classes;
		this.sort = sort;
		this.text = text;
	}

	public String getClasses() {
		return classes;
	}

	public String getSort() {
		return sort;
	}

	public String getText() {
		return text;
	}
}
