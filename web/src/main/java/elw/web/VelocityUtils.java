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

import java.util.List;

public class VelocityUtils {
	public static final VelocityUtils INSTANCE = new VelocityUtils();

	public static final String MODEL_KEY = "u";

	public <E> Ref<E> ref(E e) {
		return new Ref<E>(e);
	}

	public <E> E lastOrNull(List<E> es) {
		return es.isEmpty() ? null : es.get(es.size() - 1);
	}
}
