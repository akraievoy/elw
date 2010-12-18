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

package elw.dao;

import elw.vo.Admin;
import elw.vo.Entry;
import elw.vo.Group;
import elw.vo.Path;

public class AdminDao extends Dao<Admin>{
	@Override
	public Path pathFromMeta(Admin adm) {
		return new Path(adm.getId());
	}

	protected String[] findAllIds() {
		final Path pathAll = new Path(new String[]{null});
		final String[][] pathElems = listCriteria(pathAll);

		return pathElems[0];
	}

	public Admin findAdminById(final String id) {
		final Entry<Admin> entry = findLast(new Path(id), null, null);

		return entry != null ? entry.getMeta() : null;
	}

	public Admin[] findAll() {
		final String[] admIds = findAllIds();
		final Admin[] admins = new Admin[admIds.length];

		for (int i = 0; i < admins.length; i++) {
			admins[i] = findAdminById(admIds[i]);
		}

		return admins;
	}
}
