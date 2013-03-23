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

import org.akraievoy.couch.Squab;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

//  FIXME this plain-text password stored in the couch is... well.. LAME
public class Admin extends Squab implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private String password;
    public String getPassword() { return password; }
    public void setPassword(String password) { this.password = password; }

    private List<String> openIds = new ArrayList<String>();
    public List<String> getOpenIds() {
        return Collections.unmodifiableList(openIds);
    }
    public void setOpenIds(ArrayList<String> openIds) {
        this.openIds.clear();
        if (openIds != null) {
            this.openIds.addAll(openIds);
        }
    }

    @Override
    protected String[] pathElems() {
        return new String[] {id};
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Admin{ ");
        sb.append("id ").append(id).append(' ');
        sb.append("name ").append(name).append(' ');
        sb.append('}');
        return sb.toString();
    }
}
