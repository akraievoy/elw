package elw.dao;

import elw.vo.Group;
import org.akraievoy.gear.G;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class GroupDao extends Dao<Ctx, Group> {
	public String[] findGroupIds() {
		final String[][] pathElems = listCriteria(criteria, null, false, true, false, null);

		return pathElems[0];
	}

	public Group findGroup(final String id) {
		final Ctx groupCtx = new Ctx(Ctx.STATE_G, null, id, null, null, -1, null, null);
		final Entry<Group> entry = findLast(groupCtx, null, null);

		return entry.getMeta();
	}

	public Group[] findAllGroups() {
		final String[] groupIds = findGroupIds();
		final Group[] groups = new Group[groupIds.length];

		for (int i = 0; i < groups.length; i++) {
			groups[i] = findGroup(groupIds[i]);
		}

		return groups;
	}
}