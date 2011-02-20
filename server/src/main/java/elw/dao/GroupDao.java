package elw.dao;

import elw.vo.Entry;
import elw.vo.Group;
import elw.vo.Path;

public class GroupDao extends Dao<Group> {
	@Override
	public Path pathFromMeta(Group group) {
		return new Path(group.getId());
	}

	private String[] findGroupIds() {
		final Path pathAll = new Path(new String[]{null});
		final String[][] pathElems = listCriteria(pathAll);

		return pathElems[0];
	}

	public Group findGroup(final String id) {
		final Entry<Group> entry = findLast(new Path(id), null, null);
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