package elw.dao;

import elw.vo.*;

/**
 * Path: course.id/assType.id/ass.id
 */
public class AssDao extends Dao<Assignment> {
	private final FileDao fileDao;

	public AssDao(FileDao fileDao) {
		this.fileDao = fileDao;
	}

	@Override
	public Path pathFromMeta(Assignment assignment) {
		return new Path(assignment.getPath());
	}

	private String[] findIdsForAssType(final Course course, final AssignmentType assType) {
		final String[][] pathElems = listCriteria(forType(course, assType, null));

		return pathElems[2];
	}

	public Assignment[] findAllForAssType(final Course course, final AssignmentType type) {
		final String[] assIds = findIdsForAssType(course, type);

		return load(course, type, assIds);
	}

	private Assignment[] load(final Course course, final AssignmentType assType, String[] assIds) {
		final Assignment[] assignments = new Assignment[assIds.length];

		for (int i = 0; i < assignments.length; i++) {
			assignments[i] = findAssignment(course, assType, assIds[i]);
		}

		return assignments;
	}

	private Assignment findAssignment(final Course course, final AssignmentType assType, final String assId) {
		if (assId == null) {
			return null;
		}

		final Entry<Assignment> entry = findLast(forType(course, assType, assId), null, null);

		if (entry == null) {
			return null;
		}

		final Assignment ass = entry.getMeta();
		final Ctx ctxAss = Ctx.forAss(course, assType, ass);
		final Version[] vers = ass.getVersions();
		final FileSlot[] slots = assType.getFileSlots();

		for (FileSlot slot : slots) {
			ass.setFiles(
					slot.getId(),
					fileDao.findFilesFor(FileDao.SCOPE_ASS, ctxAss, slot.getId())
			);
			for (Version ver : vers) {
				final Ctx ctxVer = ctxAss.extendVer(ver);
				ver.setFiles(
						slot.getId(),
						fileDao.findFilesFor(FileDao.SCOPE_VER, ctxVer, slot.getId())
				);
			}
		}

		return entry.getMeta();
	}

	private static Path forType(Course course, AssignmentType assType, String assId) {
		final Path forType = new Path(new String[]{course.getId(), assType.getId(), assId});
		return forType;
	}
}