package elw.dao;

import elw.vo.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.Array;

public class FileDao extends Dao<FileMeta> {
	private static final Logger log = LoggerFactory.getLogger(FileDao.class);

	public FileDao() {
	}

	@Override
	public Path pathFromMeta(FileMeta file) {
		return new Path(file.getPath());
	}

	public Entry<FileMeta>[] findFilesForCourse(final Ctx ctx) {
		return findFiles(pathForCourse(ctx, null));
	}

	public Stamp createFileForCourse(
			Ctx ctx, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return create(pathForCourse(ctx, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForCourse(Ctx ctx, final String fileId) {
		return findLast(pathForCourse(ctx, null).setLast(fileId), null, null);
	}

	public Entry<FileMeta>[] findFilesForAssignment(final Ctx ctx, final String slotId) {
		return findFiles(pathForAss(ctx, slotId, null));
	}

	public Stamp createFileForAssignment(
			Ctx ctx, String slotId, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return create(pathForAss(ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForAssignment(Ctx ctx, final String slotId, final String fileId) {
		return findLast(pathForAss(ctx, slotId, null).setLast(fileId), null, null);
	}

	public Entry<FileMeta>[] findFilesForVersion(final Ctx ctx, final String slotId) {
		return findFiles(pathForVer(ctx, slotId, null));
	}

	public Stamp createFileForVersion(
			Ctx ctx, String slotId, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return create(pathForVer(ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForVersion(Ctx ctx, final String slotId, final String fileId) {
		return findLast(pathForVer(ctx, slotId, null).setLast(fileId), null, null);
	}


	public Entry<FileMeta>[] findFilesForStudent(final Ctx ctx, final String slotId) {
		return findFiles(pathForStud(ctx, slotId, null));
	}

	public Stamp createFileForStudent(
			Ctx ctx, String slotId, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return create(pathForStud(ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForStudent(Ctx ctx, final String slotId, final String fileId) {
		return findLast(pathForStud(ctx, slotId, null).setLast(fileId), null, null);
	}

	protected Path pathForCourse(Ctx ctx, FileMeta meta) {
		final String[] pathStr = {
				ctx.getCourse().getId(),
				"global",
				idForMeta(meta)
		};

		return new Path(pathStr);
	}

	protected Path pathForAss(Ctx ctx, String slotId, FileMeta meta) {
		final String[] pathStr = {
				ctx.getCourse().getId(),
				"assignment",
				ctx.getAssType().getId(),
				ctx.getAss().getId(),
				slotId,
				idForMeta(meta)
		};

		return new Path(pathStr);
	}

	protected Path pathForVer(Ctx ctx, String slotId, FileMeta meta) {
		final String[] pathStr = {
				ctx.getCourse().getId(),
				"version",
				ctx.getAssType().getId(),
				ctx.getAss().getId(),
				ctx.getVer().getId(),
				slotId,
				idForMeta(meta)
		};

		return new Path(pathStr);
	}

	protected Path pathForStud(Ctx ctx, String slotId, FileMeta meta) {
		final String[] pathStr = {
				ctx.getGroup().getId(),
				ctx.getStudent().getId(),
				ctx.getCourse().getId(),
				String.valueOf(ctx.getIndex()) + "-" + ctx.getAssTypeId() + "-" +
						ctx.getAss().getId() + "-" + ctx.getVer().getId(),
				slotId,
				idForMeta(meta)
		};

		return new Path(pathStr);
	}

	protected String idForMeta(FileMeta meta) {
		if (meta == null) {
			return null;
		}

		if (meta.getId() != null) {
			return meta.getId();
		}

		return Long.toString(genId(), 36);
	}

	protected Entry<FileMeta>[] findFiles(final Path path) {
		final String[][] pathElems = listCriteria(path, null, false, true, false, null);
		return load(path, pathElems[pathElems.length - 1]);
	}

	protected Entry<FileMeta>[] load(final Path path, String[] fileIds) {
		//	generic array creation, aaaaargh!
		@SuppressWarnings({"unchecked"})
		final Entry<FileMeta>[] files = (Entry<FileMeta>[]) Array.newInstance(Entry.class, fileIds.length);

		for (int i = 0; i < files.length; i++) {
			files[i] = findLast(path.setLast(fileIds[i]), null, null);
		}

		return files;
	}
}
