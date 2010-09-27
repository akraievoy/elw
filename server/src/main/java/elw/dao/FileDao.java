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

	protected final ScoreDao scoreDao;

	public FileDao(final ScoreDao scoreDao) {
		this.scoreDao = scoreDao;
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
		return setPathAndCreate(pathForCourse(ctx, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForCourse(Ctx ctx, final String fileId) {
		return findLast(pathForCourse(ctx, null).setLast(fileId), null, null);
	}


	public Entry<FileMeta>[] findFilesForAssType(final Ctx ctx, final String slotId) {
		return findFiles(pathForAssType(ctx, slotId, null));
	}

	public Stamp createFileForAssType(
			Ctx ctx, String slotId, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return setPathAndCreate(pathForAssType(ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForAssType(Ctx ctx, final String slotId, final String fileId) {
		return findLast(pathForAssType(ctx, slotId, null).setLast(fileId), null, null);
	}

	public Entry<FileMeta>[] findFilesForAss(final Ctx ctx, final String slotId) {
		return findFiles(pathForAss(ctx, slotId, null));
	}

	public Stamp createFileForAss(
			Ctx ctx, String slotId, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return setPathAndCreate(pathForAss(ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForAss(Ctx ctx, final String slotId, final String fileId) {
		return findLast(pathForAss(ctx, slotId, null).setLast(fileId), null, null);
	}

	public Entry<FileMeta>[] findFilesForVersion(final Ctx ctx, final String slotId) {
		return findFiles(pathForVer(ctx, slotId, null));
	}

	public Stamp createFileForVersion(
			Ctx ctx, String slotId, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return setPathAndCreate(pathForVer(ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForVersion(Ctx ctx, final String slotId, final String fileId) {
		return findLast(pathForVer(ctx, slotId, null).setLast(fileId), null, null);
	}


	public Entry<FileMeta>[] findFilesForStudent(final Ctx ctx, final String slotId) {
		final Entry<FileMeta>[] files = findFiles(pathForStud(ctx, slotId, null));

		for (Entry<FileMeta> file : files) {
			loadScore(ctx, slotId, file.getMeta().getId(), file);
		}

		return files;
	}

	public Stamp createFileForStudent(
			Ctx ctx, String slotId, FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		return setPathAndCreate(pathForStud(ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileForStudent(Ctx ctx, final String slotId, final String fileId) {
		final Entry<FileMeta> entry = findLast(pathForStud(ctx, slotId, null).setLast(fileId), null, null);

		loadScore(ctx, slotId, fileId, entry);

		return entry;
	}

	protected void loadScore(Ctx ctx, String slotId, String fileId, Entry<FileMeta> entry) {
		final Entry<Score> score = scoreDao.findLastScore(ctx, slotId, fileId);
		if (score != null) {
			entry.getMeta().setScore(score.getMeta());
		}
	}

	protected Stamp setPathAndCreate(final Path path, FileMeta meta, BufferedInputStream binary, BufferedReader text) throws IOException {
		meta.setPath(path.getPath());
		return create(path, meta, binary, text);
	}

	protected Path pathForCourse(Ctx ctx, FileMeta meta) {
		final String[] pathStr = {
				ctx.getCourse().getId(),
				"global",
				idForMeta(meta)
		};

		return new Path(pathStr);
	}

	protected Path pathForAssType(Ctx ctx, String slotId, FileMeta meta) {
		final String[] pathStr = {
				ctx.getCourse().getId(),
				"assType",
				ctx.getAssType().getId(),
				slotId,
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

		final String id = Long.toString(genId(), 36);

		meta.setId(id);

		return id;
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

		sortByCreateStamp(files);

		return files;
	}
}
