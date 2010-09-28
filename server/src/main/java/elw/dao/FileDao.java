package elw.dao;

import elw.vo.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.Array;

public class FileDao extends Dao<FileMeta> {
	public static final String SCOPE_COURSE = "c";
	public static final String SCOPE_ASS_TYPE = "t";
	public static final String SCOPE_ASS = "a";
	public static final String SCOPE_VER = "v";
	public static final String SCOPE_STUD = "s";

	private static final Logger log = LoggerFactory.getLogger(FileDao.class);

	protected final ScoreDao scoreDao;

	public FileDao(final ScoreDao scoreDao) {
		this.scoreDao = scoreDao;
	}

	@Override
	public Path pathFromMeta(FileMeta file) {
		return new Path(file.getPath());
	}

	public Stamp createFileFor(
			final String scope, Ctx ctx, String slotId,
			FileMeta meta, BufferedInputStream binary, BufferedReader text) throws IOException {
		return setPathAndCreate(pathFor(scope, ctx, slotId, meta), meta, binary, text);
	}

	public Entry<FileMeta> findFileFor(final String scope, Ctx ctx, String slotId, String fileId) {
		final Entry<FileMeta> lastFile = findLast(pathFor(scope, ctx, slotId, null).setLast(fileId), null, null);

		if (SCOPE_STUD.equals(scope)) {
			loadScore(ctx, slotId, fileId, lastFile);
		}

		return lastFile;
	}

	public Entry<FileMeta>[] findFilesFor(final String scope, Ctx ctx, String slotId) {
		final Entry<FileMeta>[] files = findFiles(pathFor(scope, ctx, slotId, null));

		if (SCOPE_STUD.equals(scope)) {
			for (Entry<FileMeta> file : files) {
				loadScore(ctx, slotId, file.getMeta().getId(), file);
			}
		}

		return files;
	}

	protected void loadScore(Ctx ctx, String slotId, String fileId, Entry<FileMeta> entry) {
		if (entry == null) {
			return;
		}

		final Entry<Score> score = scoreDao.findLastScore(ctx, slotId, fileId);
		if (score != null) {
			entry.getMeta().setScore(score.getMeta());
		}
	}

	protected Stamp setPathAndCreate(final Path path, FileMeta meta, BufferedInputStream binary, BufferedReader text) throws IOException {
		meta.setPath(path.getPath());
		return create(path, meta, binary, text);
	}

	protected Path pathFor(final String scope, Ctx ctx, String slotId, FileMeta meta) {
		final String[] pathStr;

		if (SCOPE_COURSE.equals(scope)) {
			pathStr = new String[] {
					ctx.getCourse().getId(),
					"global",
					idForMeta(meta)
			};
		} else if (SCOPE_ASS_TYPE.equals(scope)) {
			pathStr = new String[]{
					ctx.getCourse().getId(),
					"assType",
					ctx.getAssType().getId(),
					slotId,
					idForMeta(meta)
			};
		} else if (SCOPE_ASS.equals(scope)) {
			pathStr = new String[]{
					ctx.getCourse().getId(),
					"assignment",
					ctx.getAssType().getId(),
					ctx.getAss().getId(),
					slotId,
					idForMeta(meta)
			};
		} else if (SCOPE_VER.equals(scope)) {
			pathStr = new String[]{
					ctx.getCourse().getId(),
					"version",
					ctx.getAssType().getId(),
					ctx.getAss().getId(),
					ctx.getVer().getId(),
					slotId,
					idForMeta(meta)
			};
		} else if (SCOPE_STUD.equals(scope)) {
			pathStr = new String[] {
					ctx.getGroup().getId(),
					ctx.getStudent().getId(),
					ctx.getCourse().getId(),
					String.valueOf(ctx.getIndex()) + "-" + ctx.getAssType().getId() + "-" +
							ctx.getAss().getId() + "-" + ctx.getVer().getId(),
					slotId,
					idForMeta(meta)
			};
		} else {
			throw new IllegalArgumentException("scope: " + scope);
		}

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
