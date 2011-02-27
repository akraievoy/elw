package elw.dao;

import base.pattern.Result;
import com.google.common.base.Strings;
import com.google.common.io.ByteStreams;
import com.google.common.io.Closeables;
import com.google.common.io.InputSupplier;
import elw.vo.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

public class FileDao extends Dao<FileMeta> {
	public static final String SCOPE_ASS_TYPE = "t";
	public static final String SCOPE_ASS = "a";
	public static final String SCOPE_VER = "v";
	public static final String SCOPE_STUD = "s";

	private static final Logger log = LoggerFactory.getLogger(FileDao.class);

	private final ScoreDao scoreDao;
	public static final String[] SCOPES = new String[] {FileDao.SCOPE_ASS_TYPE, FileDao.SCOPE_ASS, FileDao.SCOPE_VER, FileDao.SCOPE_STUD};
	protected static final int DETECT_SIZE_LIMIT = 1024 * 1024 * 1024;

	public FileDao(final ScoreDao scoreDao) {
		this.scoreDao = scoreDao;
	}

	@Override
	public Path pathFromMeta(FileMeta file) {
		return new Path(file.getPath());
	}

	public Result createFileFor(
			String scope, Ctx ctx, FileSlot slot, FileMeta fileMeta,
			long length, InputSupplier<InputStream> is
	) throws IOException {
		//	LATER this means that all files which are greater than limit are binary (which makes some sense, but...)
		boolean binaryEffective = slot.isBinary() || length >= DETECT_SIZE_LIMIT;

		final InputSupplier<? extends InputStream> isEffective;
		if (!slot.isBinary() && length < DETECT_SIZE_LIMIT) {
			final byte[] bytes = ByteStreams.toByteArray(is);
			//	implemented as per this SO answer:
			//	http://stackoverflow.com/questions/277521/identify-file-binary/277568#277568
			for (byte b : bytes) {
				if (b >= 0 && b < 9 || b > 13 && b < 32) {
					binaryEffective = true;
					break;
				}
			}
			isEffective = ByteStreams.newInputStreamSupplier(bytes);
		} else {
			isEffective = is;
		}

		if (!slot.isBinaryAllowed() &&  binaryEffective) {
			return new Result("binary files not allowed", false);
		}

		if (!slot.isTextAllowed() && !binaryEffective) {
			return new Result("text files not allowed", false);
		}

		if (Strings.isNullOrEmpty(fileMeta.getContentType())) {
			fileMeta.setContentType(binaryEffective ? "application/octet-stream" : "text/plain");
		}
		if (Strings.isNullOrEmpty(fileMeta.getName())) {
			fileMeta.setContentType(binaryEffective ? "upload.bin" : "upload.txt");
		}
		fileMeta.setBinary(binaryEffective);

		InputStream input = null;
		try {
			input = isEffective.getInput();

			//	TODO: there's no added value in separating files this way
			final BufferedInputStream streamBinary = binaryEffective ? new BufferedInputStream(input) : null;
			final BufferedReader streamText = binaryEffective ? null : new BufferedReader(new InputStreamReader(input, "UTF-8"));

			createFileFor(scope, ctx, slot.getId(), fileMeta, streamBinary, streamText);
			return new Result("file created", true);
		} finally {
			Closeables.closeQuietly(input);
		}
	}

	private Stamp createFileFor(
			final String scope, Ctx ctx, String slotId,
			FileMeta meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
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

	private void loadScore(Ctx ctx, String slotId, String fileId, Entry<FileMeta> entry) {
		if (entry == null) {
			return;
		}

		final Entry<Score> score = scoreDao.findLastScore(ctx, slotId, fileId);
		if (score != null) {
			entry.getMeta().setScore(score.getMeta());
		}
	}

	private Stamp setPathAndCreate(final Path path, FileMeta meta, BufferedInputStream binary, BufferedReader text) throws IOException {
		meta.setPath(path.getPath());
		return create(path, meta, binary, text);
	}

	private Path pathFor(final String scope, Ctx ctx, String slotId, FileMeta meta) {
		final String[] pathStr;

		if (SCOPE_ASS_TYPE.equals(scope)) {
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

	private String idForMeta(FileMeta meta) {
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

	private Entry<FileMeta>[] findFiles(final Path path) {
		final String[][] pathElems = listCriteria(path);
		return load(path, pathElems[pathElems.length - 1]);
	}

	private Entry<FileMeta>[] load(final Path path, String[] fileIds) {
		//	generic array creation, aaaaargh!
		@SuppressWarnings({"unchecked"})
		final Entry<FileMeta>[] files = (Entry<FileMeta>[]) Array.newInstance(Entry.class, fileIds.length);

		for (int i = 0; i < files.length; i++) {
			files[i] = findLast(path.setLast(fileIds[i]), null, null);
		}

		sortByCreateStamp(files);

		return files;
	}

	public SortedMap<String, List<Entry<FileMeta>>> loadFilesStud(Ctx ctxAss) {
		final TreeMap<String, List<Entry<FileMeta>>> slotIdToFiles = new TreeMap<String, List<Entry<FileMeta>>>();

		for (FileSlot slot : ctxAss.getAssType().getFileSlots()) {
			final Entry<FileMeta>[] filesStud = findFilesFor(SCOPE_STUD, ctxAss, slot.getId());
			slotIdToFiles.put(slot.getId(), Arrays.asList(filesStud));
		}

		return slotIdToFiles;
	}
}
