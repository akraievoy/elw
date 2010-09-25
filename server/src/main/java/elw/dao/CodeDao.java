package elw.dao;

import elw.vo.CodeMeta;
import elw.vo.Path;
import elw.vo.ReportMeta;
import elw.vo.Stamp;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;

/**
 * Dealing with persisting, enumerating and retrieving code uploads.
 */
public class CodeDao extends Dao<CodeMeta> {
	private static final Logger log = LoggerFactory.getLogger(CodeDao.class);

	protected static final String[] CODE_DEFAULT = new String[]{"#  your code", "#    goes here", "#      :)"};

	protected File uploadsDir = new File(System.getProperty("user.home"), "elw-data/uploads");
	protected static final String META_POSTFIX = ".json";

	public CodeDao(ObjectMapper mapper) {
		this.mapper = mapper;
	}

	@Override
	public Path pathFromMeta(final CodeMeta code) {
		return new Path(code.getPath());
	}

	public void createCode(Ctx ctx, BufferedReader codeReader, String remoteAddr) throws IOException {
		final CodeMeta codeMeta = new CodeMeta();
		codeMeta.setPath(toPath(ctx).getPath());
		codeMeta.setSourceAddress(remoteAddr);

		create(toPath(ctx), codeMeta, null, codeReader);
	}

	protected static Path toPath(Ctx ctx) {
		if (ctx == null || !ctx.resolved(Ctx.STATE_EGSCIV)) {
			throw new IllegalStateException("context not fully set up: " + String.valueOf(ctx));
		}
		final Path path = new Path(new String[]{
				ctx.getGroup().getId(),
				ctx.getStudent().getId(),
				ctx.getCourse().getId(),
				String.valueOf(ctx.getAssTypeId()),
				ctx.getAss().getId(),
				ctx.getVer().getId(),
		});

		return path;
	}

	protected void resolveNames(Ctx path, final Collection<Entry<CodeMeta>> codeEntries) {
		for (Entry<CodeMeta> codeEntry : codeEntries) {
			final CodeMeta codeMeta = codeEntry.getMeta();
			final String fileName =
					path.getStudent().getName().replaceAll("\\s+", "_") + "--" +
					path.getAss().getId() + "_" + path.getVer().getId() + "--" +
					ReportMeta.getFileNameUploadStamp(codeMeta.getCreateStamp().getTime()) +
					".txt";

			codeMeta.setFileName(fileName);
		}
	}

	public Map<Stamp, Entry<CodeMeta>> findAllMetas(Ctx ctx) {
		final SortedMap<Stamp, Entry<CodeMeta>> allMetas = findAll(toPath(ctx), true, null);

		resolveNames(ctx, allMetas.values());

		return allMetas;
	}

	public Entry<CodeMeta> findMetaByStamp(Ctx ctx, Stamp stamp) {
		Entry<CodeMeta> entry = super.findByStamp(toPath(ctx), stamp, true, null);

		if (entry != null) {
			resolveNames(ctx, Collections.singletonList(entry));
		}

		return entry;
	}

	public Entry<CodeMeta> findLast(Ctx ctx) {
		final Entry<CodeMeta> last = findLast(toPath(ctx), true, null);

		if (last != null) {
			resolveNames(ctx, Collections.singletonList(last));
		}

		return last;
	}

	//	TODO re-wire totalUploads method
	public int countTotalUploads(Ctx ctx) {
		return countVersions(toPath(ctx), null, null, true, null);
	}

	public void updateMeta(Ctx ctx, Stamp stamp, CodeMeta meta) throws IOException {
		update(toPath(ctx), meta, null, null);
	}
}
