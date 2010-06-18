package elw.dao;

import elw.vo.CodeMeta;
import elw.vo.ReportMeta;
import gnu.trove.TLongArrayList;
import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Parse;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;

/**
 * Dealing with persisting, enumerating and retrieving code uploads.
 */
public class CodeDao {
	private static final Logger log = LoggerFactory.getLogger(CodeDao.class);

	protected static final String[] CODE_DEFAULT = new String[]{"#  your code", "#    goes here", "#      :)"};

	protected final ObjectMapper mapper;
	protected File uploadsDir = new File(System.getProperty("user.home"), "elw-data/uploads");
	protected static final String META_POSTFIX = ".json";

	public CodeDao(ObjectMapper mapper) {
		this.mapper = mapper;
	}

	public void createCode(Ctx ctx, BufferedReader codeReader, String remoteAddr) throws IOException {
		final File assDir = ctx.getCodeRoot(uploadsDir);

		if (!assDir.isDirectory() && !assDir.mkdirs()) {
			throw new IOException("failed to create dir: " + assDir.getPath());
		}

		final long uploadTime = System.currentTimeMillis();
		final File fTarget = new File(assDir, String.valueOf(uploadTime));
		final File fTargetMeta = new File(assDir, String.valueOf(uploadTime) + META_POSTFIX);

		final CodeMeta codeMeta = new CodeMeta();
		codeMeta.setSourceAddress(remoteAddr);
		codeMeta.setUploadStamp(uploadTime);

		FileOutputStream fos = null;
		try {
			synchronized (this) {
				G4Io.writeToFile(codeReader, fTarget);

				fos = new FileOutputStream(fTargetMeta);
				final OutputStreamWriter osw = new OutputStreamWriter(fos);
				mapper.writeValue(osw, codeMeta);
			}
		} finally {
			if (fos != null) {
				try {
					fos.close();
				} catch (IOException e) {
					log.info("failed on close", e);
				}
			}
		}
	}

	public long[] findAllStamps(Ctx ctx) {
		final File assDir = ctx.getCodeRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return new long[0];
		}

		final String[] fileNames = assDir.list();
		final TLongArrayList stamps = new TLongArrayList();

		for (String fileName : fileNames) {
			if (fileName.endsWith(META_POSTFIX)) {
				continue;
			}
			final long parsed = G4Parse.parse(fileName, -1L);

			if (parsed >= 0) {
				if (!stamps.contains(parsed)) {
					stamps.add(parsed);
				} else {
					log.warn("duped stamps: '{}' / '{}'", assDir.getPath(), fileName);
				}
			} else {
				log.warn("stamp parse error: '{}' / '{}'", assDir.getPath(), fileName);
			}
		}

		stamps.sort();

		return stamps.toNativeArray();
	}

	protected void resolveNames(Ctx path, final Collection<CodeMeta> reportMetas) {
		for (Iterator<CodeMeta> metaIt = reportMetas.iterator(); metaIt.hasNext();) {
			CodeMeta codeMeta = metaIt.next();
			final String fileName =
					path.getStudent().getName().replaceAll("\\s+", "_") + "--" +
					path.getAss().getId() + "_" + path.getVer().getId() + "--" +
					ReportMeta.getFileNameUploadStamp(codeMeta.getUploadStamp()) +
					".txt";

			codeMeta.setFileName(fileName);
		}
	}

	public Map<Long, CodeMeta> findAllMetas(Ctx ctx) {
		final File assDir = ctx.getCodeRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return Collections.emptyMap();
		}

		final String[] fileNames;
		synchronized (this) {
			fileNames = assDir.list();
		}
		if (fileNames.length == 0) {
			return Collections.emptyMap();
		}

		final int totalUploads = countTotalUploads(assDir, fileNames);
		final TreeMap<Long, CodeMeta> result = new TreeMap<Long, CodeMeta>();
		for (String fileName : fileNames) {
			if (fileName.endsWith(META_POSTFIX)) {
				continue;
			}

			final long parsed = G4Parse.parse(fileName, -1L);

			if (parsed < 0) {
				log.warn("stamp parse error: '{}' / '{}'", assDir.getPath(), fileName);
				continue;
			}
			if (result.keySet().contains(parsed)) {
				log.warn("duped stamps: '{}' / '{}'", assDir.getPath(), fileName);
				continue;
			}

			final File metaFile = new File(assDir, fileName + META_POSTFIX);
			if (!metaFile.isFile()) {
				final CodeMeta defaultMeta = new CodeMeta();
				defaultMeta.setTotalUploads(totalUploads);
				defaultMeta.setUploadStamp(parsed);
				result.put(parsed, defaultMeta);
				continue;
			}

			FileInputStream fis = null;
			try {
				fis = new FileInputStream(metaFile);
				final InputStreamReader in = new InputStreamReader(fis, "UTF-8");
				final CodeMeta codeMeta = mapper.readValue(in, CodeMeta.class);
				codeMeta.setTotalUploads(totalUploads);
				result.put(parsed, codeMeta);
			} catch (IOException e) {
				log.warn("failed to read meta: '{}' / '{}'", assDir.getPath(), fileName);
				log.info("trace", e);
			} finally {
				if (fis != null) {
					try {
						fis.close();
					} catch (IOException e) {
						log.info("failed on close", e);
					}
				}
			}
		}

		resolveNames(ctx, result.values());

		return result;
	}

	public CodeMeta findMetaByStamp(Ctx ctx, long stamp) {
		final File assDir = ctx.getCodeRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return new CodeMeta();
		}

		final File fCode = new File(assDir, String.valueOf(stamp));
		final File fMeta = new File(assDir, String.valueOf(stamp) + META_POSTFIX);
		final String[] fileNames;
		final boolean codeExists;
		final boolean metaExists;
		synchronized (this) {
			fileNames = assDir.list();
			codeExists = fCode.isFile();
			metaExists = fMeta.isFile();
		}

		if (fileNames.length == 0) {
			return new CodeMeta();
		}

		final int totalUploads = countTotalUploads(assDir, fileNames);

		if (!codeExists) {
			log.warn("querying non-existent upload: '{}' / '{}'", assDir.getPath(), stamp);
		}

		final CodeMeta defaultMeta = new CodeMeta();
		defaultMeta.setTotalUploads(totalUploads);
		defaultMeta.setUploadStamp(stamp);
		resolveNames(ctx, Collections.singletonList(defaultMeta));
		if (!metaExists) {
			return defaultMeta;
		}

		FileInputStream fis = null;
		try {
			fis = new FileInputStream(fMeta);
			final InputStreamReader in = new InputStreamReader(fis, "UTF-8");
			final CodeMeta codeMeta = mapper.readValue(in, CodeMeta.class);
			codeMeta.setTotalUploads(totalUploads);
			resolveNames(ctx, Collections.singletonList(codeMeta));
			return codeMeta;
		} catch (IOException e) {
			log.warn("failed to read meta: '{}' / '{}'", assDir.getPath(), fMeta.getName());
			log.info("trace", e);
		} finally {
			if (fis != null) {
				try {
					fis.close();
				} catch (IOException e) {
					log.info("failed on close", e);
				}
			}
		}

		return defaultMeta;
	}

	protected int countTotalUploads(File assDir, String[] fileNames) {
		int totalUploads = 0;
		for (String fileName : fileNames) {
			if (fileName.endsWith(META_POSTFIX)) {
				continue;
			}

			final long parsed = G4Parse.parse(fileName, -1L);

			if (parsed < 0) {
				log.warn("stamp parse error: '{}' / '{}'", assDir.getPath(), fileName);
				continue;
			}
			totalUploads++;
		}
		return totalUploads;
	}

	public void updateMeta(Ctx ctx, long stamp, CodeMeta meta) {
		final File assDir = ctx.getCodeRoot(uploadsDir);
		final File metaFile = new File(assDir, stamp + META_POSTFIX);

		FileOutputStream fos = null;
		try {
			synchronized (this) {
				fos = new FileOutputStream(metaFile);
				final OutputStreamWriter osw = new OutputStreamWriter(fos);
				mapper.writeValue(osw, meta);
			}
		} catch (IOException e) {
			log.warn("failed to write meta: '{}' / '{}'", assDir.getPath(), metaFile.getName());
			log.info("trace", e);
		} finally {
			if (fos != null) {
				try {
					fos.close();
				} catch (IOException e) {
					log.info("failed on close", e);
				}
			}
		}
	}

	public long findLastStamp(Ctx ctx) {
		final long[] stamps = findAllStamps(ctx);

		return stamps.length > 0 ? stamps[stamps.length - 1] : -1;
	}

	public String[] findLastCode(Ctx ctx) throws IOException {
		return findCodeByStamp(ctx, findLastStamp(ctx));
	}

	public String[] findCodeByStamp(Ctx ctx, long stamp) throws IOException {
		if (stamp < 0) {
			return CODE_DEFAULT;
		}

		final File assDir = ctx.getCodeRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return CODE_DEFAULT;
		}

		final String[] fileNames;
		synchronized (this) {
			fileNames = assDir.list();
		}
		String stampFileName = null;

		for (String fileName : fileNames) {
			if (fileName.endsWith(META_POSTFIX)) {
				continue;
			}
			final long parsed = G4Parse.parse(fileName, -1L);

			if (parsed == stamp) {
				stampFileName = fileName;
			}
		}

		if (stampFileName == null) {
			return CODE_DEFAULT;
		}

		final List<String> codeList = new ArrayList<String>();
		Reader reader = null;
		try {
			reader = new InputStreamReader(new FileInputStream(new File(assDir, stampFileName)), "UTF-8");
			final BufferedReader codeIn = new BufferedReader(reader);
			String codeLine;
			while ((codeLine = codeIn.readLine()) != null) {
				codeList.add(codeLine);
			}
		} finally {
			if (reader != null) {
				try {
					reader.close();
				} catch (IOException e) {
					log.debug("failed on close", e);
				}
			}
		}

		return codeList.toArray(new String[codeList.size()]);
	}
}
