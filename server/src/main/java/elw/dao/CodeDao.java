package elw.dao;

import elw.vo.CodeMeta;
import gnu.trove.TLongArrayList;
import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Parse;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

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

	public void createCode(AssignmentPath assignmentPath, BufferedReader codeReader) throws IOException {
		final File assDir = assignmentPath.getCodeRoot(uploadsDir);

		if (!assDir.isDirectory() && !assDir.mkdirs()) {
			throw new IOException("failed to create dir: " + assDir.getPath());
		}

		final File targetFile = new File(assDir, String.valueOf(System.currentTimeMillis()));
		synchronized (this) {
			G4Io.writeToFile(codeReader, targetFile);
		}
	}

	public long[] findAllStamps(AssignmentPath assignmentPath) {
		final File assDir = assignmentPath.getCodeRoot(uploadsDir);

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

	public SortedMap<Long, CodeMeta> findAllMetas(AssignmentPath assignmentPath) {
		final File assDir = assignmentPath.getCodeRoot(uploadsDir);

		final TreeMap<Long, CodeMeta> result = new TreeMap<Long, CodeMeta>();
		if (!assDir.isDirectory()) {
			return result;
		}

		final String[] fileNames;
		synchronized (this) {
			fileNames = assDir.list();
		}

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
				result.put(parsed, null);
				continue;
			}

			FileInputStream fis = null;
			try {
				fis = new FileInputStream(metaFile);
				final InputStreamReader in = new InputStreamReader(fis, "UTF-8");
				final CodeMeta codeMeta = mapper.readValue(in, CodeMeta.class);

				result.put(parsed, codeMeta);
			} catch (IOException e) {
				log.warn("failed to read meta: '{}' / '{}'", assDir.getPath(), fileName);
				log.info("trace", e);
				result.put(parsed, null);
			} finally {
				G4Io.silentClose(fis);
			}
		}

		return result;
	}

	public void updateMeta(AssignmentPath assignmentPath, long stamp, CodeMeta meta) {
		final File assDir = assignmentPath.getCodeRoot(uploadsDir);
		final File metaFile = new File(assDir, stamp + META_POSTFIX);

		FileOutputStream fos = null;
		try {
			synchronized (this) {
				fos = new FileOutputStream(metaFile);
				mapper.writeValue(new OutputStreamWriter(fos), meta);
				fos.flush();
			}
		} catch (IOException e) {
			log.warn("failed to write meta: '{}' / '{}'", assDir.getPath(), metaFile.getName());
			log.info("trace", e);
		} finally {
			G4Io.silentClose(fos);
		}
	}

	public long findLastStamp(AssignmentPath assignmentPath) {
		final long[] stamps = findAllStamps(assignmentPath);

		return stamps.length > 0 ? stamps[stamps.length - 1] : -1;
	}

	public String[] findLastCode(AssignmentPath assignmentPath) throws IOException {
		return findCodeByStamp(assignmentPath, findLastStamp(assignmentPath));
	}

	public String[] findCodeByStamp(AssignmentPath assignmentPath, long stamp) throws IOException {
		if (stamp < 0) {
			return CODE_DEFAULT;
		}

		final File assDir = assignmentPath.getCodeRoot(uploadsDir);

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
