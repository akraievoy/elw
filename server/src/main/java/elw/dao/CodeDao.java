package elw.dao;

import gnu.trove.TLongArrayList;
import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Parse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Dealing with persisting, enumerating and retrieving code uploads.
 */
public class CodeDao {
	private static final Logger log = LoggerFactory.getLogger(CodeDao.class);

	protected static final String[] CODE_DEFAULT = new String[]{"#  your code", "#    goes here", "#      :)"};

	protected File uploadsDir = new File(System.getProperty("user.home"), "elw-data/uploads");

	public void createCode(AssignmentPath assignmentPath, BufferedReader codeReader) throws IOException {
		final File assDir = assignmentPath.getRoot(uploadsDir);

		if (!assDir.isDirectory() && !assDir.mkdirs()) {
			throw new IOException("failed to create dir: " + assDir.getPath());
		}

		final File targetFile = new File(assDir, String.valueOf(System.currentTimeMillis()));
		G4Io.writeToFile(codeReader, targetFile);
	}

	public long[] findAllStamps(AssignmentPath assignmentPath) {
		final File assDir = assignmentPath.getRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return new long[0];
		}

		final String[] fileNames = assDir.list();
		final TLongArrayList stamps = new TLongArrayList();

		for (String fileName : fileNames) {
			final long parsed;
			final int dotIdx = fileName.indexOf(".");
			if (dotIdx < 0) {
				parsed = G4Parse.parse(fileName, -1L);
			} else {
				parsed = G4Parse.parse(fileName.substring(0, dotIdx), -1L);
			}

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

		final File assDir = assignmentPath.getRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return CODE_DEFAULT;
		}

		final String[] fileNames = assDir.list();
		String stampFileName = null;

		for (String fileName : fileNames) {
			final long parsed;
			final int dotIdx = fileName.indexOf(".");
			if (dotIdx < 0) {
				parsed = G4Parse.parse(fileName, -1L);
			} else {
				parsed = G4Parse.parse(fileName.substring(0, dotIdx), -1L);
			}

			if (parsed == stamp) {
				stampFileName = fileName;
			}
		}

		if (stampFileName == null) {
			return CODE_DEFAULT;
		}

		final List<String> codeList = new ArrayList<String>();
		FileReader reader = null;
		try {
			reader = new FileReader(new File(assDir, stampFileName));
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
