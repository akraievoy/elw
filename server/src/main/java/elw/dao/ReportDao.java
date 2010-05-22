package elw.dao;

import elw.vo.ReportMeta;
import gnu.trove.TLongArrayList;
import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Parse;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

/**
 * Dealing with persisting, enumerating and retrieving report uploads.
 */
public class ReportDao {
	private static final Logger log = LoggerFactory.getLogger(ReportDao.class);

	protected final ObjectMapper mapper;
	protected File uploadsDir = new File(System.getProperty("user.home"), "elw-data/uploads");
	protected static final String META_POSTFIX = ".json";

	public ReportDao(ObjectMapper mapper) {
		this.mapper = mapper;
	}

	public void createReport(AssignmentPath assignmentPath, InputStream in, String remoteAddr) throws IOException {
		final File assDir = assignmentPath.getReportRoot(uploadsDir);

		if (!assDir.isDirectory() && !assDir.mkdirs()) {
			throw new IOException("failed to create dir: " + assDir.getPath());
		}

		final long uploadTime = System.currentTimeMillis();
		final File fTarget = new File(assDir, String.valueOf(uploadTime));
		final File fTargetMeta = new File(assDir, String.valueOf(uploadTime) + META_POSTFIX);

		final ReportMeta reportMeta = new ReportMeta();
		reportMeta.setSourceAddress(remoteAddr);
		reportMeta.setUploadStamp(uploadTime);

		FileOutputStream fos = null;
		try {
			synchronized (this) {
				G4Io.writeToFile(in, fTarget);

				fos = new FileOutputStream(fTargetMeta);
				final OutputStreamWriter osw = new OutputStreamWriter(fos);
				mapper.writeValue(osw, reportMeta);
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

	public long[] findAllStamps(AssignmentPath assignmentPath) {
		final File assDir = assignmentPath.getReportRoot(uploadsDir);

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

	public Map<Long, ReportMeta> findAllMetas(AssignmentPath assignmentPath) {
		final File assDir = assignmentPath.getReportRoot(uploadsDir);

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

		final TreeMap<Long, ReportMeta> result = new TreeMap<Long, ReportMeta>();
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
				final ReportMeta reportMeta = mapper.readValue(in, ReportMeta.class);

				result.put(parsed, reportMeta);
			} catch (IOException e) {
				log.warn("failed to read meta: '{}' / '{}'", assDir.getPath(), fileName);
				log.info("trace", e);
				result.put(parsed, null);
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

		return result;
	}

	public ReportMeta findMetaByStamp(AssignmentPath assignmentPath, long stamp) {
		final File assDir = assignmentPath.getReportRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return new ReportMeta();
		}

		final File fReport = new File(assDir, String.valueOf(stamp));
		final File fMeta = new File(assDir, String.valueOf(stamp) + META_POSTFIX);
		final String[] fileNames;
		final boolean reportExists;
		final boolean metaExists;
		synchronized (this) {
			fileNames = assDir.list();
			reportExists = fReport.isFile();
			metaExists = fMeta.isFile();
		}

		if (fileNames.length == 0) {
			return new ReportMeta();
		}

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

		if (!reportExists) {
			log.warn("querying non-existent upload: '{}' / '{}'", assDir.getPath(), stamp);
		}

		final ReportMeta defaultMeta = new ReportMeta();
		defaultMeta.setTotalUploads(totalUploads);
		defaultMeta.setUploadStamp(stamp);
		if (!metaExists) {
			return defaultMeta;
		}

		FileInputStream fis = null;
		try {
			fis = new FileInputStream(fMeta);
			final InputStreamReader in = new InputStreamReader(fis, "UTF-8");
			final ReportMeta reportMeta = mapper.readValue(in, ReportMeta.class);
			reportMeta.setTotalUploads(totalUploads);
			return reportMeta;
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

	public void updateMeta(AssignmentPath assignmentPath, long stamp, ReportMeta meta) {
		final File assDir = assignmentPath.getReportRoot(uploadsDir);
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

	public long findLastStamp(AssignmentPath assignmentPath) {
		final long[] stamps = findAllStamps(assignmentPath);

		return stamps.length > 0 ? stamps[stamps.length - 1] : -1;
	}

	public InputStream findLastReport(AssignmentPath assignmentPath) throws IOException {
		return findReportByStamp(assignmentPath, findLastStamp(assignmentPath));
	}

	public InputStream findReportByStamp(AssignmentPath assignmentPath, long stamp) throws IOException {
		if (stamp < 0) {
			return null;
		}

		final File assDir = assignmentPath.getReportRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return null;
		}

		final String[] fileNames = assDir.list();
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
			return null;
		}

		return new FileInputStream(new File(assDir, stampFileName));
	}
}