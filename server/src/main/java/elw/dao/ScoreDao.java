package elw.dao;

import elw.vo.Score;
import gnu.trove.TLongArrayList;
import org.akraievoy.gear.G4Parse;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;

/**
 * Dealing with persisting, enumerating and retrieving scoring objects.
 */
public class ScoreDao extends Dao {
	private static final Logger log = LoggerFactory.getLogger(ScoreDao.class);

	protected static final String META_POSTFIX = ".json";

	protected final ObjectMapper mapper;
	protected File uploadsDir = new File(System.getProperty("user.home"), "elw-data/uploads");

	public ScoreDao(ObjectMapper mapper) {
		this.mapper = mapper;
	}

	public long createScore(Ctx ctx, Score score) throws IOException {
		final File assDir = ctx.getScoreRoot(uploadsDir);

		if (!assDir.isDirectory() && !assDir.mkdirs()) {
			throw new IOException("failed to create dir: " + assDir.getPath());
		}

		final long updateStamp = System.currentTimeMillis();
		final File fScore = new File(assDir, String.valueOf(updateStamp) + META_POSTFIX);

		score.setStamp(updateStamp);

		FileOutputStream fos = null;
		try {
			synchronized (this) {
				fos = new FileOutputStream(fScore);
				final OutputStreamWriter osw = new OutputStreamWriter(fos);
				mapper.writeValue(osw, score);
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

		return updateStamp;
	}

	public long[] findAllStamps(Ctx ctx) {
		final File assDir = ctx.getScoreRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return new long[0];
		}

		final String[] fileNames = assDir.list();
		final TLongArrayList stamps = new TLongArrayList();

		for (String fileName : fileNames) {
			if (!fileName.endsWith(META_POSTFIX)) {
				continue;
			}

			final long parsed = G4Parse.parse(fileName.substring(0, fileName.length() - META_POSTFIX.length()), -1L);

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

	public Map<Long, Score> findAllScores(Ctx ctx) {
		final File assDir = ctx.getScoreRoot(uploadsDir);

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

		final TreeMap<Long, Score> result = new TreeMap<Long, Score>();
		for (String fileName : fileNames) {
			if (!fileName.endsWith(META_POSTFIX)) {
				continue;
			}

			final long parsed = G4Parse.parse(fileName.substring(0, fileName.length() - META_POSTFIX.length()), -1L);

			if (parsed < 0) {
				log.warn("stamp parse error: '{}' / '{}'", assDir.getPath(), fileName);
				continue;
			}
			if (result.keySet().contains(parsed)) {
				log.warn("duped stamps: '{}' / '{}'", assDir.getPath(), fileName);
				continue;
			}

			final File scoreFile = new File(assDir, fileName);
			if (!scoreFile.isFile()) {
				result.put(parsed, null);
				continue;
			}

			FileInputStream fis = null;
			try {
				fis = new FileInputStream(scoreFile);
				final InputStreamReader in = new InputStreamReader(fis, "UTF-8");
				final Score score = mapper.readValue(in, Score.class);

				result.put(parsed, score);
			} catch (IOException e) {
				log.warn("failed to read score: '{}' / '{}'", assDir.getPath(), fileName);
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

	public Score findScoreByStamp(Ctx ctx, long stamp) {
		final File assDir = ctx.getScoreRoot(uploadsDir);

		if (!assDir.isDirectory()) {
			return null;
		}

		final File fScore = new File(assDir, String.valueOf(stamp) + META_POSTFIX);
		final String[] fileNames;
		final boolean scoreExists;
		synchronized (this) {
			fileNames = assDir.list();
			scoreExists = fScore.isFile();
		}

		if (fileNames.length == 0 || !scoreExists) {
			return null;
		}

		FileInputStream fis = null;
		try {
			fis = new FileInputStream(fScore);
			final InputStreamReader in = new InputStreamReader(fis, "UTF-8");
			final Score score = mapper.readValue(in, Score.class);
			return score;
		} catch (IOException e) {
			log.warn("failed to read score: '{}' / '{}'", assDir.getPath(), fScore.getName());
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

		return null;
	}

	public long findLastStamp(Ctx ctx) {
		final long[] stamps = findAllStamps(ctx);

		return stamps.length > 0 ? stamps[stamps.length - 1] : -1;
	}

	public Score findLastScore(Ctx ctx) {
		return findScoreByStamp(ctx, findLastStamp(ctx));
	}
}