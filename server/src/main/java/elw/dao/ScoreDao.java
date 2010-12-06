package elw.dao;

import elw.vo.Entry;
import elw.vo.Path;
import elw.vo.Score;
import elw.vo.Stamp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;

/**
 * Dealing with persisting, enumerating and retrieving scoring objects.
 */
public class ScoreDao extends Dao<Score> {
	private static final Logger log = LoggerFactory.getLogger(ScoreDao.class);

	public ScoreDao() {
	}

	@Override
	public Path pathFromMeta(Score score) {
		return new Path(score.getPath());
	}

	public Stamp createScore(Ctx ctx, final String slotId, final String fileId, Score score) throws IOException {
		final Path path = toPath(ctx, slotId, fileId);
		score.setPath(path.getPath());
		return create(path, score, null, null);
	}

	public SortedMap<Stamp, Entry<Score>> findAllScores(Ctx ctx, final String slotId, final String fileId) {
		return findAll(toPath(ctx, slotId, fileId), false, false);
	}

	public Entry<Score> findScoreByStamp(Ctx ctx, Stamp stamp, final String slotId, final String fileId) {
		return findByStamp(toPath(ctx, slotId, fileId), stamp, false, false);
	}

	public Entry<Score> findLastScore(Ctx ctx, final String slotId, final String fileId) {
		return findLast(toPath(ctx, slotId, fileId), false, false);
	}

	protected Path toPath(Ctx ctx, final String slotId, final String fileId) {
		if (ctx == null || !ctx.resolved(Ctx.STATE_EGSCIV)) {
			throw new IllegalStateException("context not fully set up: " + String.valueOf(ctx));
		}

		final Path path = new Path(new String[]{
				ctx.getGroup().getId(),
				ctx.getStudent().getId(),
				ctx.getCourse().getId(),
				String.valueOf(ctx.getIndex()) + "-" +
				ctx.getAssTypeId() + "-" +
				ctx.getAss().getId() + "-" +
				ctx.getVer().getId(),
				slotId,
				fileId
		});

		return path;
	}
}