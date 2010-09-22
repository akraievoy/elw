package elw.dao;

import elw.vo.Score;
import elw.vo.Stamp;
import org.akraievoy.gear.G4Parse;
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
	public Path pathFromMeta(Score stamped) {
		return new Path(stamped.getPath());
	}

	public Stamp createScore(Ctx ctx, Score score) throws IOException {
		final Path path = CodeDao.toPath(ctx);
		score.setPath(path.getPath());

		return create(path, score, null, null);
	}

	public Map<Stamp, Entry<Score>> findAllScores(Ctx ctx) {
		return findAllMetas(CodeDao.toPath(ctx), false, false);
	}

	public Entry<Score> findScoreByStamp(Ctx ctx, Stamp stamp) {
		return findByStamp(CodeDao.toPath(ctx), stamp, false, false);
	}

	public Entry<Score> findLastScore(Ctx ctx) {
		return findLast(CodeDao.toPath(ctx), false, false);
	}
}