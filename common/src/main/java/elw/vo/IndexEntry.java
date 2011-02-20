package elw.vo;

import org.akraievoy.gear.G4mat;
import org.codehaus.jackson.annotate.JsonIgnore;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class IndexEntry {
	private String[] path;
	private int scoreBudget;
	private int classFrom;
	private final Map<String, Integer> classDue = new TreeMap<String, Integer>();
	private boolean requireClean;	//	TODO still not used

	public boolean isRequireClean() {
		return requireClean;
	}

	public void setRequireClean(boolean requireClean) {
		this.requireClean = requireClean;
	}

	public int getClassFrom() {
		return classFrom;
	}

	public void setClassFrom(int classFrom) {
		this.classFrom = classFrom;
	}

	public int getScoreBudget() {
		return scoreBudget;
	}

	public void setScoreBudget(int scoreBudget) {
		this.scoreBudget = scoreBudget;
	}

	public String[] getPath() {
		return path;
	}

	public void setPath(String[] path) {
		this.path = path;
	}

	public Map<String, Integer> getClassDue() {
		return Collections.unmodifiableMap(classDue);
	}

	public void setClassDue(Map<String, Integer> classDue) {
		this.classDue.clear();
		if (classDue != null) {
			this.classDue.putAll(classDue);
		}
	}

	//	TODO pass Ctx to this place somehow
	public String normName(final Enrollment enr, final Student stud, final Assignment ass, final Version ver,
						   final FileSlot slot, final FileMeta meta, final Format format) {
		try {
			final String normName = enr.getName() + "-" + stud.getName() + "--" +
					ass.getName() + (ver == null ? "" : "-" + ver.getName()) + "--" +
					slot.getName() + "-" + format.format(meta.getCreateStamp().getTime(), "MMdd-HHmm");

			final String oriName = meta.getName();
			final int oriLastDot = oriName.lastIndexOf(".");
			final String oriExt = oriLastDot < 0 ? "" : oriName.substring(oriLastDot);

			final String normNameNoWs = normName.replaceAll("[\\s\\\\/]+", "_") + oriExt;

			return URLEncoder.encode(
					normNameNoWs,
					"UTF-8"
			);
		} catch (UnsupportedEncodingException e) {
			throw new IllegalStateException("UTF-8 is NOT supported?!");
		}
	}

	@JsonIgnore
	public double computePoints(Score score, final FileSlot slot) {
		if (Boolean.FALSE.equals(score.getApproved())) {
			return 0.0;
		}
		return getScoreBudget() * slot.getScoreWeight() * score.computeRatio(slot);
	}

	@JsonIgnore
	public double computePoints(final FileSlot slot) {
		return getScoreBudget() * slot.getScoreWeight();
	}

	@JsonIgnore
	public double getTotal(final AssignmentType aType, Score score) {
		double result = 0.0;

		for (FileSlot slot : aType.getFileSlots()) {
			if (!Boolean.TRUE.equals(score.getApproved())) {
				continue;
			}

			result += computePoints(score, slot);
		}

		return result;
	}

}
