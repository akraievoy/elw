package elw.vo;

import java.util.Map;

public class IndexEntry {
	protected String[] path;
	protected int scoreBudget;
	protected int classFrom;
	protected Map<String, Integer> classDue;
	protected boolean requireClean;

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
		return classDue;
	}

	public void setClassDue(Map<String, Integer> classDue) {
		this.classDue = classDue;
	}
}
