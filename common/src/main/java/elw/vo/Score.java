package elw.vo;

import java.util.Map;
import java.util.TreeMap;

public class Score {
	protected final Map<String, Double> ratios = new TreeMap<String, Double>();
	protected final Map<String, Integer> pows = new TreeMap<String, Integer>();
	protected long stamp;
	protected long codeStamp;

	public long getStamp() {
		return stamp;
	}

	public void setStamp(long stamp) {
		this.stamp = stamp;
	}

	public long getCodeStamp() {
		return codeStamp;
	}

	public void setCodeStamp(long codeStamp) {
		this.codeStamp = codeStamp;
	}

	public Map<String, Integer> getPows() {
		return pows;
	}

	public void setPows(Map<String, Integer> pows) {
		this.pows.clear();
		this.pows.putAll(pows);
	}

	public Map<String, Double> getRatios() {
		return ratios;
	}

	public void setRatios(Map<String, Double> ratios) {
		this.ratios.clear();
		this.ratios.putAll(ratios);
	}

	public Score copy() {
		final Score copy = new Score();

		copy.setStamp(System.currentTimeMillis());
		copy.ratios.putAll(ratios);
		copy.pows.putAll(pows);

		return copy;
	}

	public double getRatio(String[] ids) {
		double res = 1.0;

		for (final String id : ids) {
			if (!pows.containsKey(id) || !ratios.containsKey(id)) {
				continue;
			}
			res *= Math.pow(ratios.get(id), pows.get(id));
		}

		return res;
	}

	public boolean containsAll(String[] ids) {
		for (final String id : ids) {
			if (!pows.containsKey(id) || !ratios.containsKey(id)) {
				return false;
			}
		}

		return true;
	}
}
