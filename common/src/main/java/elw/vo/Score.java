package elw.vo;

import org.akraievoy.gear.G4mat;
import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.*;

public class Score {
	protected final Map<String, Double> ratios = new TreeMap<String, Double>();
	protected final Map<String, Integer> pows = new TreeMap<String, Integer>();
	protected long stamp;
	protected long codeStamp;
	protected long reportStamp;

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

	public long getReportStamp() {
		return reportStamp;
	}

	public void setReportStamp(long reportStamp) {
		this.reportStamp = reportStamp;
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

	@JsonIgnore
	public double getRatio() {
		final Set<String> ratioKeys = ratios.keySet();
		return getRatio(ratioKeys.toArray(new String[ratios.size()]));
	}

	@JsonIgnore
	public String getNiceRatio(TypeScoring typeScoring, TaskScoring taskScoring) {
		return G4mat.format2(taskScoring.getScoreBudget() * typeScoring.getWeight() * getRatio(typeScoring.getApplied()));
	}

	public double getRatio(String[] ids) {
		double res = 1.0;

		for (final String id : ids) {
			if (!contains(id)) {
				continue;
			}
			res *= Math.pow(ratios.get(id), pows.get(id));
		}

		return res;
	}

	public boolean contains(String id) {
		return pows.containsKey(id) && ratios.containsKey(id);
	}

	public boolean containsAll(String[] ids) {
		for (final String id : ids) {
			if (!contains(id)) {
				return false;
			}
		}

		return true;
	}

	public boolean isSetTo(String id, int pow) {
		final Integer myPow = pows.get(id);
		return myPow != null && myPow.equals(pow);
	}

	@JsonIgnore
	public Term[] getTerms(TypeScoring ts) {
		return getTerms(ts, false);
	}

	@JsonIgnore
	public Term[] getTerms(TypeScoring ts, final boolean includeIdentity) {
		final List<Term> terms = new ArrayList<Term>();

		for (final String id : ratios.keySet()) {
			if (!pows.containsKey(id)) {
				continue;
			}
			if (!ts.isApplied(id)) {
				continue;
			}
			final Integer pow = pows.get(id);
			final Double ratio = ratios.get(id);
			final double termRatio = Math.pow(ratio, pow);

			final Term term = new Term(id, termRatio, pow);

			if (term.isIdentity() && !includeIdentity) {
				continue;
			}

			terms.add(term);
		}

		return terms.toArray(new Term[terms.size()]);
	}

	public static class Term {
		protected final String id;
		protected final double ratio;
		protected final int pow;

		public Term(String id, double ratio, int pow) {
			this.id = id;
			this.ratio = ratio;
			this.pow = pow;
		}

		public String getId() {
			return id;
		}

		public double getRatio() {
			return ratio;
		}

		public int getPow() {
			return pow;
		}

		public String getNiceRatio() {
			if (isIdentity()) {
				return "";
			}

			final double percentage = Math.round(ratio * 1000) / 10.0;

			if (percentage < 100) {
				return "-" + G4mat.format2(100 - percentage) + "%";
			}

			return "+" + G4mat.format2(percentage - 100) + "%";
		}

		public boolean isIdentity() {
			return Math.abs(ratio - 1) < 1e-2;
		}

		public boolean isPositive() {
			return ratio > 1;
		}
	}
}
