package elw.vo;

import org.akraievoy.gear.G4mat;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.util.*;

public class Score implements Stamped {
	private static final DateTimeFormatter FMT_DATETIME_NICE = DateTimeFormat.forPattern("EEE MMM dd HH:mm");

	protected final Map<String, Double> ratios = new TreeMap<String, Double>();
	protected final Map<String, Integer> pows = new TreeMap<String, Integer>();

	protected Stamp createStamp;
	protected String[] path;

	protected Stamp codeStamp;
	protected Stamp reportStamp;

	protected boolean approved;
	protected String comment;

	public boolean isApproved() {
		return approved;
	}

	public void setApproved(boolean approved) {
		this.approved = approved;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public Stamp getCreateStamp() {
		return createStamp;
	}

	public void setCreateStamp(Stamp createStamp) {
		this.createStamp = createStamp;
	}

	public String[] getPath() {
		return path;
	}

	public void setPath(String[] path) {
		this.path = path;
	}

	public Stamp getCodeStamp() {
		return codeStamp;
	}

	public void setCodeStamp(Stamp codeStamp) {
		this.codeStamp = codeStamp;
	}

	public Stamp getReportStamp() {
		return reportStamp;
	}

	public void setReportStamp(Stamp reportStamp) {
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

		copy.setCreateStamp(createStamp);
		//	LATER would we copy any other stamps here?
		copy.ratios.putAll(ratios);
		copy.pows.putAll(pows);

		return copy;
	}

	@JsonIgnore
	public String getNiceStamp() {
		return createStamp == null ? "Preliminary" : FMT_DATETIME_NICE.print(new DateTime(createStamp.getTime()));
	}

	@JsonIgnore
	public String getNicePoints(TypeScoring typeScoring, IndexEntry indexEntry) {
		return G4mat.format2(getPoints(typeScoring, indexEntry));
	}

	@JsonIgnore
	public String getNiceTotal(BundleScoring bundleScoring, IndexEntry indexEntry) {
		return G4mat.format2(getTotal(bundleScoring, indexEntry));
	}

	@JsonIgnore
	public double getPoints(TypeScoring typeScoring, IndexEntry indexEntry) {
		return indexEntry.getScoreBudget() * typeScoring.getWeight() * getRatio(typeScoring.getApplied());
	}

	@JsonIgnore
	public double getTotal(BundleScoring bundleScoring, IndexEntry indexEntry) {
		double result = 0.0;

		for (String type : bundleScoring.breakdown.keySet()) {
			final TypeScoring ts = bundleScoring.breakdown.get(type);
			if ("report".equals(type) && isPreliminary()) {
				continue;
			}

			result += getPoints(ts, indexEntry);
		}

		return result;
	}

	@JsonIgnore
	public boolean isPreliminary() {
		return createStamp == null || reportStamp == null;
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
