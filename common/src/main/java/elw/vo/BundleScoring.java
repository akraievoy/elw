package elw.vo;

import java.util.*;

public class BundleScoring {
	protected final Map<String, TypeScoring> breakdown = new TreeMap<String, TypeScoring>();
	protected final List<Criteria> criterias = new ArrayList<Criteria>();
	protected int version;

	public Map<String, TypeScoring> getBreakdown() {
		return breakdown;
	}

	public void setBreakdown(Map<String, TypeScoring> breakdown) {
		this.breakdown.clear();
		this.breakdown.putAll(breakdown);

		for (TypeScoring scoring : breakdown.values()) {
			scoring.setBundleScoring(this);
		}
	}

	public Criteria[] getCriterias() {
		return criterias.toArray(new Criteria[criterias.size()]);
	}

	public void setCriterias(Criteria[] criterias) {
		this.criterias.clear();
		this.criterias.addAll(Arrays.asList(criterias));
	}

	public int getVersion() {
		return version;
	}

	public void setVersion(int version) {
		this.version = version;
	}
}

