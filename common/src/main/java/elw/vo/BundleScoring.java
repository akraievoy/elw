package elw.vo;

import java.util.*;

public class BundleScoring {
	protected final Map<String, TypeScoring> breakdown = new TreeMap<String, TypeScoring>();
	protected final List<Criteria> criterias = new ArrayList<Criteria>();

	public Map<String, TypeScoring> getBreakdown() {
		return breakdown;
	}

	public void setBreakdown(Map<String, TypeScoring> breakdown) {
		this.breakdown.clear();
		this.breakdown.putAll(breakdown);
	}

	public Criteria[] getCriterias() {
		return criterias.toArray(new Criteria[criterias.size()]);
	}

	public void setCriterias(Criteria[] criterias) {
		this.criterias.clear();
		this.criterias.addAll(Arrays.asList(criterias));
	}
}

