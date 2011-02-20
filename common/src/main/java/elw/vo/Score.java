package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class Score implements Stamped {
	private final Map<String, Double> ratios = new TreeMap<String, Double>();
	private final Map<String, Integer> pows = new TreeMap<String, Integer>();

	private Stamp createStamp;
	private String[] path;

	private Boolean approved;
	private String comment;

	private boolean best;	//	transient

	public Boolean getApproved() {
		return approved;
	}

	public void setApproved(Boolean approved) {
		this.approved = approved;
	}

	@JsonIgnore
	public boolean isBest() {
		return best;
	}

	@JsonIgnore
	public void setBest(boolean best) {
		this.best = best;
	}

	public String getComment() {
		return comment;
	}

	@SuppressWarnings({"UnusedDeclaration"})
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

	public Map<String, Integer> getPows() {
		return pows;
	}

	@SuppressWarnings({"UnusedDeclaration"})
	public void setPows(Map<String, Integer> pows) {
		this.pows.clear();
		this.pows.putAll(pows);
	}

	public Map<String, Double> getRatios() {
		return ratios;
	}

	@SuppressWarnings({"UnusedDeclaration"})
	public void setRatios(Map<String, Double> ratios) {
		this.ratios.clear();
		this.ratios.putAll(ratios);
	}

	public Score copy() {
		final Score copy = new Score();

		copy.setCreateStamp(createStamp);
		copy.setPath(null != path ? path.clone() : null);
		copy.ratios.putAll(ratios);
		copy.pows.putAll(pows);

		return copy;
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

	public double computeRatio(FileSlot slot) {
		double res = 1.0;

		for (final Criteria c : slot.getCriterias()) {
			final String id = idFor(slot, c);
			if (!contains(id)) {
				continue;
			}
			res *= Math.pow(ratios.get(id), pows.get(id));
		}

		return res;
	}

	public String idFor(FileSlot slot, Criteria c) {
		return slot.getId() + "--" + c.getId();
	}

	private boolean contains(String id) {
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

	public boolean contains(FileSlot slot, Criteria c) {
		return contains(idFor(slot, c));
	}

	public int getPow(FileSlot slot, Criteria c) {
		return contains(slot, c) ? getPows().get(idFor(slot, c)) : 0;
	}

	public double getRatio(FileSlot slot, Criteria c) {
		return contains(slot, c) ? getRatios().get(idFor(slot, c)) : 1;
	}

	@JsonIgnore
	public ScoreTerm[] getTerms(AssignmentType aType, final boolean includeIdentity) {
		final List<ScoreTerm> scoreTerms = new ArrayList<ScoreTerm>();

		for (FileSlot slot : aType.getFileSlots()) {
			for (final Criteria c : slot.getCriterias()) {
				final String id = idFor(slot, c);
				if (contains(slot, c)) {
					final Integer pow = pows.get(id);
					final Double ratio = ratios.get(id);
					final double termRatio = Math.pow(ratio, pow);

					final ScoreTerm scoreTerm = new ScoreTerm(id, termRatio, pow, slot, c);

					if (scoreTerm.isIdentity() && !includeIdentity) {
						continue;
					}

					scoreTerms.add(scoreTerm);
				}
			}
		}

		return scoreTerms.toArray(new ScoreTerm[scoreTerms.size()]);
	}
}
