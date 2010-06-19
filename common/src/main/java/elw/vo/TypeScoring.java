package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.Arrays;

public class TypeScoring extends IdName {
	protected static final String[] NONE = new String[0];

	protected double weight = 1.0;
	protected String[] auto = NONE;
	protected String[] manual = NONE;
	protected String[] applied = NONE;

	//	is set up in runtime, not serialized
	protected BundleScoring bundleScoring;

	public String[] getApplied() {
		return applied;
	}

	public void setApplied(String[] applied) {
		this.applied = applied;
	}

	public String[] getAuto() {
		return auto;
	}

	public void setAuto(String[] auto) {
		this.auto = auto;
	}

	public String[] getManual() {
		return manual;
	}

	public void setManual(String[] manual) {
		this.manual = manual;
	}

	public double getWeight() {
		return weight;
	}

	public void setWeight(double weight) {
		this.weight = weight;
	}

	@JsonIgnore
	public void setBundleScoring(BundleScoring bundleScoring) {
		this.bundleScoring = bundleScoring;
	}

	public Criteria[] resolve(String[] ids) {
		if (bundleScoring == null) {
			throw new IllegalStateException("bundleScoring not set");
		}

		final Criteria[] allCriterias = bundleScoring.getCriterias();
		final Criteria[] resolved = new Criteria[ids.length];
		for (int i = 0; i < ids.length; i++) {
			final String id = ids[i];
			if (id == null || id.trim().length() == 0) {
				throw new IllegalStateException("ids contains empty element: " + Arrays.deepToString(ids));
			}

			Criteria found = null;
			for (Criteria c : allCriterias) {
				if (id.equals(c.getId())) {
					found = c;
					break;
				}
			}

			if (found == null) {
				throw new IllegalStateException("criteria not found for id: " + id);
			}

			resolved[i] = found;
		}

		return resolved;
	}

	public Criteria[] resolveManual() {
		return resolve(getManual());
	}

	public Criteria[] resolveAuto() {
		return resolve(getAuto());
	}

	public Criteria[] resolveApplied() {
		return resolve(getApplied());
	}

	public boolean isApplied(String id) {
		for (String s : applied) {
			if (s.equals(id)) {
				return true;
			}
		}

		return false;
	}
}
