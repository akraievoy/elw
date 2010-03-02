package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Assignment extends IdName {
	protected final List<Version> versions = new ArrayList<Version>();
	protected TaskScoring scoring = null;
	protected boolean shared = false;

	public Version[] getVersions() {
		return versions.toArray(new Version[versions.size()]);
	}

	public void setVersions(Version[] versions) {
		this.versions.clear();
		this.versions.addAll(Arrays.asList(versions));
	}

	public TaskScoring getScoring() {
		return scoring;
	}

	public void setScoring(TaskScoring scoring) {
		this.scoring = scoring;
	}

	public boolean isShared() {
		return shared;
	}

	public void setShared(boolean shared) {
		this.shared = shared;
	}
}
