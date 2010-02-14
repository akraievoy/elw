package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Assignment extends IdName {
	protected final List<Version> versions = new ArrayList<Version>();

	public Version[] getVersions() {
		return versions.toArray(new Version[versions.size()]);
	}

	public void setVersions(Version[] versions) {
		this.versions.clear();
		this.versions.addAll(Arrays.asList(versions));
	}
}
