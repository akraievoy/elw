package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Assignment {
	protected final List<Version> versions = new ArrayList<Version>();

	protected String id;
	protected String name;

	public Version[] getVersions() {
		return versions.toArray(new Version[versions.size()]);
	}

	public void setVersions(Version[] versions) {
		this.versions.clear();
		this.versions.addAll(Arrays.asList(versions));
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
