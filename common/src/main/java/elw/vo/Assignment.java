package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.*;

public class Assignment extends IdNameStamped {
	protected final List<Version> versions = new ArrayList<Version>();
	protected Map<String, List<Entry<FileMeta>>> files = new TreeMap<String, List<Entry<FileMeta>>>();
	//	TODO remove this flag
	protected boolean shared = false;

	public Version[] getVersions() {
		return versions.toArray(new Version[versions.size()]);
	}

	public void setVersions(Version[] versions) {
		this.versions.clear();
		this.versions.addAll(Arrays.asList(versions));
	}

	@SuppressWarnings({"unchecked"})
	@JsonIgnore
	public Entry<FileMeta>[] getFiles(final String slotId) {
		final List<Entry<FileMeta>> filesForSlot = files.get(slotId);
		if (filesForSlot == null) {
			return new Entry[0];
		}
		return filesForSlot.toArray(new Entry[filesForSlot.size()]);
	}

	@JsonIgnore
	public void setFiles(final String slotId, Entry<FileMeta>[] files) {
		final List<Entry<FileMeta>> filesForSlot = this.files.get(slotId);
		if (filesForSlot == null) {
			this.files.put(slotId, new ArrayList<Entry<FileMeta>>(Arrays.asList(files)));
			return;
		}

		filesForSlot.clear();
		filesForSlot.addAll(Arrays.asList(files));
	}

	public boolean isShared() {
		return shared;
	}

	public void setShared(boolean shared) {
		this.shared = shared;
	}
}
