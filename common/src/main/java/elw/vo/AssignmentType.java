package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.*;

public class AssignmentType extends IdName {
	private final List<Assignment> assignments = new ArrayList<Assignment>();
	private final List<FileSlot> fileSlots = new ArrayList<FileSlot>();
	private Map<String, List<Entry<FileMeta>>> files = new TreeMap<String, List<Entry<FileMeta>>>();

	private AssignmentSetup setup = null;

	@JsonIgnore
	public Assignment[] getAssignments() {
		return assignments.toArray(new Assignment[assignments.size()]);
	}

	@JsonIgnore
	public void setAssignments(Assignment[] assignments) {
		this.assignments.clear();
		this.assignments.addAll(Arrays.asList(assignments));
	}

	public FileSlot[] getFileSlots() {
		return fileSlots.toArray(new FileSlot[fileSlots.size()]);
	}

	public void setFileSlots(FileSlot[] fileSlots) {
		this.fileSlots.clear();
		this.fileSlots.addAll(Arrays.asList(fileSlots));
	}

	public FileSlot findSlotById(final String id) {
		return IdName.findById(fileSlots, id);
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

	public AssignmentSetup getSetup() {
		return setup;
	}

	public void setSetup(AssignmentSetup setup) {
		this.setup = setup;
	}
}
