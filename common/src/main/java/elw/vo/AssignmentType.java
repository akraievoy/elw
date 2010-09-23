package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AssignmentType extends IdName {
	protected final List<Assignment> assignments = new ArrayList<Assignment>();
	protected final List<FileSlot> fileSlots = new ArrayList<FileSlot>();

	protected BundleScoring scoring = null;
	protected AssignmentSetup setup = null;

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

	public BundleScoring getScoring() {
		return scoring;
	}

	public void setScoring(BundleScoring scoring) {
		this.scoring = scoring;
	}

	public AssignmentSetup getSetup() {
		return setup;
	}

	public void setSetup(AssignmentSetup setup) {
		this.setup = setup;
	}
}
