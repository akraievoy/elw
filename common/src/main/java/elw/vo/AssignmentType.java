package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AssignmentType extends IdNameStamped {
	protected final List<Assignment> assignments = new ArrayList<Assignment>();
	protected final List<FileSet> fileSets = new ArrayList<FileSet>();

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

	public FileSet[] getFileSets() {
		return fileSets.toArray(new FileSet[fileSets.size()]);
	}

	public void setFileSets(FileSet[] fileSets) {
		this.fileSets.clear();
		this.fileSets.addAll(Arrays.asList(fileSets));
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
