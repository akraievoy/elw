package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AssignmentBundle {
	protected final List<Assignment> assignments = new ArrayList<Assignment>();
	protected Scoring scoring = null;
	protected AssignmentSetup setup = null;

	public Assignment[] getAssignments() {
		return assignments.toArray(new Assignment[assignments.size()]);
	}

	public void setAssignments(Assignment[] assignments) {
		this.assignments.clear();
		this.assignments.addAll(Arrays.asList(assignments));
	}

	public Scoring getScoring() {
		return scoring;
	}

	public void setScoring(Scoring scoring) {
		this.scoring = scoring;
	}

	public AssignmentSetup getSetup() {
		return setup;
	}

	public void setSetup(AssignmentSetup setup) {
		this.setup = setup;
	}
}
