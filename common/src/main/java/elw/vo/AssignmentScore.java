package elw.vo;

public class AssignmentScore extends Score {
	protected String assignmentId;
	protected String assignmentVersionId;

	public String getAssignmentVersionId() {
		return assignmentVersionId;
	}

	public void setAssignmentVersionId(String assignmentVersionId) {
		this.assignmentVersionId = assignmentVersionId;
	}

	public String getAssignmentId() {
		return assignmentId;
	}

	public void setAssignmentId(String assignmentId) {
		this.assignmentId = assignmentId;
	}

}