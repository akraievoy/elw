package elw.vo;

public class AssignmentScore extends Score {
	protected String assignmentId;
	protected String assignmentVersionId;
	protected long codeStamp;

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

	public long getCodeStamp() {
		return codeStamp;
	}

	public void setCodeStamp(long codeStamp) {
		this.codeStamp = codeStamp;
	}
}