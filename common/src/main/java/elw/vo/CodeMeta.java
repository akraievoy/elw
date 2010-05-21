package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

public class CodeMeta {
	protected int testsPassed;
	protected int testsFailed;

	public int getTestsFailed() {
		return testsFailed;
	}

	public void setTestsFailed(int testsFailed) {
		this.testsFailed = testsFailed;
	}

	public int getTestsPassed() {
		return testsPassed;
	}

	public void setTestsPassed(int testsPassed) {
		this.testsPassed = testsPassed;
	}

	@JsonIgnore
	public double getPassRatio() {
		return testsPassed / (0.0 + testsPassed + testsFailed);
	}
}
