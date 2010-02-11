package elw.vo;

public class Test {
	protected String[] memoryInput;
	protected String[] memoryOutput;
	protected String[] regsInput;
	protected String[] regsOutput;
	protected boolean shared;

	public String[] getMemoryInput() {
		return memoryInput;
	}

	public void setMemoryInput(String[] memoryInput) {
		this.memoryInput = memoryInput;
	}

	public String[] getMemoryOutput() {
		return memoryOutput;
	}

	public void setMemoryOutput(String[] memoryOutput) {
		this.memoryOutput = memoryOutput;
	}

	public String[] getRegsInput() {
		return regsInput;
	}

	public void setRegsInput(String[] regsInput) {
		this.regsInput = regsInput;
	}

	public String[] getRegsOutput() {
		return regsOutput;
	}

	public void setRegsOutput(String[] regsOutput) {
		this.regsOutput = regsOutput;
	}

	public boolean isShared() {
		return shared;
	}

	public void setShared(boolean shared) {
		this.shared = shared;
	}
}
