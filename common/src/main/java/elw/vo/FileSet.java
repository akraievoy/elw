package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FileSet extends IdNameStamped {
	protected List<String> requiresApprovalOf = new ArrayList<String>();
	protected List<String> contentTypes = new ArrayList<String>();
	protected long lengthLimit;
	protected String nameRegex;
	protected boolean binary;
	protected boolean writable;

	public boolean isBinary() {
		return binary;
	}

	public void setBinary(boolean binary) {
		this.binary = binary;
	}

	public String[] getContentTypes() {
		return contentTypes.toArray(new String[contentTypes.size()]);
	}

	public void setContentTypes(String[] contentTypes) {
		this.contentTypes.clear();
		this.contentTypes.addAll(Arrays.asList(contentTypes));
	}

	public long getLengthLimit() {
		return lengthLimit;
	}

	public void setLengthLimit(long lengthLimit) {
		this.lengthLimit = lengthLimit;
	}

	public String getNameRegex() {
		return nameRegex;
	}

	public void setNameRegex(String nameRegex) {
		this.nameRegex = nameRegex;
	}

	public String[] getRequiresApprovalOf() {
		return requiresApprovalOf.toArray(new String[requiresApprovalOf.size()]);
	}

	public void setRequiresApprovalOf(String[] requiresApprovalOf) {
		this.requiresApprovalOf.clear();
		this.requiresApprovalOf.addAll(Arrays.asList(requiresApprovalOf));
	}

	public boolean isWritable() {
		return writable;
	}

	public void setWritable(boolean writable) {
		this.writable = writable;
	}
}
