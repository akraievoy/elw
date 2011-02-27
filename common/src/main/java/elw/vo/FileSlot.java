package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class FileSlot extends IdNameStamped {
	private final List<String> readApprovals = new ArrayList<String>();
	private final List<String> writeApprovals = new ArrayList<String>();
	private final List<String> contentTypes = new ArrayList<String>();
	private Class validator = null;
	private String shortName;
	private long lengthLimit;
	private String nameRegex;
	private boolean escaped = true;

	private boolean binary = true;	//	TODO this should be better renamed to strict or something
	private boolean binaryAllowed = true;
	private boolean textAllowed = true;

	private boolean writable;
	private String editor;

	private double scoreWeight = 0.0;
	private final List<Criteria> criterias = new ArrayList<Criteria>();

	/**
	 * Forces treating all files of this slot as binary, with full byte-to-byte integrity.
	 * @return <code>binary</code> property
	 */
	public boolean isBinary() {
		return binary;
	}

	public void setBinary(boolean binary) {
		this.binary = binary;
	}

	/**
	 * Are any files looking as binary to be passed into the slot?
	 * @return <code>binary</code> property
	 */
	public boolean isBinaryAllowed() {
		return binaryAllowed;
	}

	public void setBinaryAllowed(boolean binaryAllowed) {
		this.binaryAllowed = binaryAllowed;
	}

	/**
	 * Are any files looking as text to be passed into this slot?
	 * @return <code>binary</code> property
	 */
	public boolean isTextAllowed() {
		return textAllowed;
	}

	public void setTextAllowed(boolean textAllowed) {
		this.textAllowed = textAllowed;
	}

	public List<String> getContentTypes() {
		return Collections.unmodifiableList(contentTypes);
	}

	public void setContentTypes(List<String> contentTypes) {
		this.contentTypes.clear();
		if (contentTypes != null) {
			this.contentTypes.addAll(contentTypes);
		}
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

	public List<String> getReadApprovals() {
		return Collections.unmodifiableList(readApprovals);
	}

	public void setReadApprovals(List<String> readApprovals) {
		this.readApprovals.clear();
		if (readApprovals != null) {
			this.readApprovals.addAll(readApprovals);
		}
	}

	public List<String> getWriteApprovals() {
		return Collections.unmodifiableList(writeApprovals);
	}

	public void setWriteApprovals(List<String> writeApprovals) {
		this.writeApprovals.clear();
		if (writeApprovals != null) {
			this.writeApprovals.addAll(writeApprovals);
		}
	}

	public String getShortName() {
		return shortName;
	}

	public void setShortName(String shortName) {
		this.shortName = shortName;
	}

	public boolean isWritable() {
		return writable;
	}

	public void setWritable(boolean writable) {
		this.writable = writable;
	}

	public String getEditor() {
		return editor;
	}

	public void setEditor(String editor) {
		this.editor = editor;
	}

	/**
	 * Usually text files are escaped on view, but sometimes you want them to be inlined in the
	 * page, as some links, for example.
	 *
	 * @return <code>escaped</code> property
	 */
	public boolean isEscaped() {
		return escaped;
	}

	public void setEscaped(boolean escaped) {
		this.escaped = escaped;
	}

	public double getScoreWeight() {
		return scoreWeight;
	}

	public void setScoreWeight(double scoreWeight) {
		this.scoreWeight = scoreWeight;
	}

	public Criteria[] getCriterias() {
		return criterias.toArray(new Criteria[criterias.size()]);
	}

	public void setCriterias(Criteria[] criterias) {
		this.criterias.clear();
		this.criterias.addAll(Arrays.asList(criterias));
	}

	public Class getValidator() {
		return validator;
	}

	public void setValidator(Class validator) {
		this.validator = validator;
	}
}
