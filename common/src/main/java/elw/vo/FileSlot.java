package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FileSlot extends IdNameStamped {
	private List<String> readApprovals = new ArrayList<String>();
	private List<String> writeApprovals = new ArrayList<String>();
	private List<String> contentTypes = new ArrayList<String>();
	private Class validator = null;
	private String shortName;
	private long lengthLimit;
	private String nameRegex;
	private boolean escaped = true;
	private boolean binary;
	private boolean writable;
	private String editor;

	private double scoreWeight = 0.0;
	private final List<Criteria> criterias = new ArrayList<Criteria>();

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

	public String[] getReadApprovals() {
		return readApprovals.toArray(new String[readApprovals.size()]);
	}

	public void setReadApprovals(String[] readApprovals) {
		this.readApprovals.clear();
		this.readApprovals.addAll(Arrays.asList(readApprovals));
	}

	public String[] getWriteApprovals() {
		return writeApprovals.toArray(new String[writeApprovals.size()]);
	}

	public void setWriteApprovals(String[] writeApprovals) {
		this.writeApprovals.clear();
		this.writeApprovals.addAll(Arrays.asList(writeApprovals));
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
