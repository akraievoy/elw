package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Course extends IdName implements Stamped {
	private final List<AssignmentType> assTypes = new ArrayList<AssignmentType>();

	private Stamp createStamp;

	private String resourcePath;

	public String getResourcePath() {
		return resourcePath;
	}

	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}

	public AssignmentType[] getAssTypes() {
		return assTypes.toArray(new AssignmentType[assTypes.size()]);
	}

	public void setAssTypes(AssignmentType[] assTypes) {
		this.assTypes.clear();
		this.assTypes.addAll(Arrays.asList(assTypes));
	}

	public Stamp getCreateStamp() {
		return createStamp;
	}

	public void setCreateStamp(Stamp createStamp) {
		this.createStamp = createStamp;
	}
}