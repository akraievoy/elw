package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Enrollment extends IdName implements Stamped{
	protected String groupId;
	protected String courseId;
	protected Stamp createStamp;
	protected Stamp updateStamp;

	protected List<Class> classes = new ArrayList<Class>();

	public String getCourseId() {
		return courseId;
	}

	public void setCourseId(String courseId) {
		this.courseId = courseId;
	}

	public String getGroupId() {
		return groupId;
	}

	public void setGroupId(String groupId) {
		this.groupId = groupId;
	}

	public Class[] getClasses() {
		return classes.toArray(new Class[classes.size()]);
	}

	public void setClasses(Class[] classes) {
		this.classes.clear();
		this.classes.addAll(Arrays.asList(classes));
	}

	public Stamp getCreateStamp() {
		return createStamp;
	}

	public void setCreateStamp(Stamp createStamp) {
		this.createStamp = createStamp;
	}

	public Stamp getUpdateStamp() {
		return updateStamp;
	}

	public void setUpdateStamp(Stamp updateStamp) {
		this.updateStamp = updateStamp;
	}
}
