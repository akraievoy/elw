package elw.vo;

import java.util.*;

public class Enrollment extends IdNameStamped {
	protected String groupId;
	protected String courseId;

	protected List<Class> classes = new ArrayList<Class>();
	protected List<IndexEntry> index = new ArrayList<IndexEntry>();

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

	public List<Class> getClasses() {
		return classes;
	}

	public void setClasses(List<Class> classes) {
		this.classes = classes;
	}

	public List<IndexEntry> getIndex() {
		return index;
	}

	public void setIndex(List<IndexEntry> index) {
		this.index = index;
	}

	public boolean checkOnTime(Stamp createStamp) {
		for (Class aClass : classes) {
			if (aClass.checkOnTime(createStamp)) {
				return true;
			}
		}

		return false;
	}

	public int cmpTotalBudget() {
		int totalBudget = 0;
		for (IndexEntry ie : getIndex()) {
			totalBudget += ie.getScoreBudget();
		}
		return totalBudget;
	}
}
