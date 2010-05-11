package elw.dao;

import elw.vo.Student;

import java.io.File;

public class AssignmentPath {
	protected final String courseId;
	protected final String groupId;
	protected final Student student;
	protected final int assBundleIdx;
	protected final String assId;
	protected final String verId;

	public AssignmentPath(String courseId, String groupId, Student student, int assBundleIdx, String assId, String verId) {
		this.courseId = courseId;
		this.groupId = groupId;
		this.student = student;
		this.assBundleIdx = assBundleIdx;
		this.assId = assId;
		this.verId = verId;
	}

	public String getCourseId() {
		return courseId;
	}

	public String getGroupId() {
		return groupId;
	}

	public Student getStudent() {
		return student;
	}

	public int getAssBundleIdx() {
		return assBundleIdx;
	}

	public String getAssId() {
		return assId;
	}

	public String getVerId() {
		return verId;
	}

	protected File getRoot(File uploadsDir) {
		final File studDir = new File(uploadsDir, "" + getCourseId() + "." + getGroupId() + "/" + getStudent().getId() + "." + getStudent().getName() + "/");
		final File assDir = new File(studDir, getAssBundleIdx() + "." + getAssId() + "." + getVerId());
		return assDir;
	}
}
