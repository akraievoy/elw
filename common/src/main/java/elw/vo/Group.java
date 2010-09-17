package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Group extends IdName implements Stamped {
	protected List<Student> students = new ArrayList<Student>();

	protected Stamp createStamp;
	protected Stamp updateStamp;

	public Student[] getStudents() {
		return students.toArray(new Student[students.size()]);
	}

	public void setStudents(Student[] students) {
		this.students.clear();
		this.students.addAll(Arrays.asList(students));
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