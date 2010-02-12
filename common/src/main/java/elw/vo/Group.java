package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Group {
	protected String name;
	protected int id;

	protected List<Student> students = new ArrayList<Student>();

	public Student[] getStudents() {
		return students.toArray(new Student[students.size()]);
	}

	public void setStudents(Student[] students) {
		this.students.clear();
		this.students.addAll(Arrays.asList(students));
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}