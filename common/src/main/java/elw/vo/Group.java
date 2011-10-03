package elw.vo;

import java.util.*;

public class Group extends Squab implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

	private SortedMap<String, Student> students = new TreeMap<String, Student>();
	public Map<String, Student> getStudents() {
		return Collections.unmodifiableSortedMap(students);
	}
	public void setStudents(SortedMap<String, Student> students) {
		this.students.clear();
        if (students != null) {
            this.students.putAll(students);
        }
    }

    @Override
    protected String[] pathElems() {
        return new String[] { id };
    }
}