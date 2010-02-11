package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Enrollment {
	protected int id;
	protected String name;

	protected int groupId;
	protected int courseId;

	protected List<AssignmentScore> assignmentScores = new ArrayList<AssignmentScore>();
	protected List<QuizScore> quizScores = new ArrayList<QuizScore>();
	protected List<Class> classes = new ArrayList<Class>();
	protected List<Shift> shifts = new ArrayList<Shift>();

	public int getCourseId() {
		return courseId;
	}

	public void setCourseId(int courseId) {
		this.courseId = courseId;
	}

	public int getGroupId() {
		return groupId;
	}

	public void setGroupId(int groupId) {
		this.groupId = groupId;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public AssignmentScore[] getAssignmentScores() {
		return assignmentScores.toArray(new AssignmentScore[assignmentScores.size()]);
	}

	public void setAssignmentScores(AssignmentScore[] assignmentScores) {
		this.assignmentScores.clear();
		this.assignmentScores.addAll(Arrays.asList(assignmentScores));
	}

	public QuizScore[] getQuizScores() {
		return quizScores.toArray(new QuizScore[quizScores.size()]);
	}

	public void setQuizScores(QuizScore[] quizScores) {
		this.quizScores.clear();
		this.quizScores.addAll(Arrays.asList(quizScores));
	}

	public Class[] getClasses() {
		return classes.toArray(new Class[classes.size()]);
	}

	public void setClasss(Class[] classes) {
		this.classes.clear();
		this.classes.addAll(Arrays.asList(classes));
	}

	public Shift[] getShifts() {
		return shifts.toArray(new Shift[shifts.size()]);
	}

	public void setShifts(Shift[] shifts) {
		this.shifts.clear();
		this.shifts.addAll(Arrays.asList(shifts));
	}
}
