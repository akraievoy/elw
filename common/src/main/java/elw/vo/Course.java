package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Course {
	protected final List<Assignment> assignments = new ArrayList<Assignment>();
	protected final List<Quiz> quizzes = new ArrayList<Quiz>();

	protected String name;
	protected String id;
	protected String resourcePath;

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getResourcePath() {
		return resourcePath;
	}

	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}

	public Assignment[] getAssignments() {
		return assignments.toArray(new Assignment[assignments.size()]);
	}

	public void setAssignments(Assignment[] assignments) {
		this.assignments.clear();
		this.assignments.addAll(Arrays.asList(assignments));
	}

	public Quiz[] getQuizzes() {
		return quizzes.toArray(new Quiz[quizzes.size()]);
	}

	public void setQuizzes(Quiz[] quizzes) {
		this.quizzes.clear();
		this.quizzes.addAll(Arrays.asList(quizzes));
	}
}