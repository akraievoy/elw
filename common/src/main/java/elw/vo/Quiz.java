package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Quiz {
	protected final List<Question> questions = new ArrayList<Question>();
	protected String id;
	protected String name;

	public Question[] getQuestions() {
		return questions.toArray(new Question[questions.size()]);
	}

	public void setQuestions(final Question[] questions) {
		this.questions.clear();
		this.questions.addAll(Arrays.asList(questions));
	}

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
}
