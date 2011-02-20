package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;

public class Answer {
	private String questionId;
	private ArrayList<String> answers;

	public String[] getAnswers() {
		return answers.toArray(new String[answers.size()]);
	}

	public void setAnswers(String[] answers) {
		this.answers.clear();
		this.answers.addAll(Arrays.asList(answers));
	}

	public String getQuestionId() {
		return questionId;
	}

	public void setQuestionId(String questionId) {
		this.questionId = questionId;
	}
}
