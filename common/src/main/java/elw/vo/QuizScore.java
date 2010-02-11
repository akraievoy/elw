package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;

public class QuizScore extends Score {
	protected String quizId;
	protected ArrayList<Answer> answers;

	public Answer[] getAnwsers() {
		return answers.toArray(new Answer[answers.size()]);
	}

	public void setAnswers(Answer[] answers) {
		this.answers.clear();
		this.answers.addAll(Arrays.asList(answers));
	}

	public String getQuizId() {
		return quizId;
	}

	public void setQuizId(String quizId) {
		this.quizId = quizId;
	}
}