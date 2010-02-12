package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class QuizBundle {
	protected final List<Quiz> quizzes = new ArrayList<Quiz>();
	protected Scoring scoring;

	public Quiz[] getQuizzes() {
		return quizzes.toArray(new Quiz[quizzes.size()]);
	}

	public void setQuizzes(Quiz[] quizzes) {
		this.quizzes.clear();
		this.quizzes.addAll(Arrays.asList(quizzes));
	}

	public Scoring getScoringSetup() {
		return scoring;
	}

	public void setScoringSetup(Scoring scoring) {
		this.scoring = scoring;
	}
}
