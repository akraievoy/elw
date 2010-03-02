package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class QuizBundle {
	protected final List<Quiz> quizzes = new ArrayList<Quiz>();
	protected BundleScoring scoring;

	public Quiz[] getQuizzes() {
		return quizzes.toArray(new Quiz[quizzes.size()]);
	}

	public void setQuizzes(Quiz[] quizzes) {
		this.quizzes.clear();
		this.quizzes.addAll(Arrays.asList(quizzes));
	}

	public BundleScoring getScoring() {
		return scoring;
	}

	public void setScoring(BundleScoring scoring) {
		this.scoring = scoring;
	}
}
