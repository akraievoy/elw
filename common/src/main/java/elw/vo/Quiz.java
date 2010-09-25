package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Quiz extends IdName {
	protected final List<Question> questions = new ArrayList<Question>();
	protected IndexEntry scoring = null;

	public Question[] getQuestions() {
		return questions.toArray(new Question[questions.size()]);
	}

	public void setQuestions(final Question[] questions) {
		this.questions.clear();
		this.questions.addAll(Arrays.asList(questions));
	}

	public IndexEntry getScoring() {
		return scoring;
	}

	public void setScoring(IndexEntry scoring) {
		this.scoring = scoring;
	}
}
