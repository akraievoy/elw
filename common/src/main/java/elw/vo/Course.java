package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Course extends IdName {
	protected final List<AssignmentBundle> assBundles = new ArrayList<AssignmentBundle>();
	protected final List<QuizBundle> quizBundles = new ArrayList<QuizBundle>();

	protected String resourcePath;

	public String getResourcePath() {
		return resourcePath;
	}

	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}

	public AssignmentBundle[] getAssBundles() {
		return assBundles.toArray(new AssignmentBundle[assBundles.size()]);
	}

	public void setAssBundles(AssignmentBundle[] assBundles) {
		this.assBundles.clear();
		this.assBundles.addAll(Arrays.asList(assBundles));
	}

	public QuizBundle[] getQuizBundles() {
		return quizBundles.toArray(new QuizBundle[quizBundles.size()]);
	}

	public void setQuizBundles(QuizBundle[] quizBundles) {
		this.quizBundles.clear();
		this.quizBundles.addAll(Arrays.asList(quizBundles));
	}
}