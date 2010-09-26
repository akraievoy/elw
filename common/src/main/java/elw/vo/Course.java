package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Course extends IdName implements Stamped {
	protected final List<AssignmentType> assTypes = new ArrayList<AssignmentType>();
	protected final List<QuizType> quizTypes = new ArrayList<QuizType>();
	protected List<Entry<FileMeta>> files = new ArrayList<Entry<FileMeta>>();

	protected Stamp createStamp;

	protected String resourcePath;

	public String getResourcePath() {
		return resourcePath;
	}

	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}

	public AssignmentType[] getAssTypes() {
		return assTypes.toArray(new AssignmentType[assTypes.size()]);
	}

	public void setAssTypes(AssignmentType[] assTypes) {
		this.assTypes.clear();
		this.assTypes.addAll(Arrays.asList(assTypes));
	}

	@SuppressWarnings({"unchecked"})
	@JsonIgnore
	public Entry<FileMeta>[] getFiles() {
		return files.toArray(new Entry[files.size()]);
	}

	@JsonIgnore
	public void setFiles(Entry<FileMeta>[] files) {
		this.files.clear();
		this.files.addAll(Arrays.asList(files));
	}

	public QuizType[] getQuizTypes() {
		return quizTypes.toArray(new QuizType[quizTypes.size()]);
	}

	public void setQuizTypes(QuizType[] quizTypes) {
		this.quizTypes.clear();
		this.quizTypes.addAll(Arrays.asList(quizTypes));
	}

	public Stamp getCreateStamp() {
		return createStamp;
	}

	public void setCreateStamp(Stamp createStamp) {
		this.createStamp = createStamp;
	}
}