package elw.vo;

public class Question {
	protected String id;
	protected String[] statement;
	protected String[] answers;
	protected String[] options;
	protected int score;

	public String[] getAnswers() {
		return answers;
	}

	public void setAnswers(String[] answers) {
		this.answers = answers;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String[] getOptions() {
		return options;
	}

	public void setOptions(String[] options) {
		this.options = options;
	}

	public int getScore() {
		return score;
	}

	public void setScore(int score) {
		this.score = score;
	}

	public String[] getStatement() {
		return statement;
	}

	public void setStatement(String[] statement) {
		this.statement = statement;
	}
}
