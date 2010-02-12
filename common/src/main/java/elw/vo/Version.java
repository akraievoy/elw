package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;

public class Version {
	protected String id;
	protected String name;
	protected String[] statementHtml;
	protected String[] solution;

	protected final ArrayList<Test> tests = new ArrayList<Test>();

	public Test[] getTests() {
		return tests.toArray(new Test[tests.size()]);
	}

	public void setTests(Test[] tests) {
		this.tests.clear();
		this.tests.addAll(Arrays.asList(tests));
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

	public String[] getStatementHtml() {
		return statementHtml;
	}

	public void setStatementHtml(String[] statementHtml) {
		this.statementHtml = statementHtml;
	}

	public String[] getSolution() {
		return solution;
	}

	public void setSolution(String[] solution) {
		this.solution = solution;
	}
}
