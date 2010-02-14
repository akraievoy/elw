package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;

public class Version extends IdName {
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
