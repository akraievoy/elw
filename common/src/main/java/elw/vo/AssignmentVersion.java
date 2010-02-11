package elw.vo;

import java.util.ArrayList;
import java.util.Arrays;

public class AssignmentVersion {
	protected String id;
	protected String shortName;
	protected String[] statementHtml;

	protected ArrayList<Test> tests;

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

	public String getShortName() {
		return shortName;
	}

	public void setShortName(String shortName) {
		this.shortName = shortName;
	}

	public String[] getStatementHtml() {
		return statementHtml;
	}

	public void setStatementHtml(String[] statementHtml) {
		this.statementHtml = statementHtml;
	}
}
