package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.*;

public class Version extends IdName {
	protected String[] statementHtml;
	protected String[] solution;
	protected final ArrayList<Test> tests = new ArrayList<Test>();

	protected Map<String, List<Entry<FileMeta>>> files = new TreeMap<String, List<Entry<FileMeta>>>();
	protected Map<String, List<Entry<FileMeta>>> studFiles = new TreeMap<String, List<Entry<FileMeta>>>();

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

	@SuppressWarnings({"unchecked"})
	@JsonIgnore
	public Entry<FileMeta>[] getFiles(final String slotId) {
		final List<Entry<FileMeta>> filesForSlot = files.get(slotId);
		if (filesForSlot == null) {
			return new Entry[0];
		}
		return filesForSlot.toArray(new Entry[filesForSlot.size()]);
	}

	@JsonIgnore
	public void setFiles(final String slotId, Entry<FileMeta>[] files) {
		final List<Entry<FileMeta>> filesForSlot = this.files.get(slotId);
		if (filesForSlot == null) {
			this.files.put(slotId, new ArrayList<Entry<FileMeta>>(Arrays.asList(files)));
			return;
		}

		filesForSlot.clear();
		filesForSlot.addAll(Arrays.asList(files));
	}

	@SuppressWarnings({"unchecked"})
	@JsonIgnore
	public Entry<FileMeta>[] getStudFiles(final String slotId) {
		final List<Entry<FileMeta>> filesForSlot = studFiles.get(slotId);
		if (filesForSlot == null) {
			return new Entry[0];
		}
		return filesForSlot.toArray(new Entry[filesForSlot.size()]);
	}

	@JsonIgnore
	public void setStudFiles(final String slotId, Entry<FileMeta>[] studFiles) {
		final List<Entry<FileMeta>> filesForSlot = this.files.get(slotId);
		if (filesForSlot == null) {
			this.studFiles.put(slotId, new ArrayList<Entry<FileMeta>>(Arrays.asList(studFiles)));
			return;
		}

		filesForSlot.clear();
		filesForSlot.addAll(Arrays.asList(studFiles));
	}
}
