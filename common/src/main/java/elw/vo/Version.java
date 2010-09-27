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

	public int countFiles(final AssignmentType assType, final Assignment ass, final String slotId) {
		return
				assType.getFiles(slotId).length +
				ass.getFiles(slotId).length +
				getStudFiles(slotId).length +
				getFiles(slotId).length;
	}

	public boolean checkRead(final AssignmentType assType, final Assignment ass, final String slotId) {
		final FileSlot fileSlot = assType.findSlotById(slotId);

		final String[] readApprovals = fileSlot.getReadApprovals();
		for (String slotIdRA : readApprovals) {
			if (!isApprovedAny(slotIdRA)) {
				return false;
			}
		}

		return true;
	}

	public boolean isApprovedAny(String slotId) {
		final Entry<FileMeta>[] files = getStudFiles(slotId);
		boolean approved = false;
		for (Entry<FileMeta> f : files) {
			if (f.getMeta().getScore() != null && f.getMeta().getScore().isApproved()) {
				approved = true;
			}
		}
		return approved;
	}

	public boolean isDeclinedLast(String slotId) {
		final Entry<FileMeta>[] files = getStudFiles(slotId);

		if (files.length == 0) {
			return false;
		}

		final Entry<FileMeta> f = files[files.length - 1];
		return f.getMeta().getScore() != null && !f.getMeta().getScore().isApproved();
	}

	public boolean isPendingLast(String slotId) {
		final Entry<FileMeta>[] files = getStudFiles(slotId);

		if (files.length == 0) {
			return false;
		}

		final Entry<FileMeta> f = files[files.length - 1];
		return f.getMeta().getScore() == null;
	}

	public boolean checkWrite(final AssignmentType assType, final Assignment ass, final String slotId) {
		final FileSlot fileSlot = assType.findSlotById(slotId);
		if (!fileSlot.isWritable()) {
			return false;
		}

		final String[] writeApprovals = fileSlot.getWriteApprovals();
		for (String slotIdWA : writeApprovals) {
			if (!isApprovedAny(slotIdWA)) {
				return false;
			}
		}

		return true;
	}
}
