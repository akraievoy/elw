package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.*;

public class Version extends IdName {
	protected Map<String, List<Entry<FileMeta>>> files = new TreeMap<String, List<Entry<FileMeta>>>();

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

	public int countFiles(
			final AssignmentType assType, final Assignment ass, final String slotId,
			final Map<String, List<Entry<FileMeta>>> filesStud
	) {
		return
				assType.getFiles(slotId).length +
				ass.getFiles(slotId).length +
				getFiles(slotId).length + 
				filesStud.get(slotId).size();
	}

	public boolean checkRead(
			final AssignmentType assType, final Assignment ass, final String slotId,
			final Map<String, List<Entry<FileMeta>>> filesStud
	) {
		final FileSlot fileSlot = assType.findSlotById(slotId);

		final String[] readApprovals = fileSlot.getReadApprovals();
		for (String slotIdRA : readApprovals) {
			if (!isApprovedAny(slotIdRA, filesStud)) {
				return false;
			}
		}

		return true;
	}

	public boolean isApprovedAny(
			final String slotId,
			final Map<String, List<Entry<FileMeta>>> filesStud
	) {
		final List<Entry<FileMeta>> files = filesStud.get(slotId);

		if (files == null || files.isEmpty()) {
			return false;
		}

		boolean approved = false;
		for (Entry<FileMeta> f : files) {
			if (f.getMeta().getScore() != null && f.getMeta().getScore().isApproved()) {
				approved = true;
			}
		}
		return approved;
	}

	public boolean isDeclinedLast(
			String slotId,
			final Map<String, List<Entry<FileMeta>>> filesStud
	) {
		final List<Entry<FileMeta>> files = filesStud.get(slotId);

		if (files == null || files.size() == 0) {
			return false;
		}

		final Entry<FileMeta> f = files.get(files.size() - 1);
		return f.getMeta().getScore() != null && !f.getMeta().getScore().isApproved();
	}

	public boolean isPendingLast(
			String slotId,
			final Map<String, List<Entry<FileMeta>>> filesStud
	) {
		final List<Entry<FileMeta>> files = filesStud.get(slotId);

		if (files == null || files.isEmpty()) {
			return false;
		}

		final Entry<FileMeta> f = files.get(files.size() - 1);
		return f.getMeta().getScore() == null;
	}

	public boolean checkWrite(
			final AssignmentType assType, final Assignment ass, final String slotId,
			final Map<String, List<Entry<FileMeta>>> filesStud
	) {
		final FileSlot fileSlot = assType.findSlotById(slotId);
		if (!fileSlot.isWritable()) {
			return false;
		}

		final String[] writeApprovals = fileSlot.getWriteApprovals();
		for (String slotIdWA : writeApprovals) {
			if (!isApprovedAny(slotIdWA, filesStud)) {
				return false;
			}
		}

		return true;
	}
}
