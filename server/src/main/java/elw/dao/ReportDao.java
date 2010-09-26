package elw.dao;

import elw.vo.Entry;
import elw.vo.Path;
import elw.vo.ReportMeta;
import elw.vo.Stamp;

import java.io.*;
import java.util.*;

/**
 * Dealing with persisting, enumerating and retrieving report uploads.
 */
public class ReportDao extends Dao<ReportMeta> {
	public ReportDao() {
	}

	@Override
	public Path pathFromMeta(ReportMeta stamped) {
		return new Path(stamped.getPath());
	}

	public void createReport(Ctx ctx, BufferedInputStream in, String remoteAddr) throws IOException {
		final Path path = CodeDao.toPath(ctx);
		final ReportMeta reportMeta = new ReportMeta();
		reportMeta.setPath(path.getPath());
		reportMeta.setSourceAddress(remoteAddr);

		create(path, reportMeta, in, null);
	}

	public SortedMap<Stamp, Entry<ReportMeta>> findAll(Ctx ctx) {
		final SortedMap<Stamp, Entry<ReportMeta>> metas = findAll(CodeDao.toPath(ctx), null, true);

		resolveNames(ctx, metas.values());

		return metas;
	}

	public Entry<ReportMeta> findByStamp(Ctx ctx, Stamp stamp) {
		final Entry<ReportMeta> entry = findByStamp(CodeDao.toPath(ctx), stamp, null, true);

		if (entry != null) {
			resolveNames(ctx, Collections.singletonList(entry));
		}

		return entry;
	}

	//	TODO re-wire totalUploads method
	public int countTotalUploads(Ctx ctx) {
		return countVersions(CodeDao.toPath(ctx), null, null, true, null);
	}

	public void updateMeta(Ctx ctx, ReportMeta meta) throws IOException {
		update(CodeDao.toPath(ctx), meta, null, null);
	}

	public Entry<ReportMeta> findLast(Ctx ctx) {
		return findLast(CodeDao.toPath(ctx), null,true);
	}

	protected void resolveNames(Ctx path, final Collection<Entry<ReportMeta>> entries) {
		for (Entry<ReportMeta> entry : entries) {
			final ReportMeta reportMeta = entry.getMeta();
			final String fileName =
					path.getStudent().getName().replaceAll("\\s+", "_") + "--" +
					path.getAss().getId() + "_" + path.getVer().getId() + "--" +
					ReportMeta.getFileNameUploadStamp(reportMeta.getCreateStamp().getTime()) +
					".rtf";

			reportMeta.setFileName(fileName);
		}
	}
}