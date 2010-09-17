package elw.dao;

import elw.vo.Stamp;
import elw.vo.Stamped;
import org.codehaus.jackson.map.ObjectMapper;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

public abstract class Dao<Criteria extends Locator, Meta extends Stamped> {
	protected static final DateTimeFormatter FMT_DATE = DateTimeFormat.forPattern("yyyy-MM-dd");
	protected static final Pattern PATTERN_DATE = Pattern.compile("\\d\\d\\d\\d-\\d\\d-\\d\\d");

	protected static final String NAME_INBOX = "__inbox";
	protected static final String DEF_EXT_META = ".json";
	protected static final String DEF_EXT_BINARY = ".bin";
	protected static final String DEF_EXT_TEXT = ".txt";

	private static final Logger log = LoggerFactory.getLogger(Dao.class);

	protected ObjectMapper mapper;
	protected Class<Meta> metaClass;
	protected Criteria criteria;	//	default criteria for inbox poller
	protected String node;
	protected File base;
	protected int createGraceTime = 30000;
	protected int cacheTime = 300000;
	protected boolean groupByDate = true;

	protected String extMeta = DEF_EXT_META;
	protected String extBinary = DEF_EXT_BINARY;
	protected String extText = DEF_EXT_TEXT;

	protected final Object createMonitor = new Object();
	protected long createStamp = -1;

	protected final Object cacheMonitor = new Object();
	protected final Map<Criteria, Long> cacheStamps
			= new HashMap<Criteria, Long>();
	protected final Map<Criteria, SortedMap<Stamp, Entry<Meta>>> cache
			= new HashMap<Criteria, SortedMap<Stamp, Entry<Meta>>>();

	protected final Object fsMonitor = new Object();

	protected Dao() {
	}

	protected ObjectMapper getMapper() {
		return mapper;
	}

	public void setMapper(ObjectMapper mapper) {
		this.mapper = mapper;
	}

	public void setBase(File base) {
		this.base = base;
	}

	public void setExtBinary(String extBinary) {
		this.extBinary = extBinary;
	}

	public void setExtMeta(String extMeta) {
		this.extMeta = extMeta;
	}

	public void setExtText(String extText) {
		this.extText = extText;
	}

	public void setCacheTime(int cacheTime) {
		this.cacheTime = cacheTime;
	}

	public void setNode(String node) {
		this.node = node;
	}

	public Class<Meta> getMetaClass() {
		return metaClass;
	}

	public void setMetaClass(Class<Meta> metaClass) {
		this.metaClass = metaClass;
	}

	public void setGroupByDate(boolean groupByDate) {
		this.groupByDate = groupByDate;
	}

	public void setCriteria(Criteria criteria) {
		this.criteria = criteria;
	}

	public void start() {
		if (metaClass == null) {
			throw new IllegalStateException("metaClass not set");
		}
		if (criteria == null) {
			log.warn("criteria not set for metaClass: {}", metaClass.getSimpleName());
		}
		if (mapper == null) {
			mapper = new ObjectMapper();
		}
		if (node == null) {
			node = System.getProperty("user.name");
		}
		if (base == null) {
			base = new File(System.getProperty("user.home"), "elw-data/" + metaClass.getSimpleName());
		}

		log.warn("node: '{}'; base: {}", node, base.getAbsolutePath());
	}

	public void pollInbox() {
		if (criteria == null) {
			return;
		}
		final File inbox = new File(base, NAME_INBOX);
		if (!inbox.isDirectory()) {
			if (!inbox.mkdirs()) {
				log.warn("failed to create inbox: {}", inbox.getAbsolutePath());
				return;
			}
		}

		final File[] filesMeta = inbox.listFiles(new FileFilter() {
			public boolean accept(File pathname) {
				return pathname.isFile() && pathname.getName().endsWith(extMeta);
			}
		});

		if (filesMeta == null) {
			log.warn("failed to list: {}", inbox.getAbsolutePath());
			return;
		}

		for (File fileMeta : filesMeta) {
			final File parent = fileMeta.getParentFile();
			final String base = fileMeta.getName().substring(0, fileMeta.getName().length() - extMeta.length());
			final File fileText = new File(parent, base + extText);
			final File fileBinary = new File(parent, base + extBinary);

			final boolean text;
			final boolean binary;
			final long textStamp;
			final long binaryStamp;
			synchronized (fsMonitor) {
				text = fileText.isFile();
				binary = fileBinary.isFile();
				textStamp = text ? fileText.lastModified() : -1;
				binaryStamp = text ? fileBinary.lastModified() : -1;
			}
			final long grossStamp = Math.max(fileMeta.lastModified(), Math.max(textStamp, binaryStamp));

			if (System.currentTimeMillis() - grossStamp < createGraceTime) {
				continue;
			}

			final Meta meta;
			try {
				synchronized (fsMonitor) {
					meta = mapper.readValue(fileMeta, metaClass);
				}
			} catch (IOException e) {
				log.warn("failed to read meta: {}", fileMeta.getAbsolutePath());
				continue;
			}

			final Entry<Meta> value = new Entry<Meta>(
					fileMeta,
					text ? fileText : null,
					binary ? fileBinary : null,
					meta,
					grossStamp
			);

			try {
				create(criteria, meta, value.openBinaryStream(), value.openTextReader());
				log.info("created record of class {}", metaClass.getSimpleName());
			} catch (IOException e) {
				log.warn("rolling back/removing inputs: {}", metaClass.getSimpleName());
				log.warn("failed", e);
			} finally {
				value.closeStreams();
				deleteAll(fileMeta, fileBinary, fileText, "created");
			}
		}
	}

	public Stamp create(Criteria c, Meta meta, BufferedInputStream binary, BufferedReader text) throws IOException {
		final long createTime;
		synchronized (createMonitor) {
			createTime = createStamp = Math.max(System.currentTimeMillis(), createStamp + 1);
		}
		final Stamp stamp = new Stamp(node, createTime);

		final TreeSet<File> dirs = new TreeSet <File>();
		listCriteria(c, meta, true, false, false, dirs);
		if (dirs.size() != 1) {
			throw new IllegalArgumentException("criteria '" + c + "' does not yield one data dir");
		}
		final File dataDir;
		if (groupByDate) {
			dataDir = new File(dirs.iterator().next(), FMT_DATE.print(createTime));
			if (!dataDir.mkdirs()) {
				log.warn("failed to create data dir: {}", dataDir.getAbsolutePath());
			}
		} else {
			dataDir = dirs.iterator().next();
		}

		meta.setCreateStamp(stamp);
		writeAtomically(dataDir, stamp, false, meta, binary, text);

		invalidate(c, meta);

		return stamp;
	}

	public Stamp update(Criteria c, Meta meta, BufferedInputStream binary, BufferedReader text) throws IOException {
		final Stamp stamp = new Stamp(node, System.currentTimeMillis());

		final TreeSet<File> dirs = new TreeSet<File>();
		listCriteria(c, meta, true, false, false, dirs);
		if (dirs.size() != 1) {
			throw new IllegalArgumentException("criteria '" + c + "' does not yield one data dir");
		}
		final File dataDir;
		if (groupByDate) {
			dataDir = new File(dirs.iterator().next(), FMT_DATE.print(meta.getCreateStamp().getTime()));
		} else {
			dataDir = dirs.iterator().next();
		}

		writeAtomically(dataDir, stamp, true, meta, binary, text);

		invalidate(c, meta);

		return stamp;
	}

	public SortedSet<Stamp> findAllStamps(Criteria c, Boolean text, Boolean binary) throws IOException {
		final SortedMap<Stamp, Entry<Meta>> metas = load(c);

		filter(metas, null, null, text, binary);

		return new TreeSet<Stamp>(metas.keySet());
	}

	public SortedMap<Stamp, Entry<Meta>> findAllMetas(Criteria c, Boolean text, Boolean binary) throws IOException {
		final SortedMap<Stamp, Entry<Meta>> metas = load(c);

		filter(metas, null, null, text, binary);

		return metas;
	}

	public Entry<Meta> findLast(Criteria c, Boolean text, Boolean binary) {
		final SortedMap<Stamp, Entry<Meta>> metas = load(c);

		filter(metas, null, null, text, binary);

		return metas.get(metas.lastKey());
	}

	public Entry<Meta> findByTime(Criteria c, long time, Boolean text, Boolean binary) {
		final SortedMap<Stamp, Entry<Meta>> metas = load(c);

		final Stamp infStamp = new Stamp("", time);
		final Stamp supStamp = new Stamp("", time + 1);
		final SortedMap<Stamp, Entry<Meta>> narrowMap = metas.tailMap(infStamp).headMap(supStamp);

		filter(narrowMap, null, null, text, binary);

		return narrowMap.isEmpty() ? null : narrowMap.get(narrowMap.lastKey());
	}

	public int countVersions(
			Criteria c, Long sinceTime, Long untilTime,
			Boolean text, Boolean binary
	) {
		final SortedMap<Stamp, Entry<Meta>> metas = load(c);
		
		filter(metas, sinceTime, untilTime, text, binary);

		return metas.size();
	}

	public String[][] list(Criteria c) {
		return listCriteria(c, null, false, false, groupByDate, null);
	}

	protected SortedMap<Stamp, Entry<Meta>> load(Criteria c) {
		final long now = System.currentTimeMillis();
		final SortedMap<Stamp, Entry<Meta>> loaded;

		synchronized (cacheMonitor) {
			final Long cacheStamp = cacheStamps.get(c);
			final SortedMap<Stamp, Entry<Meta>> precached = cache.get(c);
			if (cacheStamp != null) {
				if (now - cacheStamp < cacheTime) {
					//	okay, the cache is valid
					return precached;
				}
			}

			final TreeSet<File> dirs = new TreeSet<File>();
			listCriteria(c, null, false, true, groupByDate, dirs);
			loaded = listRecords(dirs, precached);

			cacheStamps.put(c, now);
			cache.put(c, loaded);
		}

		return loaded;
	}

	protected void invalidate(Criteria c, Meta meta) {
		synchronized (cacheMonitor) {
			final Set<Criteria> cSet = cache.keySet();
			for (Iterator<Criteria> cIt = cSet.iterator(); cIt.hasNext();) {
				final Criteria cCached = cIt.next();
				if (intersects(cCached.getPath(metaClass, meta), c.getPath(metaClass, meta))) {
					cacheStamps.put(cCached, 0L);
				}
			}
		}
	}

	protected boolean intersects(String[] pathCache, String[] pathUpdate) {
		final int len = Math.min(pathCache.length, pathUpdate.length);

		for (int i = 0; i < len; i++) {
			final String elemCache = pathCache[i];
			final String elemUpdate = pathUpdate[i];
			if (elemCache != null && elemUpdate != null && !elemCache.equals(elemUpdate)) {
				return false;
			}
		}

		return true;
	}

	protected void writeAtomically(
			File dataDir, Stamp stamp, final boolean requireExistingMeta,
			Stamped meta, BufferedInputStream binary, BufferedReader text
	) throws IOException {
		final File fileMeta = new File(dataDir, stamp.toString() + extMeta);
		final File fileBinary = new File(dataDir, stamp.toString() + extBinary);
		final File fileText = new File(dataDir, stamp.toString() + extText);

		if (requireExistingMeta && !fileMeta.exists()) {
			throw new IllegalStateException("file must exist: " + fileMeta.getAbsolutePath());
		}

		boolean success = false;
		try {
			meta.setUpdateStamp(stamp);
			mapper.writeValue(fileMeta, meta);
			if (binary != null) {
				writeBinary(binary, fileBinary);
			}
			if (text != null) {
				writeText(text, fileText);
			}
			success = true;
		} finally {
			if (!success) {
				deleteAll(fileMeta, fileBinary, fileText, "broken");
			}
		}
	}

	protected void deleteAll(File fileMeta, File fileBinary, File fileText, final String which) {
		synchronized (fsMonitor) {
			deleteFile(fileMeta, which, "meta");
			deleteFile(fileBinary, which, "binary");
			deleteFile(fileText, which, "text");
		}
	}

	protected static void deleteFile(File file, final String which, final String what) {
		if (file.isFile() && !file.delete()) {
			log.warn("failed to delete " + which + " " + what + ": {}", file.getAbsolutePath());
		}
	}

	protected static void writeText(BufferedReader text, File fileText) throws IOException {
		BufferedReader in = null;
		BufferedWriter out = null;

		try {
			in = new BufferedReader(text);
			out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileText), "UTF-8"));

			final char[] buffer = new char[32768];
			int readCharsCount;
			while ((readCharsCount = in.read(buffer)) > 0) {
				out.write(buffer, 0, readCharsCount);
			}
			out.flush();
		} finally {
			close(in);
			close(out);
		}
	}

	protected static void writeBinary(BufferedInputStream binary, File fileBinary) throws IOException {
		BufferedInputStream in = null;
		BufferedOutputStream out = null;
		try {
			in = new BufferedInputStream(binary);
			out = new BufferedOutputStream(new FileOutputStream(fileBinary));

			final byte[] buffer = new byte[32768];

			int readBytesCount;
			while ((readBytesCount = in.read(buffer)) != -1) {
				out.write(buffer, 0, readBytesCount);
			}
			out.flush();
		} finally {
			close(in);
			close(out);
		}
	}

	protected void filter(SortedMap<Stamp, Entry<Meta>> metas, Long sinceTime, Long untilTime, Boolean text, Boolean binary) {
		final Set<Stamp> stamps = metas.keySet();
		for (Iterator<Stamp> stampIt = stamps.iterator(); stampIt.hasNext();) {
			final Stamp stamp = stampIt.next();

			if (
					(sinceTime != null && stamp.getTime() < sinceTime) ||
					(untilTime != null && stamp.getTime() > untilTime)
			) {
				stampIt.remove();
			}

			final Entry<Meta> entry = metas.get(stamp);
			if (text != null && text != (entry.getFileText() != null)) {
				stampIt.remove();
			}
			if (binary != null && binary != (entry.getFileBinary() != null)) {
				stampIt.remove();
			}
		}
	}

	/**
	 * Load all versions of all records for a given set of data dirs.
	 *
	 * @param dirs some set of data dirs, see {@link Dao#listCriteria(Locator, Stamped, boolean, boolean, boolean, java.util.SortedSet)}
	 * @param dest to dump entries into
	 * @return dest argument, <code>null</code> in case you don't want to bother with memory allocation yourself
	 */
	protected SortedMap<Stamp, Entry<Meta>> listRecords(
			final Set<File> dirs,
			final SortedMap<Stamp, Entry<Meta>> dest
	) {
		final SortedMap<Stamp, Entry<Meta>> result =
				dest == null ? new TreeMap<Stamp, Entry<Meta>>() : dest;
		final SortedSet<Stamp> alive = new TreeSet<Stamp>();

		for (File dir : dirs) {
			final File[] filesMeta = listMetas(dir);
			if (filesMeta == null) {
				log.warn("failed to list: {}", dir.getAbsolutePath());
				continue;
			}

			for (File fileMeta : filesMeta) {
				final File parent = fileMeta.getParentFile();
				final Stamp expectedStamp = parseStamp(fileMeta, extMeta);
				final File fileText = new File(parent, expectedStamp.toString() + extText);
				final File fileBinary = new File(parent, expectedStamp.toString() + extBinary);

				final boolean text;
				final boolean binary;
				final long textStamp;
				final long binaryStamp;
				synchronized (fsMonitor) {
					text = fileText.isFile();
					binary = fileBinary.isFile();
					textStamp = text ? fileText.lastModified() : -1;
					binaryStamp = text ? fileBinary.lastModified() : -1;
				}
				final long grossStamp = Math.max(fileMeta.lastModified(), Math.max(textStamp, binaryStamp));

				final Entry<Meta> precached = result.get(expectedStamp);
				if (precached != null && precached.getStamp() == grossStamp) {
					alive.add(expectedStamp);
					continue;	//	don't parse anything
				}

				final Meta meta;
				try {
					synchronized (fsMonitor) {
						meta = mapper.readValue(fileMeta, metaClass);
					}
				} catch (IOException e) {
					log.warn("failed to read meta: {}", fileMeta.getAbsolutePath());
					continue;
				}

				if (!expectedStamp.equals(meta.getCreateStamp())) {
					log.warn("create stamp mismatch for meta: {}", fileMeta.getAbsolutePath());
					continue;
				}

				final Entry<Meta> value = new Entry<Meta>(
						fileMeta,
						text ? fileText : null,
						binary ? fileBinary : null,
						meta,
						grossStamp
				);

				alive.add(expectedStamp);
				result.put(expectedStamp, value);
			}
		}

		result.keySet().retainAll(alive);

		return result;
	}

	protected File[] listMetas(File dir) {
		final File[] metas;

		synchronized (fsMonitor) {
			metas = dir.listFiles(new FileFilter() {
				public boolean accept(File f) {
					return parseStamp(f, Dao.this.extMeta) != null;
				}
			});
		}

		return metas;
	}

	protected static Stamp parseStamp(File f, final String ext) {
		final String n = f.getName();
		if (!f.isFile() || !n.endsWith(ext)) {
			return null;
		}
		final String nn = n.substring(0, n.length() - ext.length());
		if (Stamp.PATTERN_STRING.matcher(nn).matches()) {
			return Stamp.fromString(nn);
		} else {
			return null;
		}
	}

	/**
	 * Computes possible criteria positions, enumerating undefined ones via FS accesses.
	 *
	 * @param c				criteria to inspect
	 * @param meta			 to compensate for partially defined criteria
	 * @param create		   to create missing directories for any defined component
	 * @param ignoreEmpty	  to filter out directories which do not contain any data records
	 * @param localGroupByDate triggers grouping by date
	 * @param dirs			 pass-in arg to return directories which contain records for this criteria	 @return all possible values for each criteria path position
	 */
	protected String[][] listCriteria(
			final Criteria c,
			final Meta meta,
			final boolean create,
			final boolean ignoreEmpty,
			final boolean localGroupByDate,
			final SortedSet<File> dirs
	) {
		final String[] path = c.getPath(metaClass, meta);

		//	these grow exponentially
		final SortedSet<File> curDirs = new TreeSet<File>();
		final SortedSet<File> nextDirs = dirs == null ? new TreeSet<File>() : dirs;
		curDirs.add(base);
		nextDirs.clear();
		nextDirs.addAll(curDirs);	//	just in case criteria returns an empty path on us

		//	these grow linearly and are reported back to Dao user
		final TreeSet<String> nextVals = new TreeSet<String>();
		final String[][] pathElems = new String[path.length][];

		for (int peI = 0; peI < path.length; peI++) {
			nextDirs.clear();
			nextVals.clear();
			final String pe = path[peI];

			if (pe == null) {
				for (File curDir : curDirs) {
					final File[] childDirs;
					synchronized (fsMonitor) {
						childDirs = curDir.listFiles(new FileFilter() {
							public boolean accept(File pathname) {
								return pathname.isDirectory() && !pathname.getName().equals(NAME_INBOX);
							}
						});
					}
					if (childDirs == null) {
						log.warn("failed to list: {}", curDir.getAbsolutePath());
						continue;
					}
					for (final File childDir : childDirs) {
						nextVals.add(childDir.getName());
					}
					nextDirs.addAll(Arrays.asList(childDirs));
				}
			} else {
				for (File curDir : curDirs) {
					final File nextDir = new File(curDir, pe);
					synchronized (fsMonitor) {
						if (!nextDir.exists() && create) {
							if (!nextDir.mkdirs()) {
								log.warn("failed to create dir: {}", nextDir.getAbsolutePath());
							}
						}
						if (nextDir.isDirectory()) {
							nextDirs.add(nextDir);
						}
					}
				}
				nextVals.add(pe);
			}
			curDirs.clear();
			curDirs.addAll(nextDirs);
			pathElems[peI] = nextVals.toArray(new String[nextVals.size()]);
		}

		if (localGroupByDate) {
			curDirs.clear();
			curDirs.addAll(nextDirs);
			nextDirs.clear();

			for (File curDir : curDirs) {
				final File[] childDirs;
				synchronized (fsMonitor) {
					childDirs = curDir.listFiles(new FileFilter() {
						public boolean accept(File pathname) {
							return pathname.isDirectory() && PATTERN_DATE.matcher(pathname.getName()).matches();
						}
					});
				}
				nextDirs.addAll(Arrays.asList(childDirs));
			}
		}

		if (ignoreEmpty) {
			curDirs.clear();	//	temporarily store all no-data dirs here
			for (File dir : nextDirs) {
				final File[] metas = listMetas(dir);
				if (metas == null) {
					log.warn("failed to list: {}", dir.getAbsolutePath());
					continue;
				}
				if (metas.length == 0) {
					curDirs.add(dir);
				}
			}
			nextDirs.removeAll(curDirs);
		}

		return pathElems;
	}

	protected static void close(final Closeable closeable) {
		if (closeable != null) {
			try {
				closeable.close();
			} catch (IOException e) {
				log.info("failed on close", e);
			}
		}
	}

	public static class Entry<Meta> {
		protected final Meta meta;
		protected final File fileMeta;
		protected final File fileBinary;
		protected final File fileText;
		protected final long stamp;
		protected BufferedInputStream binary;
		protected BufferedReader text;

		public Entry(File fileMeta, File fileText, File fileBinary, Meta meta, long stamp) {
			this.meta = meta;
			this.fileMeta = fileMeta;
			this.fileText = fileText;
			this.fileBinary = fileBinary;
			this.stamp = stamp;
		}

		public File getFileMeta() {
			return fileMeta;
		}

		public File getFileBinary() {
			return fileBinary;
		}

		public File getFileText() {
			return fileText;
		}

		public Meta getMeta() {
			return meta;
		}

		public long getStamp() {
			return stamp;
		}

		public BufferedInputStream openBinaryStream() throws IOException {
			if (fileBinary == null) {
				return null;
			}
			return binary = new BufferedInputStream(new FileInputStream(fileBinary));
		}

		public BufferedReader openTextReader() throws IOException {
			if (fileText == null) {
				return null;
			}

			return text = new BufferedReader(new InputStreamReader(new FileInputStream(fileText), "UTF-8"));
		}

		public void closeStreams() {
			close(binary);
			close(text);
		}
	}
}
