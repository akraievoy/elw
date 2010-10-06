package elw.vo;

import org.akraievoy.gear.G4Str;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class Entry<Meta> {
	private static final Logger log = LoggerFactory.getLogger(Entry.class);

	protected final Meta meta;
	protected final File fileMeta;
	protected final File fileBinary;
	protected final File fileText;
	protected final long stamp;
	protected ThreadLocal<BufferedInputStream> binary = new ThreadLocal<BufferedInputStream>();
	protected ThreadLocal<BufferedReader> text = new ThreadLocal<BufferedReader>();

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
		final BufferedInputStream newBinary = new BufferedInputStream(new FileInputStream(fileBinary));
		binary.set(newBinary);
		return newBinary;
	}

	public BufferedReader openTextReader() throws IOException {
		if (fileText == null) {
			return null;
		}

		final BufferedReader newText = new BufferedReader(new InputStreamReader(new FileInputStream(fileText), "UTF-8"));
		text.set(newText);
		return newText;
	}

	public String[] dumpText() throws IOException {
		try {
			final BufferedReader reader = openTextReader();

			if (reader == null) {
				return new String[0];
			}

			final List<String> lines = new ArrayList<String>();
			String curLine;

			while ((curLine = reader.readLine()) != null) {
				lines.add(curLine);
			}

			return lines.toArray(new String[lines.size()]);
		} finally {
			closeStreams();
		}
	}

	public String getText() throws IOException {
		return G4Str.join(dumpText(), "<br/>");
	}

	public void closeStreams() {
		close(binary.get());
		close(text.get());
		binary.set(null);
		text.set(null);
	}

	public static void close(final Closeable closeable) {
		if (closeable != null) {
			try {
				closeable.close();
			} catch (IOException e) {
				log.info("failed on close", e);
			}
		}
	}
}
