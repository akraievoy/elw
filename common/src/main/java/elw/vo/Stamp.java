package elw.vo;

import java.util.regex.Pattern;

public class Stamp implements Comparable<Stamp> {
	public static final Pattern PATTERN_STRING = Pattern.compile("\\w+-\\w+");

	protected String node;
	protected long time;

	public Stamp() {
	}

	public Stamp(String node, long time) {
		this.node = node;
		this.time = time;
	}

	public String getNode() {
		return node;
	}

	public void setNode(String node) {
		this.node = node;
	}

	public long getTime() {
		return time;
	}

	public void setTime(long time) {
		this.time = time;
	}

	public int compareTo(Stamp o) {
		return time < o.time ? -1 : (time == o.time ? node.compareTo(o.node) : 1);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		Stamp stamp = (Stamp) o;

		return time == stamp.time && node.equals(stamp.node);
	}

	@Override
	public int hashCode() {
		int result = node.hashCode();
		result = 31 * result + (int) (time ^ (time >>> 32));
		return result;
	}

	@Override
	public String toString() {
		return Long.toString(time, 36) + "-" + node;
	}

	public static Stamp fromString(final String value) {
		if (value == null) {
			throw new IllegalStateException("illegal value: " + String.valueOf(value));
		}

		final int dashIdx = value.indexOf("-");
		if (dashIdx < 0) {
			throw new IllegalStateException("illegal value: " + String.valueOf(value));
		}

		final String timeBase36 = value.substring(0, dashIdx);
		final String node = value.substring(dashIdx + 1);

		return new Stamp(node, Long.parseLong(timeBase36, 36));
	}

	public static Stamp parse(String parameter, Stamp def) {
		if (parameter == null || parameter.length() == 0 || parameter.indexOf("-") < 0) {
			return def;
		}

		return fromString(parameter);
	}
}
