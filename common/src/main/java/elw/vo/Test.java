package elw.vo;

import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

public class Test {
	protected boolean shared;
	protected final SortedMap<String, String[]> args = new TreeMap<String, String[]>();

	public boolean isShared() {
		return shared;
	}

	public void setShared(boolean shared) {
		this.shared = shared;
	}

	public Map<String, String[]> getArgs() {
		return args;
	}

	public void setArgs(Map<String, String[]> args) {
		this.args.clear();
		if (args != null) {
			this.args.putAll(args);
		}
	}
}
