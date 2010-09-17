package elw.dao;

import elw.vo.Stamped;

public interface Locator {
	<T extends Stamped> String[] getPath(final Class<T> metaClass, T meta);
}
