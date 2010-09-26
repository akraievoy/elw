package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.List;

public class IdName {
	protected String name;
	protected String id;

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	@JsonIgnore
	public int getIdAsInt() {
		return Integer.parseInt(id);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String toString() {
		return name == null || name.trim().length() == 0 ? id : name;
	}

	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof IdName)) return false;

		IdName idName = (IdName) o;

		if (id != null ? !id.equals(idName.id) : idName.id != null) return false;
		if (name != null ? !name.equals(idName.name) : idName.name != null) return false;

		return true;
	}

	@Override
	public int hashCode() {
		int result = name != null ? name.hashCode() : 0;
		result = 31 * result + (id != null ? id.hashCode() : 0);
		return result;
	}

	public static <E extends IdName> E findById(final E[] elems, String id) {
		E found = null;
		for (E e : elems) {
			if (id.equals(e.getId())) {
				found = e;
				break;
			}
		}

		return found;
	}

	public static <E extends IdName> E findById(final List<E> elems, String id) {
		E found = null;
		for (E e : elems) {
			if (id.equals(e.getId())) {
				found = e;
				break;
			}
		}

		return found;
	}

	public static <E extends IdName> int indexOfId(final E[] elems, String id) {
		int found = -1;
		for (int i = 0, elemsLength = elems.length; i < elemsLength; i++) {
			final E e = elems[i];
			if (id.equals(e.getId())) {
				found = i;
				break;
			}
		}

		return found;
	}

	public static <E extends IdName> E findByName(final E[] elems, String name, final boolean ignoreCase) {
		E found = null;
		for (E e : elems) {
			if ((ignoreCase && name.equalsIgnoreCase(e.getName())) || name.equals(e.getName())) {
				found = e;
				break;
			}
		}

		return found;
	}
}
