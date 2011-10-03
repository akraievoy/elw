package elw.vo;

import java.util.*;

public interface IdNamed {
    String getId();
    void setId(String id);

    String getName();
    void setName(String name);

    interface Resolver<Z extends IdNamed> {
        Z resolve(final String key);
    }

    public static class _ {
        public static <T extends IdNamed, M extends Map<String,T>> M mark(final M map) {
            for (Map.Entry<String, T> e : map.entrySet()) {
                e.getValue().setId(e.getKey());
            }
            return map;
        }

        public static <T extends IdNamed, M extends Map<String,T>> void filter(
                final M map, final Set<String> ids, M result
        ) {
            result.clear();
            result.putAll(map);
            result.keySet().retainAll(ids);
        }

        public static <T extends IdNamed, M extends Map<String,T>> M extend(
                final M base, final M over, final M res
        ) {
            res.clear();
            res.putAll(base);
            res.putAll(over);

            return res;
        }

        public static <T extends IdNamed, M extends Map<String, T>> M resolve(
                M current, M complete
        ) {
            for (Map.Entry<String, T> e : current.entrySet()) {
                if (e.getValue() == null) {
                    current.put(e.getKey(), complete.get(e.getKey()));
                }
            }
            return current;
        }

        public static <T extends IdNamed, M extends Map<String, T>> M resolve(
                M current, Resolver<T> resolver
        ) {
            for (Map.Entry<String, T> e : current.entrySet()) {
                if (e.getValue() == null) {
                    current.put(e.getKey(), resolver.resolve(e.getKey()));
                }
            }
            return current;
        }

        public static <T extends IdNamed> SortedMap<String, T> singleton(T t) {
            final SortedMap<String, T> singletonMap = new TreeMap<String, T>();
            singletonMap.put(t.getId(), t);
            return singletonMap;
        }

        public static <T extends IdNamed> T one(SortedMap<String, T> singletonMap) {
            return singletonMap.get(singletonMap.firstKey());
        }

        public static <E extends IdNamed> E findByName(final Collection<E> elems, String name, final boolean ignoreCase) {
            for (E e : elems) {
                final String nameWs = ws(name);
                final String eNameWs = ws(e.getName());
                if (ignoreCase && nameWs.equalsIgnoreCase(eNameWs) || nameWs.equals(eNameWs)) {
                    return e;
                }
            }

            return null;
        }

        private static String ws(final String text) {
            return text.replaceAll("\\s+", " ").trim();
        }
    }
}
