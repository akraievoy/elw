package elw.dao;

import com.google.common.base.Charsets;
import com.google.common.collect.MapMaker;
import com.google.common.io.ByteStreams;
import com.google.common.io.CharStreams;
import com.google.common.io.InputSupplier;
import elw.vo.Squab;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;

import java.io.BufferedOutputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.*;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

/**
 * CouchDB entity persistence.
 */
public class CouchDao {
    private final ObjectMapper mapper = new ObjectMapper();

    {
        mapper.getSerializationConfig().enable(SerializationConfig.Feature.INDENT_OUTPUT);
    }

    protected String couchUrl = "http://localhost:5984/";

    public void setCouchUrl(String couchUrl) {
        if (!couchUrl.endsWith("/")) {
            throw new IllegalArgumentException("couchUrl '" + couchUrl + "' must end with '/'");
        }
        this.couchUrl = couchUrl;
    }

    protected String dbName = "elw-data";

    public void setDbName(String dbName) {
        this.dbName = dbName;
    }

    protected String username = "supercow";

    public void setPassword(String password) {
        this.password = password;
    }

    protected String password = "typical";

    public void setUsername(String username) {
        this.username = username;
    }

    private final MapMaker caches = new MapMaker().concurrencyLevel(3).expireAfterWrite(1L, TimeUnit.MINUTES);
    private final ConcurrentMap<Squab.Path, List<Squab.Path>> cachePaths = caches.makeMap();
    private final ConcurrentMap<Squab.Path, List<List<String>>> cacheAxes = caches.makeMap();
    private final ConcurrentMap<Squab.Path, List<? extends Squab>> cacheSquabs = caches.makeMap();
    private final ConcurrentMap<Squab.Path, SortedMap<Long, ? extends Squab.Stamped>> cacheStamped = caches.makeMap();

    private static final SortedMap<Long, ? extends Squab.Stamped> EMPTY_MAP =
            Collections.unmodifiableSortedMap(new TreeMap<Long, Squab.Stamped>());

    public CouchDao() { /* nothing to do here */ }

    protected void invalidate(Squab.Path path) {
        invalidate_(path, cacheAxes.keySet());
        invalidate_(path, cachePaths.keySet());
        invalidate_(path, cacheSquabs.keySet());
        invalidate_(path, cacheStamped.keySet());
    }

    protected void invalidate_(Squab.Path path, final Set<Squab.Path> paths) {
        for (Iterator<Squab.Path> iterator = paths.iterator(); iterator.hasNext(); ) {
            if (iterator.next().intersects(path)) {
                iterator.remove();
            }
        }
    }

    //  Squab
    public Long update(Squab squab) {
        Long stamp = null;
        if (squab instanceof Squab.Stamped) {
            stamp = ((Squab.Stamped) squab).updateStamp();
        }
        couchPut(
                squab.getCouchPath().id(),
                squab
        );
        invalidate(squab.getCouchPath());
        return stamp;
    }

    public <S extends Squab> S findOne(final Class<S> squabClass, String... path) {
        final List<S> all = findAll(squabClass, path);
        if (all.isEmpty()) {
            throw new IllegalStateException(
                    "no records: " + squabClass.getSimpleName() + " '" + Arrays.toString(path) + "'"
            );
        }
        return all.get(0);
    }

    public InputSupplier<InputStream> file(final Squab squab, String fileName) {
        return couchFileGet(squab.getCouchPath(), fileName);
    }

    public byte[] fileBytes(final Squab squab, String fileName) throws IOException {
        return ByteStreams.toByteArray(couchFileGet(squab.getCouchPath(), fileName));
    }

    public List<String> fileLines(final Squab squab, String fileName) throws IOException {
        return CharStreams.readLines(CharStreams.newReaderSupplier(
                couchFileGet(squab.getCouchPath(), fileName),
                Charsets.UTF_8
        ));
    }

    public String fileText(final Squab squab, String fileName) throws IOException {
        return CharStreams.toString(CharStreams.newReaderSupplier(
                couchFileGet(squab.getCouchPath(), fileName),
                Charsets.UTF_8
        ));
    }

    public <S extends Squab> S findSome(final Class<S> squabClass, String... path) {
        final List<S> all = findAll(squabClass, path);
        return all.isEmpty() ? null : all.get(0);
    }

    public <S extends Squab> List<S> findAll(final Class<S> squabClass, String... path) {
        final Squab.Path fullPath = new Squab.Path(squabClass, path);
        final List<? extends Squab> allCached = cacheSquabs.get(fullPath);
        if (allCached != null) {
            //noinspection unchecked
            return (List<S>) allCached;
        }

        final List<Squab.Path> allPaths = findAllPaths(fullPath);
        final ArrayList<S> all = new ArrayList<S>();
        for (Squab.Path aPath : allPaths) {
            all.add(couchGet(aPath, squabClass));
        }

        final List<S> allRO = Collections.unmodifiableList(all);
        cacheSquabs.put(fullPath, allRO);

        return allRO;
    }

    public <S extends Squab> List<List<String>> axes(final Class<S> squabClass, String... path) {
        return axes(new Squab.Path(squabClass, path));
    }

    /**
     * Computes possible criteria positions, enumerating undefined ones.
     *
     * @param fullPath criteria to inspect
     * @return all possible values for each criteria path position
     */
    public List<List<String>> axes(final Squab.Path fullPath) {
        final List<List<String>> cachedAxes = cacheAxes.get(fullPath);
        if (cachedAxes != null) {
            return cachedAxes;
        }

        final List<Squab.Path> paths = findAllPaths(fullPath);

        final SortedMap<Integer, TreeSet<String>> resultMap =
                new TreeMap<Integer, TreeSet<String>>();
        for (Squab.Path matchPath : paths) {
            for (int i = 0; i < matchPath.len(); i++) {
                final TreeSet<String> axis = resultMap.get(i);
                if (axis == null) {
                    TreeSet<String> newAxis = new TreeSet<String>();
                    newAxis.add(matchPath.elem(i));
                    resultMap.put(i, newAxis);
                } else {
                    axis.add(matchPath.elem(i));
                }
            }
        }

        final List<List<String>> axes = new ArrayList<List<String>>(resultMap.size());
        for (int i = 0; i < resultMap.size(); i++) {
            axes.add(Collections.unmodifiableList(new ArrayList<String>(resultMap.get(i))));
        }

        final List<List<String>> axesRO = Collections.unmodifiableList(axes);
        cacheAxes.put(fullPath, axesRO);

        return axesRO;
    }

    public <S extends Squab> List<Squab.Path> findAllPaths(Class<S> squabClass, String... path) {
        return findAllPaths(new Squab.Path(squabClass, path));
    }

    public List<Squab.Path> findAllPaths(Squab.Path fullPath) {
        final List<Squab.Path> cachedIds = cachePaths.get(fullPath);
        if (cachedIds != null) {
            return cachedIds;
        }

        final Squab.RespViewList viewList = couchList(fullPath);
        final List<Squab.Path> paths = new ArrayList<Squab.Path>(viewList.getRows().size());
        for (Squab.RespViewList.Row row : viewList.getRows()) {
            final Squab.Path rowPath = Squab.Path.fromId(row.getId());
            if (rowPath.len() < fullPath.len()) {
                continue;
            }
            boolean afterWild = false;
            boolean matches = true;
            for (int i = 0; i < fullPath.len(); i++) {
                //  path may have some wilds in the middle, so we have to filter even after couch range-query
                afterWild |= i > 0 && fullPath.elem(i - 1) == null;
                if (afterWild && i < fullPath.len() && fullPath.elem(i) != null && !fullPath.elem(i).equals(rowPath.elem(i))) {
                    matches = false;
                    break;
                }
            }

            if (matches) {
                paths.add(rowPath);
            }
        }

        final List<Squab.Path> pathsRO = Collections.unmodifiableList(paths);
        cachePaths.put(fullPath, pathsRO);

        return pathsRO;
    }

    //  Squab.Stamped
    public <S extends Squab.Stamped> S findLast(Class<S> squabClass, String... path) {
        final SortedMap<Long, S> timeToSquab = findAllStamped(squabClass, path);
        return timeToSquab.isEmpty() ? null : timeToSquab.get(timeToSquab.lastKey());
    }

    public <S extends Squab.Stamped> S findByStamp(long stamp, Class<S> squabClass, String... path) {
        return findAllStamped(squabClass, path).get(stamp);
    }

    public <S extends Squab.Stamped> SortedMap<Long, S> findAllStamped(
            Long sinceTime, Long untilTime, Class<S> squabClass, String... path
    ) {
        final SortedMap<Long, S> stampToSquab = findAllStamped(squabClass, path);
        if (sinceTime != null && untilTime != null) {
            return stampToSquab.subMap(sinceTime, untilTime);
        }
        if (sinceTime != null) {
            return stampToSquab.tailMap(sinceTime);
        }
        if (untilTime != null) {
            return stampToSquab.headMap(untilTime);
        }
        return stampToSquab;
    }

    public <S extends Squab.Stamped> SortedMap<Long, S> findAllStamped(Class<S> squabClass, String... path) {
        final Squab.Path fullPath = new Squab.Path(squabClass, path);
        final SortedMap<Long, ? extends Squab.Stamped> stampedCached = cacheStamped.get(fullPath);
        if (stampedCached != null) {
            //noinspection unchecked
            return (SortedMap<Long, S>) stampedCached;
        }

        final List<S> squabs = findAll(squabClass, path);
        final SortedMap<Long, S> timeToSquab;
        if (squabs.isEmpty()) {
            //noinspection unchecked
            timeToSquab = (SortedMap<Long, S>) EMPTY_MAP;
        } else {
            timeToSquab = byStamp(squabs);
        }

        final SortedMap<Long, S> timeToSquabRO = Collections.unmodifiableSortedMap(timeToSquab);
        cacheStamped.put(fullPath, timeToSquabRO);

        return timeToSquabRO;
    }

    protected static <S extends Squab.Stamped> SortedMap<Long, S> byStamp(final Collection<S> squabs) {
        final SortedMap<Long, S> result = new TreeMap<Long, S>();
        for (S squab : squabs) {
            result.put(squab.getStamp(), squab);
        }
        return result;
    }

    //  Couch HTTP
    protected Squab.RespViewList couchList(Squab.Path path) {
        URL url;
        HttpURLConnection connection = null;
        try {
            url = new URL(couchUrl +
                    dbName + "/" + "_all_docs/?" +
                    "startkey=\"" + URLEncoder.encode(path.rangeMin(), "UTF-8") + "\"&" +
                    "endkey=\"" + URLEncoder.encode(path.rangeMax(), "UTF-8") + "\""
            );
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setUseCaches(false);
            connection.setDoInput(true);
            connection.setDoOutput(false);

            final InputStream is = connection.getInputStream();
            final Squab.RespViewList list;
            list = mapper.readValue(is, Squab.RespViewList.class);
            is.close();

            return list;
        } catch (IOException e) {
            throw new IllegalStateException("list failed", e);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    protected <S extends Squab> S couchGet(Squab.Path path, final Class<S> squabClass) {
        URL url;
        HttpURLConnection connection = null;
        try {
            url = new URL(couchUrl + dbName + "/" + URLEncoder.encode(path.id(), "UTF-8"));
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setUseCaches(true);
            connection.setDoInput(true);
            connection.setDoOutput(false);

            final InputStream is = connection.getInputStream();
            final S squab;
            squab = mapper.readValue(is, squabClass);
            is.close();

            return squab;
        } catch (IOException e) {
            throw new IllegalStateException("GET failed", e);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    public InputSupplier<InputStream> couchFileGet(Squab.Path path, final String fileName) {
        final URL url;
        try {
            url = new URL(
                    couchUrl +
                            dbName + "/" +
                            URLEncoder.encode(path.id(), "UTF-8") + "/" +
                            URLEncoder.encode(fileName, "UTF-8")
            );

            return new InputSupplier<InputStream>() {
                public InputStream getInput() throws IOException {
                    final HttpURLConnection connection = (HttpURLConnection) url.openConnection();
                    connection.setRequestMethod("GET");
                    connection.setUseCaches(true);
                    connection.setDoInput(true);
                    connection.setDoOutput(false);

                    return new FilterInputStream(connection.getInputStream()) {
                        @Override
                        public void close() throws IOException {
                            connection.disconnect();
                            super.close();
                        }
                    };
                }
            };
        } catch (IOException e) {
            throw new IllegalStateException("failed to construct URL", e);
        }
    }

    protected void couchPut(String couchId, Squab squab) {
        HttpURLConnection connection = null;

        URL url;
        try {
            final String squabJSON = mapper.writeValueAsString(squab);

            //Create connection
            url = new URL(couchUrl + dbName + "/" + URLEncoder.encode(couchId, "UTF-8"));
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("PUT");
            connection.setRequestProperty(
                    "Content-Type",
                    "application/json"
            );
            connection.setRequestProperty(
                    "Content-Length",
                    Integer.toString(squabJSON.getBytes().length)
            );
            //  use Base64 codec bundled with Jackson: bytes -> "base64"
            final String base64Str = mapper.writeValueAsString((username + ":" + password).getBytes());
            connection.addRequestProperty(
                    "Authorization",
                    //  drop first and last chars as those are JSON quotas
                    "Basic " + base64Str.substring(1, base64Str.length() - 1)
            );
            connection.setUseCaches(false);
            connection.setDoInput(true);
            connection.setDoOutput(true);

            BufferedOutputStream bos = new BufferedOutputStream(connection.getOutputStream());
            bos.write(squabJSON.getBytes(Charsets.UTF_8));
            bos.flush();
            bos.close();

            final InputStream is = connection.getInputStream();
            final Squab.RespUpdate update =
                    mapper.readValue(is, Squab.RespUpdate.class);
            if (!update.isOk()) {
                throw new IllegalStateException("PUT failed:" + update);
            }
            is.close();
        } catch (IOException e) {
            throw new IllegalStateException("PUT failed", e);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }
}
