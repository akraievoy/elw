package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.annotate.JsonIgnoreProperties;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.annotate.JsonWriteNullProperties;

import javax.annotation.Nonnull;
import java.lang.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.TreeMap;

/**
 * CouchDB Entity.
 */
public abstract class Squab {
    protected static final String PATH_SEP = "-";
    protected static final String PATH_SEP_MAX = ".";
    protected static final String PATH_WILD = "*";

    private TreeMap<String, CouchFile> couchFiles = null;
    @JsonProperty("_attachments")
    @JsonWriteNullProperties(false)
    public TreeMap<String, CouchFile> getCouchFiles() { return couchFiles; }
    @JsonProperty("_attachments")
    public void setCouchFiles(TreeMap<String, CouchFile> couchFiles) { this.couchFiles = couchFiles; }

    protected String couchId = "";
    protected String couchRev = "";

    @JsonProperty("_id")
    @JsonWriteNullProperties(false)
    public String getCouchId() {
        return couchId != null && couchId.length() == 0 ? null : couchId;
    }

    @JsonProperty("_id")
    public void setCouchId(String couchId) {
        if (couchId == null) {
            throw new IllegalArgumentException("couchId is null");
        }
        this.couchId = couchId;
    }

    @JsonProperty("_rev")
    @JsonWriteNullProperties(false)
    public String getCouchRev() {
        return couchRev != null && couchRev.length() == 0 ? null : couchRev;
    }

    @JsonProperty("_rev")
    public void setCouchRev(String couchRev) {
        if (couchRev == null) {
            throw new IllegalArgumentException("couchRev is null");
        }
        this.couchRev = couchRev;
    }

    public CouchFile getCouchFile(String name) {
        return couchFiles == null ? null : couchFiles.get(name);
    }

    public void putCouchFile(String name, CouchFile couchFile) {
        if (couchFiles == null) {
            couchFiles = new TreeMap<String, CouchFile>();
        }
        couchFiles.put(name, couchFile);
    }

    @JsonIgnore
    public Path getCouchPath() {
        if (couchId == null || couchId.length() == 0) {
            couchId = couchPath(pathElems()).id();
        }
        return Path.fromId(couchId);
    }

    protected Path couchPath(String... pathElems) {
        return new Path(getClass(), pathElems);
    }

    protected abstract String[] pathElems();

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Squab)) return false;

        Squab squab = (Squab) o;

        return couchId.equals(squab.couchId) && couchRev.equals(squab.couchRev);
    }

    @Override
    public int hashCode() {
        int result = couchRev.hashCode();
        result = 31 * result + couchId.hashCode();
        return result;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName());
        sb.append("{couchKey='").append(couchId).append('\'');
        sb.append(", couchRev='").append(couchRev).append('\'');
        sb.append('}');
        return sb.toString();
    }

    public static class Path {
        public Path(@Nonnull String... elems) {
            this.elems = elems;
        }
        public Path(@Nonnull java.lang.Class<? extends Squab> squabClass, @Nonnull String... elems) {
            this.elems = new String[elems.length + 1];
            this.elems[0] = squabClass.getSimpleName();
            System.arraycopy(elems, 0, this.elems, 1, elems.length);
        }

        private final String[] elems;
        public String[] elems() { return elems.clone(); }
        public String elem(final int index) { return elems[index]; }
        public int len() { return elems.length; }

        @SuppressWarnings({"SimplifiableIfStatement"})
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return Arrays.equals(elems, ((Path) o).elems);
        }

        @Override
        public int hashCode() {
            return elems != null ? Arrays.hashCode(elems) : 0;
        }

        @Override
        public String toString() {
            return "{" + Arrays.deepToString(elems) + "}";
        }

        public boolean intersects(final Path that) {
            final int len = Math.min(this.elems.length, that.elems.length);
            for (int i = 0; i < len; i++) {
                final String eThis = this.elems[i];
                final String eThat = that.elems[i];
                if (eThis != null && eThat != null && !eThis.equals(eThat)) {
                    return false;
                }
            }
            return true;
        }

        private Path set(final int index, final String value) {
            final String[] newPath = this.elems.clone();
            newPath[index] = value;
            return new Path(newPath);
        }

        public Path setLast(final String value) {
            return set(this.elems.length - 1, value);
        }

        public String id() {
            final StringBuilder idBuilder = new StringBuilder();
            for (String pathElem : elems) {
                idBuilder.append(pathElem == null ? PATH_WILD : pathElem);
                idBuilder.append(PATH_SEP);
            }
            return idBuilder.toString();
        }

        public String rangeMin() {
            final StringBuilder idBuilder = new StringBuilder();
            for (String pathElem : elems) {
                if (pathElem == null) {
                    break;
                }
                idBuilder.append(pathElem);
                idBuilder.append(PATH_SEP);
            }
            return idBuilder.toString();
        }

        public String rangeMax() {
            final StringBuilder idBuilder = new StringBuilder();
            for (String pathElem : elems) {
                if (pathElem == null) {
                    break;
                }
                idBuilder.append(pathElem);
                idBuilder.append(PATH_SEP);
            }
            idBuilder.delete(idBuilder.length()-PATH_SEP.length(), idBuilder.length());
            idBuilder.append(PATH_SEP_MAX);
            return idBuilder.toString();
        }

        public static Path fromId(final String id) {
            if (id == null) {
                return null;
            }
            if (!id.endsWith(PATH_SEP)) {
                throw new IllegalStateException("'" + id + "' does not end with '" + PATH_SEP + "'");
            }

            final String idSafe = id.substring(0, id.length() - PATH_SEP.length());
            final String[] elems = idSafe.split(PATH_SEP);

            for (int i = 0; i < elems.length; i++) {
                if (PATH_WILD.equals(elems[i])) {
                    elems[i] = null;
                }
            }

            return new Path(elems);
        }
    }

    /**
     * Same as super, but tracks stamp (millis, base36) as last part of path.
     */
    public static abstract class Stamped extends Squab {
        protected Long stamp = null;
        public Long getStamp() { return stamp; }
        public void setStamp(Long stamp) { this.stamp = stamp; }

        protected Path couchPath(String... pathElems) {
            if (stamp == null) {
                stamp = genStamp();
            }
            final String[] elemsWithStamp = new String[pathElems.length + 1];
            System.arraycopy(pathElems, 0, elemsWithStamp, 0, pathElems.length);
            elemsWithStamp[elemsWithStamp.length - 1] = Long.toString(stamp, 36);
            return new Path(getClass(), elemsWithStamp);
        }

        public long updateStamp() {
            this.stamp = genStamp();
            super.couchId = null;
            super.couchRev = null;
            return this.stamp;
        }

        private static long lastStamp = 0;
        public static long genStamp() {
            synchronized (Stamped.class) {
                return lastStamp = Math.max(System.currentTimeMillis(), lastStamp + 1);
            }
        }
    }

    //  LATER document which fields are required/provided on PUT and GET
    public static class CouchFile {
        private byte[] data;
        @JsonWriteNullProperties(false)
        public byte[] getData() { return data; }
        public void setData(byte[] data) { this.data = data; }

        private String contentType;
        @JsonProperty("content_type")
        public String getContentType() { return contentType; }
        @JsonProperty("content_type")
        public void setContentType(String contentType) { this.contentType = contentType; }

        private Long length;
        @JsonWriteNullProperties(false)
        public Long getLength() { return length; }
        public void setLength(Long length) { this.length = length; }

        private Long revpos;
        @JsonWriteNullProperties(false)
        public Long getRevpos() { return revpos; }
        public void setRevpos(Long revpos) { this.revpos = revpos; }

        private Boolean stub;
        @JsonWriteNullProperties(false)
        public Boolean getStub() { return stub; }
        public void setStub(Boolean stub) { this.stub = stub; }
    }

    public static class RespChanges {
        private long lastSeq;
        @JsonProperty("last_seq")
        public long getLastSeq() { return lastSeq; }
        @JsonProperty("last_seq")
        public void setLastSeq(long lastSeq) { this.lastSeq = lastSeq; }

        private ArrayList<Change> results;
        public ArrayList<Change> getResults() { return results; }
        public void setResults(ArrayList<Change> results) { this.results = results; }

        public static class Change {
            private String id;
            public String getId() { return id; }
            public void setId(String id) { this.id = id; }

            private long seq;
            public long getSeq() { return seq; }
            public void setSeq(long seq) { this.seq = seq; }

            private TreeMap<String, String> changes;
            public TreeMap<String, String> getChanges() { return changes; }
            public void setChanges(TreeMap<String, String> changes) { this.changes = changes; }
        }
    }

    public static class RespUpdate {
        private boolean ok;
        public boolean isOk() { return ok; }
        public void setOk(boolean ok) { this.ok = ok; }

        private String failed;
        public String getFailed() { return failed; }
        public void setFailed(String failed) { this.failed = failed; }

        private String id;
        public String getId() { return id; }
        public void setId(String id) { this.id = id; }

        private String rev;
        public String getRev() { return rev; }
        public void setRev(String rev) { this.rev = rev; }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            sb.append("{failed='").append(failed).append('\'');
            sb.append(", ok=").append(ok);
            sb.append(", id='").append(id).append('\'');
            sb.append(", rev='").append(rev).append('\'');
            sb.append('}');
            return sb.toString();
        }
    }

    public static class RespViewList {
        public static class Row {
            @JsonIgnoreProperties("doc")
            public static class Value {
                private String rev;
                public String getRev() { return rev; }
                public void setRev(String rev) { this.rev = rev; }
            }

            private String id;
            public String getId() { return id; }
            public void setId(String id) { this.id = id; }

            private String key;
            public String getKey() { return key; }
            public void setKey(String key) { this.key = key; }

            private Value value;
            public Value getValue() { return value; }
            public void setValue(Value value) { this.value = value; }
        }

        private int totalRows;
        @JsonProperty("total_rows")
        public int getTotalRows() { return totalRows; }
        @JsonProperty("total_rows")
        public void setTotalRows(int totalRows) { this.totalRows = totalRows; }

        private int offset;
        public int getOffset() { return offset; }
        public void setOffset(int offset) { this.offset = offset; }

        private ArrayList<Row> rows = null;
        public ArrayList<Row> getRows() { return rows; }
        public void setRows(ArrayList<Row> rows) { this.rows = rows; }
    }
}
