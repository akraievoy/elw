package elw.vo;

import java.util.SortedMap;
import java.util.TreeMap;

import static java.util.Collections.unmodifiableSortedMap;

import org.akraievoy.couch.Squab;

public abstract class FileBase extends Squab.Stamped implements IdNamed, Stamped {
    public static final String CONTENT = "content";

    protected String id;
    public static final int DETECT_SIZE_LIMIT = 1024 * 1024 * 1024;
    public static final String[] SCOPES = new String[] {Attachment.SCOPE, Solution.SCOPE};

    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) {
        this.name = name;
        this.id = this.name.replaceAll("\\s+", "_").replaceAll(PATH_SEP, "");
    }

    private String comment;
    public String getComment() { return comment; }
    public void setComment(String comment) { this.comment = comment; }

    private String sourceAddress;
    public String getSourceAddress() { return sourceAddress; }
    public void setSourceAddress(String sourceAddress) { this.sourceAddress = sourceAddress; }

    private String author;
    public String getAuthor() { return author; }
    public void setAuthor(String author) { this.author = author; }

    private final SortedMap<String, FileType> fileType = new TreeMap<String, FileType>();
    public SortedMap<String, FileType> getFileType() {
        return unmodifiableSortedMap(fileType);
    }
    public void setFileType(SortedMap<String, FileType> fileType) {
        this.fileType.clear();
        if (fileType != null) {
            this.fileType.putAll(fileType);
        }
    }

    public long computeSize() {
        return getCouchFile(CONTENT).getLength();
    }

    @Deprecated
    public abstract void setupPathElems(Ctx ctx, FileSlot slot);

    public abstract void setupPathElems(String[] pathElems);
}
