package elw.vo;

import java.util.*;

import static java.util.Collections.unmodifiableSortedMap;

public class FileSlot implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private final List<String> readApprovals = new ArrayList<String>();
    public List<String> getReadApprovals() {
        return Collections.unmodifiableList(readApprovals);
    }
    public void setReadApprovals(List<String> readApprovals) {
        this.readApprovals.clear();
        if (readApprovals != null) {
            this.readApprovals.addAll(readApprovals);
        }
    }

    private final List<String> writeApprovals = new ArrayList<String>();
    public List<String> getWriteApprovals() {
        return Collections.unmodifiableList(writeApprovals);
    }
    public void setWriteApprovals(List<String> writeApprovals) {
        this.writeApprovals.clear();
        if (writeApprovals != null) {
            this.writeApprovals.addAll(writeApprovals);
        }
    }

    private final SortedMap<String, FileType> fileTypes = new TreeMap<String, FileType>();
    public SortedMap<String, FileType> getFileTypes() {
        return unmodifiableSortedMap(fileTypes);
    }
    public void setFileTypes(SortedMap<String, FileType> fileTypes) {
        this.fileTypes.clear();
        if (fileTypes != null) {
            this.fileTypes.putAll(fileTypes);
        }
    }

    private boolean writable;
    public boolean isWritable() { return writable; }
    public void setWritable(boolean writable) { this.writable = writable; }

    private double scoreWeight = 0.0;
    public double getScoreWeight() { return scoreWeight; }
    public void setScoreWeight(double scoreWeight) { this.scoreWeight = scoreWeight; }

    private final SortedMap<String, Criteria> criterias = new TreeMap<String, Criteria>();
    public SortedMap<String, Criteria> getCriterias() {
        return Collections.unmodifiableSortedMap(this.criterias);
    }
    public void setCriterias(SortedMap<String, Criteria> criterias) {
        this.criterias.clear();
        if (criterias != null) {
            this.criterias.putAll(criterias);
        }
    }
}
