package elw.vo;

import java.util.*;

import static java.util.Collections.copy;
import static java.util.Collections.unmodifiableSortedMap;

public class FileSlot implements IdNamed, Cloneable {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }
    public FileSlot withId(String id) { setId(id); return this; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private List<String> readApprovals = new ArrayList<String>();
    public List<String> getReadApprovals() {
        return Collections.unmodifiableList(readApprovals);
    }
    public void setReadApprovals(List<String> readApprovals) {
        this.readApprovals.clear();
        if (readApprovals != null) {
            this.readApprovals.addAll(readApprovals);
        }
    }

    private List<String> writeApprovals = new ArrayList<String>();
    public List<String> getWriteApprovals() {
        return Collections.unmodifiableList(writeApprovals);
    }
    public void setWriteApprovals(List<String> writeApprovals) {
        this.writeApprovals.clear();
        if (writeApprovals != null) {
            this.writeApprovals.addAll(writeApprovals);
        }
    }

    private SortedMap<String, FileType> fileTypes = new TreeMap<String, FileType>();
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

    private SortedMap<String, Criteria> criterias = new TreeMap<String, Criteria>();
    public SortedMap<String, Criteria> getCriterias() {
        return Collections.unmodifiableSortedMap(this.criterias);
    }
    public void setCriterias(SortedMap<String, Criteria> criterias) {
        this.criterias.clear();
        if (criterias != null) {
            this.criterias.putAll(criterias);
        }
    }

    @Override
    public FileSlot clone() throws CloneNotSupportedException {
        final FileSlot clone = (FileSlot) super.clone();
        
        clone.readApprovals = new ArrayList<String>(readApprovals);
        clone.writeApprovals = new ArrayList<String>(writeApprovals);

        clone.fileTypes = new TreeMap<String, FileType>();
        for (Map.Entry<String, FileType> fTypeEntry : fileTypes.entrySet()) {
            clone.fileTypes.put(
                    fTypeEntry.getKey(),
                    fTypeEntry.getValue().clone()
            );
        }
        
        clone.criterias = new TreeMap<String, Criteria>();
        for (Map.Entry<String, Criteria> critEntry : criterias.entrySet()) {
            clone.criterias.put(
                    critEntry.getKey(),
                    critEntry.getValue().clone()
            );
        }
        
        return clone;
    }
}
