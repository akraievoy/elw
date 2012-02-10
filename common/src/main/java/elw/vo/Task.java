package elw.vo;

import java.util.Collections;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.akraievoy.couch.Squab;

public class Task extends Squab implements IdNamed, Cloneable {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }
    public Task withId(String id) { setId(id); return this; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    private SortedMap<String, Version> versions = new TreeMap<String, Version>();
    public SortedMap<String, Version> getVersions() {
        return Collections.unmodifiableSortedMap(versions);
    }
    public void setVersions(SortedMap<String, Version> versions) {
        this.versions.clear();
        if (versions != null) {
            this.versions.putAll(versions);
        }
    }

    private final String[] extraPathElems;
    public Task() {
        this.extraPathElems = null;
    }
    public Task(String[] extraPathElems) {
        this.extraPathElems = extraPathElems;
    }

    @Override
    protected String[] pathElems() {
        if (extraPathElems == null || extraPathElems.length != 2) {
            throw new IllegalStateException("pathElems: courseId" + PATH_SEP + "tTypeId" + PATH_SEP + "taskId");
        } else {
            return new String[] {extraPathElems[0], extraPathElems[1], id};
        }
    }

    @Override
    public Task clone() throws CloneNotSupportedException {
        final Task clone = (Task) super.clone();
        
        clone.versions = new TreeMap<String, Version>();
        for (Map.Entry<String, Version> verEntry : versions.entrySet()) {
            clone.versions.put(verEntry.getKey(), verEntry.getValue().clone());
        }

        return clone;
    }
}
