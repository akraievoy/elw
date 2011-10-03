package elw.vo;

import java.util.*;

import static java.util.Collections.unmodifiableSortedMap;

public class Course extends Squab implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private String template;
    public String getTemplate() { return template; }
    public void setTemplate(String template) { this.template = template; }

    private final SortedMap<String, Criteria> criterias = new TreeMap<String, Criteria>();
    public SortedMap<String, Criteria> getCriterias() {
        return unmodifiableSortedMap(criterias);
    }
    public void setCriterias(SortedMap<String, Criteria> criterias) {
        this.criterias.clear();
        if (criterias != null) {
            this.criterias.putAll(criterias);
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

    private final SortedMap<String, TaskType> taskTypes = new TreeMap<String, TaskType>();
    public SortedMap<String, TaskType> getTaskTypes() {
        return unmodifiableSortedMap(taskTypes);
    }
    public void setTaskTypes(SortedMap<String, TaskType> taskTypes) {
        this.taskTypes.clear();
        if (taskTypes != null) {
            this.taskTypes.putAll(taskTypes);
        }
    }

    @Override
    protected String[] pathElems() { return new String[]{id}; }
}