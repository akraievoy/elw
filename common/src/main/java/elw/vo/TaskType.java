package elw.vo;

import java.util.*;

public class TaskType implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private final SortedMap<String, Task> tasks = new TreeMap<String, Task>();
    public SortedMap<String, Task> getTasks() {
        return Collections.unmodifiableSortedMap(tasks);
    }
    public void setTasks(SortedMap<String, Task> tasks) {
        this.tasks.clear();
        if (tasks != null) {
            this.tasks.putAll(tasks);
        }
    }

    private final SortedMap<String, FileSlot> fileSlots = new TreeMap<String, FileSlot>();
    public SortedMap<String, FileSlot> getFileSlots() {
        return Collections.unmodifiableSortedMap(fileSlots);
    }
    public void setFileSlots(SortedMap<String, FileSlot> fileSlots) {
        this.fileSlots.clear();
        if (fileSlots != null) {
            this.fileSlots.putAll(fileSlots);
        }
    }
}
