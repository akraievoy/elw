package elw.vo;

import java.util.*;

public class TaskType implements IdNamed, Cloneable {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private SortedMap<String, Task> tasks = new TreeMap<String, Task>();
    public SortedMap<String, Task> getTasks() {
        return Collections.unmodifiableSortedMap(tasks);
    }
    public void setTasks(SortedMap<String, Task> tasks) {
        this.tasks.clear();
        if (tasks != null) {
            this.tasks.putAll(tasks);
        }
    }

    private SortedMap<String, FileSlot> fileSlots = new TreeMap<String, FileSlot>();
    public SortedMap<String, FileSlot> getFileSlots() {
        return Collections.unmodifiableSortedMap(fileSlots);
    }
    public void setFileSlots(SortedMap<String, FileSlot> fileSlots) {
        this.fileSlots.clear();
        if (fileSlots != null) {
            this.fileSlots.putAll(fileSlots);
        }
    }

    @Override
    public TaskType clone() throws CloneNotSupportedException {
        final TaskType clone = (TaskType) super.clone();
        
        clone.tasks = new TreeMap<String, Task>();
        for (Map.Entry<String, Task> taskEntry : tasks.entrySet()) {
            clone.tasks.put(taskEntry.getKey(), taskEntry.getValue().clone());
        }
        
        clone.fileSlots = new TreeMap<String, FileSlot>();
        for (Map.Entry<String, FileSlot> slotEntry : fileSlots.entrySet()) {
            clone.fileSlots.put(slotEntry.getKey(), slotEntry.getValue().clone());
        }
        
        return clone;
    }
}
