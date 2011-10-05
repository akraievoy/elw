package elw.vo;

import java.util.*;

public class Enrollment extends Squab implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private String groupId;
    public String getGroupId() { return groupId; }
    public void setGroupId(String groupId) { this.groupId = groupId; }

    private String courseId;
    public String getCourseId() { return courseId; }
    public void setCourseId(String courseId) { this.courseId = courseId; }

    private List<Class> classes = new ArrayList<Class>();
    public List<Class> getClasses() {
        return Collections.unmodifiableList(classes);
    }
    public void setClasses(List<Class> classes) {
        this.classes.clear();
        if (classes != null) {
            this.classes.addAll(classes);
        }
    }

    private List<IndexEntry> index = new ArrayList<IndexEntry>();
    public List<IndexEntry> getIndex() {
        return Collections.unmodifiableList(index);
    }
    public void setIndex(List<IndexEntry> index) {
        this.index.clear();
        if (index != null) {
            this.index.addAll(index);
        }
    }

    @Override
    protected String[] pathElems() {
        return new String[] { groupId, courseId, id };
    }

    public boolean checkOnTime(elw.vo.Stamped stamped) {
        for (Class aClass : classes) {
            if (aClass.checkOnTime(stamped)) {
                return true;
            }
        }

        return false;
    }

    public int cmpTotalBudget() {
        int totalBudget = 0;
        for (IndexEntry ie : getIndex()) {
            totalBudget += ie.getScoreBudget();
        }
        return totalBudget;
    }
}
