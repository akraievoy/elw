package elw.vo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Student implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private String email;
    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }

    private List<String> openIds = new ArrayList<String>();
    public List<String> getOpenIds() {
        return Collections.unmodifiableList(openIds);
    }
    public void setOpenIds(ArrayList<String> openIds) {
        this.openIds.clear();
        if (openIds != null) {
            this.openIds.addAll(openIds);
        }
    }

    private boolean lead;
    public boolean isLead() { return lead; }
    public void setLead(boolean lead) { this.lead = lead; }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Student student = (Student) o;
        return id.equals(student.id);
    }

    @Override
    public int hashCode() {
        return id.hashCode();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Student{ ");
        sb.append("id ").append(id).append(' ');
        sb.append("name ").append(name).append(' ');
        sb.append("email ").append(email).append(' ');
        sb.append('}');
        return sb.toString();
    }
}