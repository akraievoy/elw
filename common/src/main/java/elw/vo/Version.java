package elw.vo;

import java.util.*;

public class Version implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }
    public Version withId(String id) { setId(id); return this; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public Version withName(String name) { setName(name); return this; }

    private boolean shared = false;
    public boolean isShared() { return shared; }
    public void setShared(boolean shared) { this.shared = shared; }
    
    private Map<String, List<Solution>> files = new TreeMap<String, List<Solution>>();
    private List<Solution> getFiles(final String slotId) {
        final List<Solution> filesForSlot = files.get(slotId);
        if (filesForSlot == null) {
            return Collections.emptyList();
        }
        return Collections.unmodifiableList(filesForSlot);
    }
    public void setFiles(final String slotId, List<Solution> files) {
        final List<Solution> filesForSlot = this.files.get(slotId);
        if (filesForSlot == null) {
            this.files.put(slotId, new ArrayList<Solution>(files));
            return;
        }

        filesForSlot.clear();
        if (files != null) {
            filesForSlot.addAll(files);
        }
    }
}
