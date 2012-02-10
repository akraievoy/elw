package elw.vo;

public class Version implements IdNamed, Cloneable {
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
    
    @Override
    public Version clone() throws CloneNotSupportedException {
        final Version clone = (Version) super.clone();

        //  nothing more to do here

        return clone;
    }
}
