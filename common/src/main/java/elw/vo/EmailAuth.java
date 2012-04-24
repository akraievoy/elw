package elw.vo;

import org.akraievoy.couch.Squab;

/**
 * Information required for email authorization.
 */
public class EmailAuth extends Squab.Stamped {
    private String email;
    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }

    private boolean activated;
    public boolean isActivated() { return activated; }
    public void setActivated(boolean activated) { this.activated = activated; }

    @Override
    protected String[] pathElems() {
        return new String[] {email};
    }
}
