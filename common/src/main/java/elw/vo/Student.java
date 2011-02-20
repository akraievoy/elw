package elw.vo;

public class Student extends IdName {
	private String email;
	private boolean lead;

	public boolean isLead() {
		return lead;
	}

	public void setLead(boolean lead) {
		this.lead = lead;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}
}