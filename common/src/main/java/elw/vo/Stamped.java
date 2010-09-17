package elw.vo;

public interface Stamped {

	Stamp getCreateStamp();

	void setCreateStamp(Stamp newStamp);

	Stamp getUpdateStamp();

	void setUpdateStamp(Stamp newStamp);
}
