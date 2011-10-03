package elw.vo;

public interface Ctx {
    String STATE_NONE = "";
    String STATE_G = "g";
    String STATE_GS = "gs";
    String STATE_ECG = "ecg";
    String STATE_ECGS = "ecgs";
    String STATE_C = "c";
    String STATE_CT = "ct";
    String STATE_CTA = "cta";
    String STATE_CTAV = "ctav";
    String STATE_CIV = "civ";
    String STATE_EGSCIV = "egsciv";

    Task getAss();

    TaskType getAssType();

    Course getCourse();

    Enrollment getEnr();

    Group getGroup();

    Student getStudent();

    Version getVer();

    int getIndex();

    boolean resolved(String state);
}
