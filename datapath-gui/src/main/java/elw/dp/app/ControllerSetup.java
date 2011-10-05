package elw.dp.app;

/**
 * LATER add javadocs for a class created by anton
 */
public interface ControllerSetup {
    void setBaseUrl(String baseUrl);

    void setUploadHeader(String uploadHeader);

    void setElwCtx(String elwCtx);

    String getElwCtx();

    String getBaseUrl();

    String getUploadHeader();
}
