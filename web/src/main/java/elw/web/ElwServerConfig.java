package elw.web;

/**
 * Passive victim of Spring property injection (and, possibly, other injectors).
 */
public class ElwServerConfig {
    private String baseUrl;

    public ElwServerConfig() {
        //  default constructor, nothing to do here
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl;
    }
}
