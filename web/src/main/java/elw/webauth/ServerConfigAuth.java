package elw.webauth;

/**
 * Auth-centric configuration options.
 */
public interface ServerConfigAuth {
    String getBaseUrl();

    boolean isRelyingPartyIdent();

    String getFormRedirect();

    String getFormRedirectParam();

    long getSessionExpiryMillis();

    String getSmtpUser();

    String getSmtpPass();

    String getSmtpFrom();

    String getSmtpSubject();

    boolean getSmtpAuth();

    boolean getSmtpStartTls();

    String getSmtpHost();

    int getSmtpPort();

    String getMailProtocol();

    String getSmtpSocketFactory();

    boolean isSmtpDebug();

    long getMailTargetDelayMillis();

    long getMailSourceDelayMillis();

    String getMailTokenSalt();

    String getMailBody();

    String getMailResponseForm();
}
