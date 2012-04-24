package elw.web;

import elw.dao.ConfigLoader;
import elw.webauth.ServerConfigAuth;
import org.akraievoy.couch.CouchSetup;
import org.apache.commons.configuration.CompositeConfiguration;

import java.util.Map;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * This bean delegates to specific CompositeConfiguration, which is
 * set up to be reloaded in runtime.
 */
public class ElwServerConfig implements ServerConfigAuth, CouchSetup {
    private final CompositeConfiguration configuration;

    public ElwServerConfig() {
        configuration = ConfigLoader.getConfiguration("elw");
    }

    public String getBaseUrl() {
        return configuration.getString("auth.baseUrl");
    }

    public boolean isRelyingPartyIdent() {
        return configuration.getBoolean("auth.openId.relyingPartyIdent");
    }

    public String getFormRedirect() {
        return configuration.getString("auth.openId.formRedirect");
    }

    public String getFormRedirectParam() {
        return configuration.getString("auth.openId.formRedirectParam");
    }

    public long getSessionExpiryMillis() {
        return configuration.getLong("auth.sessionExpiryMillis");
    }

    public String getSmtpUser() {
        return configuration.getString("auth.smtp.user");
    }

    public String getSmtpPass() {
        return configuration.getString("auth.smtp.pass");
    }

    public String getSmtpFrom() {
        return configuration.getString("auth.smtp.from");
    }

    public String getSmtpSubject() {
        return configuration.getString("auth.smtp.subject");
    }

    public boolean getSmtpAuth() {
        return configuration.getBoolean("auth.smtp.auth");
    }

    public boolean getSmtpStartTls() {
        return configuration.getBoolean("auth.smtp.startTls");
    }

    public String getSmtpHost() {
        return configuration.getString("auth.smtp.host");
    }

    public int getSmtpPort() {
        return configuration.getInt("auth.smtp.port");
    }

    public String getMailProtocol() {
        return configuration.getString("auth.mailProtocol");
    }

    public String getSmtpSocketFactory() {
        return configuration.getString("auth.smtp.socketFactory");
    }

    public boolean isSmtpDebug() {
        return configuration.getBoolean("auth.smtp.debug");
    }

    public long getMailTargetDelayMillis() {
        return configuration.getLong("auth.mailTargetDelayMillis");
    }

    public long getMailSourceDelayMillis() {
        return configuration.getLong("auth.mailSourceDelayMillis");
    }

    public long getMailResponseTimeoutMillis() {
        return configuration.getLong("auth.mailRespTimeoutMillis");
    }

    public String getMailTokenSalt() {
        return configuration.getString("auth.mailTokenSalt");
    }

    public String getMailBody() {
        return configuration.getString("auth.mail.body");
    }

    public String getMailResponseForm() {
        return configuration.getString("auth.mail.responseForm");
    }

    public String getCouchDbUrl() {
        return configuration.getString("couchdb.url");
    }

    public String getCouchDbUser() {
        return configuration.getString("couchdb.user");
    }

    public String getCouchDbPassword() {
        return configuration.getString("couchdb.password");
    }

    public Map<String, String> getCouchDbNames() {
        final Properties dbNameProps = configuration.getProperties(
            "couchdb.dbnames"
        );

        final SortedMap<String, String> dbNameMap =
            new TreeMap<String, String>();

        for (Map.Entry<Object, Object> entry : dbNameProps.entrySet()) {
            dbNameMap.put(
                String.valueOf(entry.getKey()),
                String.valueOf(entry.getValue())
            );
        }

        return dbNameMap;
    }
}
