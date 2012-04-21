package elw.web;

import org.openid4java.association.AssociationException;
import org.openid4java.consumer.ConsumerException;
import org.openid4java.consumer.ConsumerManager;
import org.openid4java.consumer.VerificationResult;
import org.openid4java.discovery.DiscoveryException;
import org.openid4java.discovery.DiscoveryInformation;
import org.openid4java.discovery.Identifier;
import org.openid4java.discovery.yadis.YadisResolver;
import org.openid4java.message.AuthRequest;
import org.openid4java.message.MessageException;
import org.openid4java.message.ParameterList;
import org.openid4java.message.ax.FetchRequest;
import org.openid4java.server.RealmVerifier;
import org.openid4java.server.RealmVerifierFactory;
import org.openid4java.util.HttpFetcherFactory;
import org.springframework.http.HttpMethod;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * OpenID and email-based auth logic should be kept within this area.
 */
public class AuthTool {
    private static final String SESSION_OID_DISCOVERY = "OID_DISCOVERY";
    private static final String SESSION_OID_CONSUMER = "OID_CONSUMER";

    private static final String OID_ENDPOINT_YAHOO =
            "https://me.yahoo.com";
    private static final String OID_ENDPOINT_GOOGLE =
            "https://www.google.com/accounts/o8/id";


    private static RealmVerifierFactory realmVerifierFactory =
            //  what the heck, I did not see anything like that in the docs?..
            new RealmVerifierFactory(
                    new YadisResolver(
                            new HttpFetcherFactory()
                    )
            );

    public static class ChallengeResult {
        final String targetUrl;
        final Map params;
        final HttpMethod method;

        public ChallengeResult(
                HttpMethod method,
                String targetUrl, 
                Map<String, String> params
        ) {
            this.method = method;
            this.targetUrl = targetUrl;
            this.params = Collections.unmodifiableMap(params);
        }
    }
    
    /** Simple checked umbrella over this pile of some openId exceptions. */
    public static class AuthException extends Exception {
        public AuthException(String message, Throwable cause) {
            super(message, cause);
        }
    }

    private final ElwServerConfig elwServerConfig;

    public AuthTool(ElwServerConfig elwServerConfig) {
        this.elwServerConfig = elwServerConfig;
    }

    public ChallengeResult openIdChallenge(
            final String oidServerEndpoint,
            final HttpServletRequest request,
            final HttpSession session
    ) throws AuthException {

        //  coding this up this just like the QuickStart sample says, see:
        //      http://code.google.com/p/openid4java/wiki/QuickStart
        //  adding some extra code for exception handling

        try {

            // perform discovery on the user-supplied identifier
            final ConsumerManager oidConsumer = oidConsumer(session, true);
            final List discoveries = oidConsumer.discover(oidServerEndpoint);

            // attempt to associate with the OpenID provider
            // and retrieve one service endpoint for authentication
            final DiscoveryInformation discovered =
                    oidConsumer.associate(discoveries);

            // store the discovery information in the user's session for later use
            // leave out for stateless operation / if there is no session
            session.setAttribute(SESSION_OID_DISCOVERY, discovered);

            // obtain a AuthRequest message to be sent to the OpenID provider
            final AuthRequest authReq =
                    oidConsumer.authenticate(
                            discovered,
                            elwServerConfig.getBaseUrl()
                    );

            FetchRequest fetch = FetchRequest.createFetchRequest();
            if (OID_ENDPOINT_GOOGLE.equals(oidServerEndpoint)) {
                fetch.addAttribute(
                        "email",
                        "http://axschema.org/contact/email",
                        true
                );
            } else if (OID_ENDPOINT_YAHOO.equals(oidServerEndpoint)) {
                fetch.addAttribute(
                        "email",
                        "http://axschema.org/contact/email",
                        true
                );
            } else {
                //works for myOpenID
                fetch.addAttribute(
                        "email",
                        "http://schema.openid.net/contact/email",
                        true
                );
            }

            if (!fetch.getAttributes().isEmpty()) {
                authReq.addExtension(fetch);
            }

            if (discovered.isVersion2()) {
                return new ChallengeResult(
                        HttpMethod.POST,
                        authReq.getDestinationUrl(false),
                        authReq.getParameterMap()
                );
            }

            return new ChallengeResult(
                    HttpMethod.GET,
                    authReq.getDestinationUrl(true),
                    Collections.EMPTY_MAP
            );
        } catch (DiscoveryException e) {
            throw new AuthException("OpenID auth failed", e);
        } catch (MessageException e) {
            throw new AuthException("OpenID auth failed", e);
        } catch (ConsumerException e) {
            throw new AuthException("OpenID auth failed", e);
        }
    }
    
    public String openIdResponse(
            final HttpServletRequest request,
            final HttpSession session
    ) throws AuthException {
        final ConsumerManager oidConsumer = oidConsumer(session, false);
        if (oidConsumer == null) {
            throw new AuthException(
                    "no ConsumerManager in session", new IllegalStateException()
            );
        }

        final Identifier verified;

        try {
            // extract the parameters from the authentication response
            // (which comes in as a HTTP request from the OpenID provider)
            final ParameterList openidResp =
                    new ParameterList(request.getParameterMap());

            // retrieve the previously stored discovery information
            final DiscoveryInformation discovered =
                    (DiscoveryInformation) session.getAttribute(SESSION_OID_DISCOVERY);

            // extract the receiving URL from the HTTP request
            final StringBuffer receivingURL = request.getRequestURL();
            final String queryString = request.getQueryString();
            if (queryString != null && queryString.length() > 0)
                receivingURL.append("?").append(request.getQueryString());

            // verify the response
            final VerificationResult verification =
                    oidConsumer.verify(
                            receivingURL.toString(),
                            openidResp,
                            discovered
                    );

            // examine the verification result and extract the verified identifier
            verified = verification.getVerifiedId();
        } catch (MessageException e) {
            throw new AuthException("OpenID auth failed", e);
        } catch (DiscoveryException e) {
            throw new AuthException("OpenID auth failed", e);
        } catch (AssociationException e) {
            throw new AuthException("OpenID auth failed", e);
        } finally {
            session.removeAttribute(SESSION_OID_DISCOVERY);
            session.removeAttribute(SESSION_OID_CONSUMER);
        }

        if (verified != null) {
            return verified.getIdentifier();
        }

        throw new AuthException(
                "OpenID authentication failed", new IllegalStateException()
        );

    }

    protected static ConsumerManager oidConsumer(
            final HttpSession session, final boolean create
    ) {
        final Object existing = session.getAttribute(SESSION_OID_CONSUMER);

        if (existing != null || !create) {
            return (ConsumerManager) existing;
        }

        final ConsumerManager created = new ConsumerManager();

        RealmVerifier rv = realmVerifierFactory.getRealmVerifierForConsumer();
        rv.setEnforceRpId(false);
        created.setRealmVerifier(rv);

        session.setAttribute(SESSION_OID_CONSUMER, created);

        return created;
    }

}
