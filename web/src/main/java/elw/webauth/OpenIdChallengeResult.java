package elw.webauth;

import org.springframework.http.HttpMethod;

import java.util.Collections;
import java.util.Map;

/**
 * OpenID challenge result (this should be sent to provider).
 */
public class OpenIdChallengeResult {
    final String targetUrl;
    final Map parameterMap;
    final HttpMethod method;

    @SuppressWarnings("unchecked")
    public OpenIdChallengeResult(
            HttpMethod method,
            String targetUrl,
            Map parameterMap
    ) {
        this.method = method;
        this.targetUrl = targetUrl;
        this.parameterMap = Collections.unmodifiableMap(parameterMap);
    }
}
