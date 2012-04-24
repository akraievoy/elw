package elw.webauth;

import java.util.Collections;
import java.util.List;

/**
 * OpenID auth response, received from provider.
 */
public class OpenIdResponseResult {
    final List<String> openIds;
    final List<String> emails;

    public OpenIdResponseResult(List<String> emails, List<String> openIds) {
        this.emails = Collections.unmodifiableList(emails);
        this.openIds = Collections.unmodifiableList(openIds);
    }
}
