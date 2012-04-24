package elw.webauth;

/**
 * Returning two tokens from one digest-ish method.
 */
public class EmailAuthTokens {
    public final String challengeToken;
    public final String responseToken;

    public EmailAuthTokens(
            final String challengeToken,
            final String responseToken
    ) {
        this.challengeToken = challengeToken;
        this.responseToken = responseToken;
    }
}
