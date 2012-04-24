package elw.webauth;

/** Simple checked umbrella over this pile of some openId exceptions. */
public class AuthException extends Exception {
    public AuthException(String message, Throwable cause) {
        super(message, cause);
    }
}
