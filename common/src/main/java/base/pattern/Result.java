package base.pattern;

import org.slf4j.Logger;

import javax.annotation.Nullable;

//  LATER wipe this leaky solution due to following drawbacks:
//      there should be no non-localized messages in the sources
//      results from various places are cross-assignable
//      success/failure is easily discernable from the enum value name by a simple convention
public class Result {
    private final boolean success;
    private final String message;

    public Result(String message, boolean success) {
        this.message = message;
        this.success = success;
    }

    public static void success(Result[] resultRef, final String message) {
        if (resultRef == null || resultRef.length < 1) {
            return;
        }

        resultRef[0] = new Result(message, true);
    }

    private static void failure(@Nullable Result[] resultRef, final String message) {
        if (resultRef == null || resultRef.length < 1) {
            return;
        }

        resultRef[0] = new Result(message, false);
    }

    public static void success(@Nullable final Logger log, Result[] resultRef, final String message) {
        if (log != null) {
            log.info(message);
        }
        success(resultRef, message);
    }

    public static void failure(@Nullable final Logger log, @Nullable Result[] resultRef, final String message) {
        if (log != null) {
            log.warn(message);
        }
        failure(resultRef, message);
    }

    public String getMessage() {
        return message;
    }

    public boolean isSuccess() {
        return success;
    }
}
