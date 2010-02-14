package base.pattern;

import org.slf4j.Logger;

public class Result {
	protected final boolean success;
	protected final String message;

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

	public static void failure(Result[] resultRef, final String message) {
		if (resultRef == null || resultRef.length < 1) {
			return;
		}

		resultRef[0] = new Result(message, false);
	}

	public static void success(final Logger log, Result[] resultRef, final String message) {
		log.info(message);
		success(resultRef, message);
	}

	public static void failure(final Logger log, Result[] resultRef, final String message) {
		log.warn(message);
		failure(resultRef, message);
	}

	public String getMessage() {
		return message;
	}

	public boolean isSuccess() {
		return success;
	}
}
