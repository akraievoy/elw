package com.bws.base.utils;

import javax.swing.*;

public class Die extends RuntimeException {
	private static final long serialVersionUID= -2265192338078311581L;

    public Die() {
        super();
    }

    public Die(String message) {
        super(message);
    }

    public Die(String message, Throwable cause) {
        super(message, cause);
    }

    public Die(Throwable cause) {
        super(cause);
    }

    /** Some frequently used barks.
	 * @note this is just a conveniency, not a replacement for exception types.
	 * Please use customized exception types where appropriate.
	 */
	static public Die notImplemented() {
		return new Die("notImplementedYet");
	}

	public static UnsupportedOperationException unsupported(String message) {
		if (!Str.isEmpty(message)) {
			return new UnsupportedOperationException(message);
		}

		return new UnsupportedOperationException();
	}

	public static UnsupportedOperationException unsupported() {
		return unsupported(null);
	}

	static public RuntimeException uncheck(Throwable ex) {
		final Throwable rootCause = Vm.getRootCause(ex);
		if (rootCause instanceof RuntimeException) {
			return (RuntimeException) rootCause;
		}

		if (ex instanceof RuntimeException) {
			return (RuntimeException) ex;
		}

		return new Die(ex);
	}

	/** Should be used in case of severe misconfig, rendering the app unusable. */
	public static Die criticalConfigError(String message) {
		return new Die(message);
	}

	/** Should be used in case of severe misconfig, rendering the app unusable. */
	public static Die criticalConfigError(Throwable rootCause) {
		return new Die(rootCause);
	}

	public static String mustImplement(final Class expectedClass, final Class actualClass) {
		return actualClass.getName() + " must implement " + expectedClass.getName();
	}

	public static IllegalStateException invalidState(String message) {
		return new IllegalStateException(message);
	}

	public static IllegalArgumentException invalidValue(String property, Object expected, Object actual) {
		final String msg=
			"invalid " + property + ", " +
			"expected '" + String.valueOf(expected) + "'," +
			" actual: '" + String.valueOf(actual) + "'";

		return new IllegalArgumentException(msg);
	}

	public static IllegalArgumentException expectingNotEmpty(String property, Object actual) {
		final String msg= "expecting non-empty " + property + " actual: '" + String.valueOf(actual) + "'";

		return new IllegalArgumentException(msg);
	}

	public static IllegalArgumentException unexpected(String property, Object actual) {
		final String msg= "unexpected " + property + " actual: '" + String.valueOf(actual) + "'";

		return new IllegalArgumentException(msg);
	}

	public static void ifNotEqual(String property, Object expected, Object actual) {
		if (!Jcf.nullSafeEquals(expected, actual)) {
			throw invalidValue(property, expected, actual);
		}
	}

	public static void ifEmpty(String property, Object actual) {
		if (Str.isEmpty(actual)) {
			throw expectingNotEmpty(property, actual);
		}
	}

	public static void ifFalse(boolean condition, String message) {
		if (!condition) {
			throw invalidState(message);
		}
	}

	public static void ifTrue(boolean condition, String message) {
		if (condition) {
			throw invalidState(message);
		}
	}

	public static void ifReached(String message) {
        throw invalidState(message);
	}

	public static void ifNull(String property, Object someObject) {
		ifFalse(someObject != null, String.valueOf(property) + " should not be null");
	}

	public static void ifNotNull(final String property, final Object someValue) {
		ifFalse(someValue == null, String.valueOf(property) + " should be null");
	}

    public static Error ifReached(final Throwable cause) {
		return new Error("This error state was concidered unreachable before", cause);
	}

	public static void ifNotAwt() {
		ifFalse(SwingUtilities.isEventDispatchThread(), "should be running in AWT dispatcher");
	}
}