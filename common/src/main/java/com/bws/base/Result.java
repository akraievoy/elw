package com.bws.base;

/*
* Copyright (c) 2006 Anton Kraievoy, Alexander Iotko.
*/

import java.io.Serializable;
import com.bws.base.utils.*;

/**
 * Simple structure holding result of abstract operation.
 * 
 * @author Anton Kraievoy
 */
public class Result implements Serializable {
    public static final String OUTSTATE_SUCCESS = "success";
    public static final String OUTSTATE_FAILURE = "failure";
    public static final String OUTSTATE_DEPENDENT = "dependent";
    public static final String OUTSTATE_VALIDATION = "validation";

    public static final Result SUCCESS = simple(true);
    public static final Result FAILURE = simple(false);
    public static final Result FAILURE_DEPENDENT = message(OUTSTATE_DEPENDENT, null);
    public static final Result FAILURE_VALIDATION = message(OUTSTATE_VALIDATION, null);

    String outState;
    String messageKey;

    public String getMessageKey() {
        return messageKey;
    }

    public String getMessage() {
        return Str.isEmpty(messageKey) ? "" : Msg.get(messageKey);
    }

    public String getMessage(final String keyBase) {
        final String messageOverride = getMessage();
        if (!Str.isEmpty(messageOverride)) {
            return messageOverride;
        }

        final String actualKey = keyBase + "." + outState;
        if (Msg.containsKey(actualKey)) {
            return Msg.get(actualKey);
        } else {
            return actualKey;
        }
    }

    public boolean isSuccessful() {
        return OUTSTATE_SUCCESS.equals(outState);
    }

    public boolean isFailed() {
        return OUTSTATE_FAILURE.equals(outState);
    }

    public static Result simple(boolean newSuccessful) {
        return message(newSuccessful ? OUTSTATE_SUCCESS : OUTSTATE_FAILURE, null);
    }

    public static Result message(String newOutState, String messageKey) {
        if (newOutState == null) {
            throw new IllegalArgumentException("newOutState should not be null");
        }

        final Result result = new Result();
        result.outState = newOutState;
        result.messageKey = messageKey;
        return result;
    }

    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        final Result result = (Result) o;
        return outState.equals(result.outState);
    }

    public int hashCode() {
        return outState.hashCode();
    }
}