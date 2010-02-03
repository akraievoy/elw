/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package ua.iasa.pathsim;

import com.bws.base.utils.Str;

import java.util.*;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: Data.java,v 1.14 2006/12/28 10:38:54 Anton S. Kraievoy Exp $
 */

public class Data {
    public static boolean isHexDigit(char c) {
        return '0' <= c && c <= '9' || 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f';
    }

    public static boolean isLetter(char c) {
        return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z';
    }

    public static boolean isDecDigit(char c) {
        return '0' <= c && c <= '9';
    }
    
    public static long int2long(int value) {
        if (value >= 0) {
            return value;
        } else {
            return -2 * (long)Integer.MIN_VALUE + value;
        }
    }

    public static boolean isHexPositive(String token) {
        for (int i = 0; i < token.length(); i++) {
            if (!isHexDigit(token.charAt(i))) return false;
        }
        return true;
    }

    public static boolean isDecPositive(String token) {
        for (int i = 0; i < token.length(); i++) {
            if (!isDecDigit(token.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean isHexPositive(String token, int numberOfDigits) {
        token = token.toUpperCase();

        if (token.length() == 0) return false;

        for (int i = 0; i < token.length(); i++) {
            if (!isHexDigit(token.charAt(i))) return false;
        }

        return token.length() <= numberOfDigits;
    }

    public static int valueOfHex(char c) {
        if ('0' <= c && c <= '9') return c - '0';
        if ('A' <= c && c <= 'F') return 10 + c - 'A';
        if ('a' <= c && c <= 'f') return 10 + c - 'a';

        throw new IllegalArgumentException("Illegal: '" + c + "'");
    }

    public static int valueOfDec(char c) {
        if ('0' <= c && c <= '9') return c - '0';

        throw new IllegalArgumentException("illegal: '" + c + "'");
    }

    public static long hex2long(String address) {
        long value = 0;

        for (int i = 0; i < address.length(); i++) {
            value = 16 * value + valueOfHex(address.charAt(i));
        }

        return value;
    }

    public static String invert(final String hex) {
        StringBuffer result = new StringBuffer();

        for (int i = 0; i < hex.length(); i++) {
            final int value = valueOfHex(hex.charAt(i));
            final int invertedValue = 15 - value;
            result.append(Str.HEX_DIGITS.charAt(invertedValue));
        }

        return result.toString();
    }

    public static int hex2int(String hex) {
        if (hex.length() == 0) {
            return 0;
        }

        if (hex.length() < 8 || '0' <= hex.charAt(0) && hex.charAt(0) <= '7') {
            return (int) hex2long(hex);
        } else {
            return (int) -(hex2long(invert(hex)) + 1L);
        }
    }

    public static String int2hex(int value, int numberOfDigits) {
        if (value >= 0) {
            return long2hex(value, numberOfDigits);
        } else {
            return long2hex(-2 * (long)Integer.MIN_VALUE + value);
        }
    }

    public static int dec2int(String registerNumber) {
        int value = 0;

        for (int i = 0; i < registerNumber.length(); i++) {
            value = 10 * value + valueOfDec(registerNumber.charAt(i));
        }

        return value;
    }

    public static String long2hex(long word) {
        return long2hex(word, 1);
    }

    public static String long2hex(long number, int numberOfDigits) {
        if (number < 0) {
            System.out.println("number = " + number);
            return int2hex((int) number, numberOfDigits);
        }

        StringBuffer result = new StringBuffer();
        long intermediateValue = number;
        int digitCount = 0;

        while (intermediateValue > 0 || digitCount < numberOfDigits) {
            int digitValue = (int) (intermediateValue % 16);
            result.insert(0, Str.HEX_DIGITS.charAt(digitValue));
            digitCount++;
            intermediateValue /= 16;
        }

        return result.toString();
    }

    public static String int2dec(int number, int numberOfDigits) {
        final StringBuffer result = new StringBuffer();
        final boolean sign;

        long intermediateValue;
        if (number < 0) {
            sign = true;
            intermediateValue = -(long)number;
        } else {
            sign = false;
            intermediateValue = number;
        }

        int digitCount = 0;
        while (intermediateValue > 0 || digitCount < numberOfDigits) {
            int digitValue = (int) (intermediateValue % 10);
            result.insert(0, Str.DEC_DIGITS.charAt(digitValue));
            digitCount++;
            intermediateValue /= 10;
        }

        if (sign) {
            result.insert(0, "-");
        }

        return result.toString();
    }

    public static String int2dec(int word) {
        return int2dec(word, 1);
    }

    public static List<String> extractCode(final String rawText) {
        final List<String> result = new ArrayList<String>();
        final String[] lines = rawText.split("\n");

        for (String line : lines) {
            final String codeStr = line.replaceFirst("#.+$", "");

            if (Str.isEmpty(codeStr)) {
                continue;
            }

            result.add(codeStr.trim().toUpperCase());
        }

        return result;
    }
}

