package com.bws.base.utils;

import com.bws.base.BusinessObject;
import java.io.File;
import java.text.*;
import org.apache.commons.lang.*;

import java.util.*;
import java.util.logging.*;
import java.util.regex.Pattern;

/**
 * Common code for parsing data values from Strings.
 * Several fine utility methods like isEmpty() and substEmpty().
 *
 * @author Anton Kraievoy
 */
public class Str {
    private static final Logger log = Logger.getLogger(Str.class.getName());

    public static final String DATE_PATTERN = "MM/dd/yyyy";
    public static final String DATE_PATTERN_HOURS = "MM/dd/yyyy hh:mm";
    public static final String DATE_PATTERN_MILLIS = "MM/dd/yy hh:mm:ss.SSS";

    private static final SimpleDateFormat sdfDate = new SimpleDateFormat(DATE_PATTERN);
    private static final SimpleDateFormat sdfDatetime = new SimpleDateFormat(DATE_PATTERN_HOURS);
    private static final SimpleDateFormat sdfDatetimeMillis = new SimpleDateFormat(DATE_PATTERN_MILLIS);

    private static final Pattern NUMBER_PATTERN = Pattern.compile("^[0-9]+$");
    private static final Pattern NONCYRILLIC_PATTERN = Pattern.compile("^\\P{InCyrillic}*$");
    private static final Pattern ALNUMSPACE_PATTERN = Pattern.compile("^(\\p{Alnum}|\\p{Blank})*$");

    private static final String VALID_SINGLELINE = "1234567890abcdefghijklmonpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ~!@#$%^&*()_+-=[]{};':\",./<>?";
    private static final String VALID_MULTILINE = VALID_SINGLELINE + "\n\r";

    /**
     * @deprecated use {@link Jcf#LONG_ZERO instead}
     */
    public static final Long ZERO = Jcf.LONG_ZERO;
    public static final String EMPTY = "";
    public static final String[] EMPTY_STRING_ARRAY = new String[0];
    public static String NL = new String(new byte[]{13, 10});
    public static final String ALPHA_LOW = "abcdefghijklmnopqrstuvwxyz";
    public static final String ALPHA_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    public static final String NUMERIC = "1234567890";
    public static final String ALPHA_ALL = ALPHA_LOW+ALPHA_UPPER;
    public static final String ALPHANUMERIC = ALPHA_ALL+NUMERIC;
    protected static final NumberFormat defaultDecimalFormat = new DecimalFormat("###########0.##", new DecimalFormatSymbols(Locale.ENGLISH));
    public static final String SPACE = " ";
    public static final String HEX_DIGITS = "0123456789ABCDEF";
    public static final String DEC_DIGITS = "0123456789";

    protected Str() {
        // Utility class with sealed public constructor
    }

    public static int parseInt(String value, int defaultVal) {
        if (isEmpty(value)) {
            return defaultVal;
        }

        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return defaultVal;
        }
    }

    public static double parseDouble(final String value, final int defaultVal) {
        if (isEmpty(value)) {
            return defaultVal;
        }

        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return defaultVal;
        }
    }

    public static Double parseDouble(final String value, final Double defaultVal) {
        if (isEmpty(value)) {
            return defaultVal;
        }

        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return defaultVal;
        }
    }

    public static Long parseLong(String value, Long defaultVal) {
        if (isEmpty(value)) {
            return defaultVal;
        }

        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            return defaultVal;
        }
    }

    public static long parseLong(String value, long defaultVal) {
        if (isEmpty(value)) {
            return defaultVal;
        }

        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            return defaultVal;
        }
    }

    public static Boolean parseBoolean(String value, Boolean defaultVal) {
        if (isEmpty(value)) {
            return defaultVal;
        }

        try {
            return Boolean.valueOf(value);
        } catch (NumberFormatException e) {
            return defaultVal;
        }
    }

    public static String substEmpty(String value, String defaultVal) {
        if (isEmpty(value)) {
            return defaultVal;
        }

        return value;
    }

    public static boolean isEmpty(String value) {
        return value == null || value.trim().length() <= 0;
    }

    public static boolean isEmpty(Object value) {
        return value == null || value.toString().trim().length() <= 0;
    }

    public static String repeatString(final String baseString, final int length) {
        return repeatStringAsBuffer(baseString, length).toString();
    }

    public static StringBuffer repeatStringAsBuffer(final String baseString, final int length) {
        StringBuffer result = new StringBuffer();
        for (int columnIndex = 0; columnIndex < length; columnIndex++) {
            result.append(baseString);
        }
        return result;
    }

    public static StringBuffer repeatAndAppendString(final String baseString, final StringBuffer toAppend, final int length) {
        for (int columnIndex = 0; columnIndex < length; columnIndex++) {
            toAppend.append(baseString);
        }
        return toAppend;
    }

    public static boolean isNumber(final String parameter) {
        return !isEmpty(parameter) && NUMBER_PATTERN.matcher(parameter).matches();
    }

    /**
     * if last characted in stringbuffer is comma -- it is removed from
     * stringbuffer
     *
     * @return the same StringBufffer that was passed into method.
     */
    public static StringBuffer removeLastComma(final StringBuffer sql) {
        if (sql.charAt(sql.length() - 1) == ',') {
            sql.setLength(sql.length() - 1);
        }
        return sql;
    }

    public static boolean checkEmail(String email) {
        return checkEmail(email, null);
    }

    public static boolean checkEmail(String email, String regexp) {
        if (regexp == null) {
            regexp = "^[a-z0-9]+([_\\.\\-a-z0-9\'])*@([a-z0-9]+([\\.\\-a-z0-9])*)\\.[a-z][a-z]+$";
        }
        return email.toLowerCase().matches(regexp);
    }

    /**
     * <p>Escapes the characters in a <code>String</code> using HTML entities.</p>
     * <p/>
     * <p/>
     * For example: <tt>"bread" & "butter"</tt> => <tt>&amp;quot;bread&amp;quot; &amp;amp; &amp;quot;butter&amp;quot;</tt>.
     * </p>
     * <p/>
     * <p>Supports all known HTML 4.0 entities, including funky accents.</p>
     *
     * @param content the <code>String</code> to escape, may be null
     * @return a new escaped <code>String</code>, <code>null</code> if null string input
     * @see StringEscapeUtils#unescapeHtml(String)
     * @see </br><a href="http://hotwired.lycos.com/webmonkey/reference/special_characters/">ISO Entities</a>
     * @see </br><a href="http://www.w3.org/TR/REC-html32#latin1">HTML 3.2 Character Entities for ISO Latin-1</a>
     * @see </br><a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">HTML 4.0 Character entity references</a>
     * @see </br><a href="http://www.w3.org/TR/html401/charset.html#h-5.3">HTML 4.01 Character References</a>
     * @see </br><a href="http://www.w3.org/TR/html401/charset.html#code-position">HTML 4.01 Code positions</a>
     */
    public static String escapeHTML(final String content) {
        return StringEscapeUtils.escapeHtml(content != null ? content : EMPTY);
    }

    public static String extractFilenameFromPath(String path) {
        return new File(replaceAll(substEmpty(path, ""), "\\", "/")).getName();
    }

    public static boolean contains(String[] strings, String string) {
        for (String string1 : strings) {
            if (Jcf.nullSafeEquals(string, string1)) {
                return true;
            }
        }
        return false;
    }

    public static String[] append(String[] strings, final String newString) {
        final String[] newConstrainedColumns = new String[strings.length + 1];
        System.arraycopy(strings, 0, newConstrainedColumns, 0, strings.length);
        newConstrainedColumns[strings.length] = newString;
        return newConstrainedColumns;
    }

    public static boolean isAlnumOrSpace(String aString) {
        return ALNUMSPACE_PATTERN.matcher(aString).matches();
    }

    public static boolean isFileNameCharacters(String aString) {
        return NONCYRILLIC_PATTERN.matcher(aString).matches();
    }

    /**
     * This is not a regex-based method, it treats params as regular strings.
     */
    public static String replaceAll(final String content, final String searchPattern, final String replacement) {
        return org.apache.commons.lang.StringUtils.replace(content, searchPattern, replacement);
    }

    public static Long[] parseLongs(final String[] strings, final Long defaultVal) {
        final Long[] result = new Long[strings.length];
        for (int index = 0; index < result.length; index++) {
            result[index] = parseLong(strings[index], defaultVal);
        }
        return result;
    }

    /**
     * Handles only not-null Collection of String objects. 
     */
    public static List<Long> parseLongs(final Collection<Long> idCollection) {
        final List<Long> result = new ArrayList<Long>();
        for (Object anIdCollection : idCollection) {
            final Long value = Str.parseLong((String) anIdCollection, null);
            if (value != null) {
                result.add(value);
            }
        }
        return result;
    }

    public static long[] parseLongs(final String[] strings, final long defaultVal) {
        final long[] result = new long[strings.length];
        for (int index = 0; index < result.length; index++) {
            result[index] = parseLong(strings[index], defaultVal);
        }
        return result;
    }

     public static String getCustomStackTrace( Throwable throwable ) {
         final StringBuffer result = new StringBuffer();
         result.append(throwable.toString());
         final String br = "<br>";
         result.append(br);

         StackTraceElement[] stackTrace = throwable.getStackTrace();
         for (int i = 0; i < stackTrace.length; i++) {
             StackTraceElement stackTraceElement = stackTrace[i];
             if (i > 0 && !stackTraceElement.getClassName().startsWith("com.artifactnetwork.")) {
                 result.append("<span style='color: #CCCCFF'>").append(stackTraceElement).append("</span>");
             } else {
                 result.append("<span style='color: #444444'>").append(stackTraceElement).append("</span>");
             }
             result.append(br);
         }
         if (throwable.getCause() != null) {
             result.append("<span style='color: white; background-color: black;'>Caused by</span>");
             result.append(br);
             result.append(getCustomStackTrace(throwable.getCause()));
         }
         return result.toString();
     }

     public static boolean isLongerThan(String value, int charCount) {
         return value != null && value.length() > charCount;
     }

    public static boolean isValidSingleLine(final String singleLine) {
        return StringUtils.containsOnly(singleLine, VALID_SINGLELINE);
    }

    public static boolean isValidMultiLine(final String singleLine) {
        return StringUtils.containsOnly(singleLine, VALID_MULTILINE);
    }

    public static String nullsafeTrim(final String someString) {
        return someString == null ? null : someString.trim();
    }

    public static String nullSafeToString(Object obj, String defaultValue) {
        if (obj == null) {
            return defaultValue;
        }
        return obj.toString();
    }

    public static String web(final Object obj) {
        return substEmpty(escapeHTML(nullSafeToString(obj, EMPTY)), "&nbsp;");
    }

    public static boolean hasValidPrefix(final String cardNumber, final String[] validPrefixes) {
        for (String validPrefix : validPrefixes) {
            if (cardNumber.startsWith(validPrefix)) {
                return true;
            }
        }
        return false;
    }

    public static boolean hasValidLength(final String cardNumber, final int[] validLengths) {
        final int length = cardNumber.length();
        for (int validLength : validLengths) {
            if (length == validLength) {
                return true;
            }
        }
        return false;
    }

    public static String concat(final Object[] tokens, final String infix) {
        return StringUtils.join(tokens, infix);
    }

    public static String concat(final int[] tokens, final String infix) {
        return StringUtils.join(Jcf.asIntegerObjs(tokens), infix);
    }

    public static String randomString(final int maxLen, final String sourceChars) {
        StringBuffer result = new StringBuffer();

        final int newLen = Jcf.randomPositiveInt() % maxLen + 1;

        for (int charIndex = 0; charIndex < newLen; charIndex++) {
            final int sourceIndex = Jcf.randomPositiveInt() % sourceChars.length();
            result.append(sourceChars.charAt(sourceIndex));
        }

        return result.toString();
    }

    public static String randomString(final int maxLen) {
        return randomString(maxLen, ALPHA_ALL);
    }

    public static String lcsIgnoreCase(String strA, String strB) {
        return lcs(strA.toLowerCase(), strB.toLowerCase());
    }

    public static String lcs(String strA, String strB) {
        int[][] l = new int[strA.length() + 1][strB.length() + 1];

        for (int aIndex = strA.length(); aIndex >= 0; aIndex--) {
            for (int bIndex = strB.length(); bIndex >= 0; bIndex--) {
                if (aIndex == strA.length() || bIndex == strB.length()) {
                    l[aIndex][bIndex] = 0;
                } else if (strA.charAt(aIndex) == strB.charAt(bIndex)) {
                    l[aIndex][bIndex] = 1 + l[aIndex + 1][bIndex + 1];
                } else {
                    l[aIndex][bIndex] = Math.max(l[aIndex + 1][bIndex], l[aIndex][bIndex + 1]);
                }
            }
        }

        StringBuffer result = new StringBuffer();
        int posA = 0;
        int posB = 0;

        while (posA < strA.length() && posB < strB.length()) {
            if (l[posA][posB] == l[posA + 1][posB]) {
                posA++;
            } else if (l[posA][posB] == l[posA][posB + 1]) {
                posB++;
            } else {
                result.append(strA.charAt(posA));
                posB++;
                posA++;
            }
        }

        return result.toString();
    }

    public static String getDate(java.util.Date aDate) {
        return aDate != null ? sdfDate.format(aDate) : EMPTY;
    }

    public static String getDatetime(java.util.Date aDate) {
        return sdfDatetime.format(aDate);
    }

    public static String getDatetimeMillis(java.util.Date aDate) {
        return sdfDatetimeMillis.format(aDate);
    }

    /** If no objects will have LCS longer than one half of key being searched then null will be returned. */
    public static BusinessObject suggestObjectByName(final String searchedName, final BusinessObject[] objects) {
        int bestIndex = -1;
        String bestLCS = repeatString(".", searchedName.length() / 2);

        for (int objectIndex = 0; objectIndex < objects.length; objectIndex++) {
            BusinessObject objectExt = objects[objectIndex];
            final String currentName = objectExt.getName();

            final String currentLCS = lcsIgnoreCase(currentName, searchedName);
            if (currentLCS.length() >= bestLCS.length()) {
                bestIndex = objectIndex;
                bestLCS = currentLCS;
            }
        }

        if (bestIndex >= 0) {
            final BusinessObject object = objects[bestIndex];

            if (log.isLoggable(Level.FINEST)) {
                log.log(
                        Level.FINEST,
                        "suggestObjectByName() returning " + object + " for searchedName '" + searchedName + "', " +
                        "bestLCS is '" + bestLCS + "'"
                );
            }

            return object;
        }

        if (log.isLoggable(Level.FINE)) {
            log.fine("suggestObjectByName() returning null for searchedName '" + searchedName + "'");
        }

        return null;
    }

    public static String extractClass(final Class someClass) {
        final String fqn = someClass.getName();
        final int lastDot = fqn.lastIndexOf(".");
        if (lastDot >= 0) {
            return fqn.substring(lastDot + 1);
        }
        return fqn;
    }

    public static String stripWhitespace(final String someText) {
        if (someText == null) {
            return EMPTY;
        }

        //  note that unicode NBSP is treated as whitespace
        String charsToReplace = " \t\n\u000B\f\r\u00A0";
        StringBuffer result = new StringBuffer(someText);

        int charIndex = 0;
        boolean whitespaceToken = false;

        while (charIndex < result.length()) {
            final boolean newWhitespaceToken = charsToReplace.indexOf(result.charAt(charIndex)) >= 0;

            if (newWhitespaceToken) {
                if (!whitespaceToken) {
                    result.setCharAt(charIndex, ' ');
                    charIndex++;
                } else {
                    result.deleteCharAt(charIndex);
                }
            } else {
                charIndex++;
            }

            whitespaceToken = newWhitespaceToken;
        }

        return result.toString();
    }

    /**
     * Right pads the given string with whitespace characters
     * @param input input string
     * @param length the length till which to pad
     * @return right padded string
     */
    public static String padWhiteSpace(String input, int length) {
        if (Str.isEmpty(input)) { // return blank padded string
            return Str.repeatString(" ", length);
        }

        if (input.length() >= length) { // no padding required
            return input;
        }

        final int diff = length - input.length();
        return input + Str.repeatString(" ", diff);
    }

    /**
     * Returns string if its length is less or equal to maxlength, truncated string to maxLength otherwise.
     * @param string
     * @param maxLength
     */
    public static String truncate(String string, int maxLength) {
        if (string!=null && string.length()>maxLength) {
            string = string.substring(0, maxLength);
        }
        return string;
    }

    public static String formatNumber(final Number somenumber) {
        return formatNumber(somenumber, "0");
    }

    public static String formatNumber(final double somenumber) {
        return formatNumber(new Double(somenumber));
    }

    public static String formatNumber(final float somenumber) {
        return formatNumber(new Float(somenumber));
    }

    public static synchronized String formatNumber(final Number somenumber, final String defaultValue) {
        if (somenumber == null) {
            return defaultValue;
        }
        return defaultDecimalFormat.format(somenumber.doubleValue());
    }

    public static String replaceSomeQuotationUnicodeCharsToLowAscii(String text) {
        text = text.replace((char)0x2018, (char)0x27); // left single quotation mark
        text = text.replace((char)0x2019, (char)0x27); // right single quotation mark
        text = text.replace((char)0x201A, (char)0x27); // single low-9 quotation mark
        text = text.replace((char)0x201C, (char)0x22); // left double quotation mark
        text = text.replace((char)0x201D, (char)0x22); // right double quotation mark
        text = text.replace((char)0x201E, (char)0x22); // double low-9 quotation mark
        text = text.replace((char)0x2013, (char)0x2D); // en dash
        text = text.replace((char)0x2014, (char)0x2D); // em dash
        text = text.replace((char)0x2015, (char)0x2D); // horizontal bar
        return text;
    }

    public static String valueOf(Object obj, String defaultValue) {
        if (obj==null) {
            return defaultValue;
        }
        return String.valueOf(obj);
    }

    public static int indexOf(String searchTarget, String[] strings) {
        for (int strIndex = 0; strIndex < strings.length; strIndex++) {
            final String str = strings[strIndex];
            if (Jcf.nullSafeEquals(searchTarget, str)) {
                return strIndex;
            }
        }

        return -1;
    }
}