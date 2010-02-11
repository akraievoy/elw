/*
 * Copyright (c) 2006 Anton Kraievoy, Alexander Iotko.
 */
package com.bws.base.utils;

import java.io.*;

/**
 * Several useful methods for generic IO.
 *
 * @author Anton Kraievoy
 */
public class Io {
    private static final java.util.logging.Logger log = java.util.logging.Logger.getLogger(Io.class.getName());
    /**
     * One day, in millis.
     */
    public static final int OUTDATE_THRESH = 24 * 60 * 60 * 1000;

    private Io() { /* Intentionally left empty. */ }

    /**
     * Chunk length to read data. Default value is 1024 * 32 
     */
    public final static int CHUNK_SIZE = 1024 * 32;

    /**
     * Pumps data from source to target until EOF occurs or IOException is thrown.
     *  
     * @param srcStream source of data
     * @param destination destination for data 
     * @throws IOException
     */
    public static void pumpData(final InputStream srcStream, final OutputStream destination) throws IOException {
        //  create it each time, as we will have multiple threads working here
        final byte[] buffer = new byte[CHUNK_SIZE];

        int readBytesCount;

        //  actually it is strange why we should retry on zero...
        //  but... as -1 was specified as EOF marker, I think we should
        while ((readBytesCount = srcStream.read(buffer)) != -1) {
            destination.write(buffer, 0, readBytesCount);
        }
        destination.flush();
    }

    /**
     * Pumps data from source to target until EOF occurs or IOException is thrown.
     * <b>Note</b>: this method must rely on some predefined character encoding to be consistent across different platforms.
     * 
     * @param srcReader source of data
     * @param destWriter destination for data 
     * @throws IOException
     */
    public static void pumpData(final Reader srcReader, final Writer destWriter) throws IOException {
        //  create it each time, as we will have multiple threads working here
        final char[] buffer = new char[CHUNK_SIZE];

        int readCharsCount;

        //  actually it is strange why we should retry on zero...
        //  but... as -1 was specified as EOF marker, I think we should
        while ((readCharsCount = srcReader.read(buffer)) >= 0) {
            destWriter.write(buffer, 0, readCharsCount);
        }
        destWriter.flush();
    }

    /**
     * <b>Note</b>: source stream will be automatically closed.
     */
    public static String dumpToString(final InputStream sourceStream, final String encoding) throws IOException {
        final BufferedReader source = new BufferedReader(new InputStreamReader(sourceStream, encoding));
        final StringWriter result = new StringWriter();
        final BufferedWriter target = new BufferedWriter(result);

        pumpData(source, target);

        source.close();
        target.close();

        return result.toString();
    }

    public static String dumpToString(final InputStream sourceStream) throws IOException {
        return dumpToString(sourceStream, System.getProperty("file.encoding"));
    }

    /**
     * <b>Note</b>: source stream will be automatically closed.
     */
    public static byte[] dumpToByteArray(final InputStream source) throws IOException {
        final ByteArrayOutputStream result = new ByteArrayOutputStream();

        pumpData(source, result);

        source.close();
        result.close();

        return result.toByteArray();
    }

    public static void safeClose(final OutputStream target) {
        try { if (target != null) { target.close(); } } catch (IOException ioe) { log.warning("safeClose(): " + ioe.getMessage()); }
    }

    public static void safeClose(final InputStream source) {
        try { if (source != null) { source.close(); } } catch (IOException ioe) { log.warning("safeClose(): " + ioe.getMessage()); }
    }

    public static void safeClose(final BufferedInputStream source) {
        try { if (source != null) { source.close(); } } catch (IOException ioe) { log.warning("safeClose(): " + ioe.getMessage()); }
    }

    public static String getResource(Class owner, String resource, final String encoding) {
        try {
            return dumpToString(owner.getResourceAsStream(resource), encoding);
        } catch (IOException e) {
            throw Die.criticalConfigError("failed to resolve '"+ resource+ "' from class " + owner.getName());
        }
    }
}