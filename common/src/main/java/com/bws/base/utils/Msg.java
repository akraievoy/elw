package com.bws.base.utils;

import java.io.*;

import java.util.*;
import java.util.logging.Level;

/**
 * Cross-module message bundle implementation.
 *
 * @author Anton Kraievoy $Id: Msg.java,v 1.2 2006/12/27 20:31:40 Anton S. Kraievoy Exp $
 */
public class Msg {
    private static final java.util.logging.Logger log = java.util.logging.Logger.getLogger(Msg.class.getName());
    protected static final String BUNDLES_CHARSET = "UTF-8";

    protected static final String[] NAMES = {
            "com/bws/eChess/messages.properties"
    };

    protected static ResourceBundle msg = null;

    public static boolean containsKey(final String searchKey) {
        return Jcf.contains(getMsg().getKeys(), searchKey);
    }

    public static String get(final String key) {
        if (!containsKey(key)) {
            log.warning("Lookup failed for key '" + key + "'");
            return key;
        }

        return getMsg().getString(key);
    }

    protected static ResourceBundle createMsg() {
        try {
            StringWriter mergedProps = new StringWriter();

            for (String aNAMES : NAMES) {
                final InputStream resourceStream = Msg.class.getResourceAsStream(aNAMES);
                if (resourceStream == null) {
                    log.warning(aNAMES + " is not in classpath");
                    continue;
                }
                final InputStreamReader resourceReader = new InputStreamReader(resourceStream, BUNDLES_CHARSET);
                Io.pumpData(resourceReader, mergedProps);
                resourceReader.close();
                mergedProps.write("\n");
            }

            mergedProps.flush();
            mergedProps.close();

            return new PropertyResourceBundle(new ByteArrayInputStream(mergedProps.toString().getBytes(BUNDLES_CHARSET)));
        } catch (IOException e) {
            log.log(Level.SEVERE, "Error accessing bundle sources", e);

            try {
                return new PropertyResourceBundle(new ByteArrayInputStream(new byte[0]));
            } catch (IOException couldNotHappen) {
                log.log(Level.SEVERE, "Error creating fake bundle source, giving up", couldNotHappen);

                throw new RuntimeException(couldNotHappen);
            }
        }
    }

    protected static synchronized ResourceBundle getMsg() {
        if (msg == null) {
            msg = createMsg();
        }
        return msg;
    }
}