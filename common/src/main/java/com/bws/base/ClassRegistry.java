/*
 * Copyright (c) 2006 Anton Kraievoy, Alexander Iotko.
 */
package com.bws.base;

import java.util.*;
import java.util.logging.Logger;

/**
 * This class simply maps interface classes to implementation classes, latter ones are referenced via class name.
 * <p/>
 * Note that no compile-time dependencies are posed from interface to implementation, which is lazily loaded.
 *
 * @author Anton Kraievoy
 */
public abstract class ClassRegistry {
    private static final Logger log = Logger.getLogger(ClassRegistry.class.getName());

    private boolean classesLoaded = false;
    private Map<Class, String> classToImpl = new HashMap<Class, String>();
    private Map<Class, Class> classToClass = new HashMap<Class, Class>();

    protected ClassRegistry() {
    }

    public synchronized Class get(final Class keyClass) {
        ensureClassesLoaded();
        return classToClass.get(keyClass);
    }

    protected synchronized void ensureClassesLoaded() {
        if (!classesLoaded) {
            log.info("ensureClassesLoaded() setting up class mapping");

            classToImpl.clear();
            setupClassMapping(classToImpl);

            log.info("ensureClassesLoaded() loading classes");

            Set facadeClasses = classToImpl.keySet();

            for (Object facadeClass1 : facadeClasses) {
                final Class facadeClass = (Class) facadeClass1;
                final String implName = classToImpl.get(facadeClass);
                try {
                    classToClass.put(facadeClass, ClassRegistry.class.getClassLoader().loadClass(implName));
                } catch (ClassNotFoundException e) {
                    log.severe("ensureClassesLoaded() error for '" + facadeClass + "': " + e.getMessage());
                }
            }

            classesLoaded = true;
        }
    }

    public synchronized void reset() {
        classToClass.clear();
        classToImpl.clear();
        classesLoaded = false;
    }

    /**
     * Put pairs of published interface class (as key, of type <u>Class</u>) and corresponding implementation class name
     * (as <u>String</u> value).
     */
    protected abstract void setupClassMapping(final Map<Class, String> newClassToImpl);

    public synchronized Object instantiate(final Class keyClass) {
        ensureClassesLoaded();
        try {
            final Class implClass = get(keyClass);

            if (!keyClass.isAssignableFrom(implClass)) {
                throw new ReflectiveConfigurationError("" + keyClass + " is mapped to " + implClass + " which does not implement the first");
            }

            return instantiateInternal(implClass);
        } catch (Throwable e) {
            log.severe("instantiate() failed due to " + e.getMessage());
            throw new ReflectiveConfigurationError("Failed instantiating impl for " + keyClass, e);
        }
    }

    protected Object instantiateInternal(Class implClass) throws InstantiationException, IllegalAccessException {
        return implClass.newInstance();
    }
}