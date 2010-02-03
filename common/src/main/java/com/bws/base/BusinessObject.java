/*
 * Copyright (c) 2006 Anton Kraievoy, Alexander Iotko.
 */
package com.bws.base;

import java.io.Serializable;

/**
 * This is an extension of parent interface, with several commonly used 
 * attributes that may be simply ignored by successors.
 * 
 * @author Anton Kraievoy
 */
public interface BusinessObject extends Serializable {
    Long getId();

    void setId(final Long newId);

    void setName(final String newName);

    String getName();

    boolean isDeleted();

    void setDeleted(final boolean newDeleted);

    long getUserId();

    void setUserId(final long newUserId);
}
