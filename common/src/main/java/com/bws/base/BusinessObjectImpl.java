/*
 * Copyright (c) 2006 Anton Kraievoy, Alexander Iotko.
 */
package com.bws.base;

import com.bws.base.utils.Str;

/**
 * Default implementation of BusinessObjectExt interface.
 *
 * @author Anton Kraievoy
 */
public abstract class BusinessObjectImpl implements BusinessObject {
    protected Long id;
    protected String name;
    protected boolean deleted;

    protected Long accountId;
    protected Long userId;
    protected Long projectId;

    public Long getId() {
        return id;
    }

    public void setId(final Long newId) {
        id = newId;
    }

    public void setName(final String newName) {
        name = newName;
    }

    public String getName() {
        return name;
    }

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(final boolean newDeleted) {
        deleted = newDeleted;
    }

    public long getUserId() {
        return userId;
    }

    public void setUserId(final long newUserId) {
        userId = newUserId;
    }

    public void setUserId(final Long newUserId) {
        userId = newUserId;
    }

    public boolean equals(Object that) {
        if (this == that) return true;
        if (!(that instanceof BusinessObjectImpl)) return false;
        final BusinessObject objectExt = (BusinessObject) that;
        return getId() == null ? objectExt.getId() != null : getId().equals(objectExt.getId());
    }

    public int hashCode() {
        return getId() == null ? 0 : getId().hashCode();
    }

    public String toString() {
        return Str.extractClass(getClass()) + "#" + String.valueOf(getId()) + "{" + getName() + "}";
    }
}