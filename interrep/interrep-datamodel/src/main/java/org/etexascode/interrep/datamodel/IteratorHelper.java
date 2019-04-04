/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.interrep.datamodel;

import java.util.Iterator;

import org.etexascode.CoberturaIgnore;

/**
 * This class provides a helper for an iterator
 * 
 * @author ablatt
 * @param <E> The interface of the type.
 * @param <T> The type.
 */
public class IteratorHelper<E, T> implements Iterator<E> {

    /**
     * Iterator object
     */
    private final Iterator<T> iter;

    /**
     * Construct
     * 
     * @param iter The item for helper to assist with
     */
    public IteratorHelper(Iterator<T> iter) {
        this.iter = iter;
    }

    /**
     * Checks if there is anything else to iterate through
     */
    @CoberturaIgnore
    @Override
    public boolean hasNext() {
        return iter.hasNext();
    }

    /**
     * Gets the next item in the iterator
     * 
     * @return The next item
     */
    @CoberturaIgnore
    @SuppressWarnings("unchecked")
    @Override
    public E next() {
        return (E)iter.next();
    }

    /**
     * Throws an exception if someone tries to remove items from iterator helper
     * 
     * @throws UnsupportedOperationException If this method is invoked.
     */
    @CoberturaIgnore
    @Override
    public void remove() {
        throw new UnsupportedOperationException("Iterator Helper does not allow items to be removed");
    }
}
