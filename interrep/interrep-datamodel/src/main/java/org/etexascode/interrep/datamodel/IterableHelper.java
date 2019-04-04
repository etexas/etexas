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
 * Iterates through lists of objects
 * 
 * @author ablatt
 * @param <E> The list to iterate through
 */

public class IterableHelper<E> implements Iterable<E> {

    /** Container for the iteratable object */
    private final Iterator<E> iter;

    /**
     * Helper construct
     * 
     * @param iter The item to iterate through
     */
    public IterableHelper(Iterator<E> iter) {
        this.iter = iter;
    }

    /**
     * Special iterator to parse through items
     */
    @CoberturaIgnore
    @Override
    public Iterator<E> iterator() {
        return iter;
    }
}
