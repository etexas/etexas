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

package org.etexascode.webapp.datamodel.util;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * A wrapper class to convert a list of items into a JSON format.
 * 
 * @author jrutherford
 * @author emyers
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class WrappedList<T> implements Serializable {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The items for this list. */
    @XmlElementWrapper(name = "list")
    @XmlElement(name = "item")
    private List<T> items;

    /* required for JAX */
    public WrappedList() {}

    /**
     * Constructs a new <code>WrappedList</code> with the specified items.
     * 
     * @param items The items for this list.
     */
    public WrappedList(List<T> items) {

        this.items = items;
    }

    /**
     * Returns the items for this list.
     * 
     * @return The list of items for this list.
     */
    public List<T> getList() {

        return items;
    }

    /**
     * Sets the items for this list.
     * 
     * @param items The list of items to set.
     */
    public void setList(List<T> items) {

        this.items = items;
    }
}