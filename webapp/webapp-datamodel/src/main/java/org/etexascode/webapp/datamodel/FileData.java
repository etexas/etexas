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
package org.etexascode.webapp.datamodel;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * Bytes of file data.
 * 
 * @author bbadillo
 * @author emyers
 */
@Entity
@Table(name = "files")
public class FileData extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The data for this file. */
    @Column(length = 65000000)
    private byte[] data;

    /**
     * Returns the data for this file.
     * 
     * @return The bytes of data for this file.
     */
    public byte[] getData() {

        return ((data == null) ? null : data.clone());
    }

    /**
     * Sets the data for this file.
     * 
     * @param data The bytes of data to set.
     */
    public void setData(byte[] data) {

        this.data = ((data == null) ? null : data.clone());
    }
}