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
package org.etexascode.appslayerdata;

import java.util.Collection;
import java.util.Collections;

import javax.xml.bind.annotation.XmlElement;

import org.etexascode.devicedata.BasicMessage;

/**
 * @author ablatt
 */

public abstract class AbstractDeviceInfo implements IDeviceInfo {

    /** Serial id */
    private static final long serialVersionUID = 6212617446174044241L;

    /** The messages this device is receiving */
    @XmlElement
    private final Collection<BasicMessage> messages;

    /** The MAC address of this device */
    @XmlElement
    public final long deviceId;

    /**
     * No-arg constructor needed for JAXB because a different constructor is provided too.
     */
    public AbstractDeviceInfo() {
        this.messages = null;
        this.deviceId = 0;
    }

    /**
     * Constructor
     * 
     * @param m The messages this device is receiving
     * @param dId The MAC address of this device
     */
    public AbstractDeviceInfo(Collection<BasicMessage> m, long dId) {
        messages = Collections.unmodifiableCollection(m);
        deviceId = dId;
    }

    /**
     * Gets the device ID.
     * 
     * @return The device ID.
     */
    @Override
    public long getDeviceId() {
        return deviceId;
    }

    /**
     * Gets the list of device messages.
     * 
     * @return The list of messages for the device.
     */
    @Override
    public Collection<BasicMessage> getMessages() {
        return messages;
    }
}
