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
package org.etexascode.webapp.rest.model;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 * The model for a standalone device.
 * 
 * @author ttevendale
 */
public class StandaloneDevice extends Device {

    /** Serial ID */
    @XmlTransient
    private static final long serialVersionUID = -1478498050074953174L;

    public enum StandaloneDeviceType implements DeviceType {
        RSE,
        FIXED_CELL
    };

    @XmlElement
    private final double x;

    @XmlElement
    private final double y;

    @XmlElement
    private final double z;

    /**
     * Constructor for a standalone device
     * 
     * @param deviceRuleId The device rule ID
     * @param deviceMac The mac address of this device
     * @param deviceType The type that this device is (RSE, etc...)
     * @param x The x coordinate of this device.
     * @param y The y coordinate of this device.
     * @param z The z coordinate of this device.
     */
    public StandaloneDevice(long deviceRuleId, long deviceMac, StandaloneDeviceType deviceType, double x, double y, double z) {
        super(deviceRuleId, deviceMac, deviceType);
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
     * Gets the x coordinate of the device.
     * 
     * @return The x coordinate.
     */
    public double getX() {
        return this.x;
    }

    /**
     * Gets the y coordinate of the device.
     * 
     * @return The y coordinate.
     */
    public double getY() {
        return this.y;
    }

    /**
     * Gets the z coordinate of the device.
     * 
     * @return The z coordinate.
     */
    public double getZ() {
        return this.z;
    }

}
