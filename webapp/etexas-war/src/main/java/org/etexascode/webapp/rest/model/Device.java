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

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The device model.
 * 
 * @author ttevendale
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public abstract class Device implements Serializable {

    /** Serial ID */
    @XmlTransient
    private static final long serialVersionUID = -724634139075209326L;

    @XmlElement
    private final long deviceRuleId;

    @XmlElement
    private final long deviceMac;

    /**
     * enum Interface for deviceTypes
     * 
     * @author ttevendale
     */
    protected interface DeviceType {};

    @XmlElement
    private final DeviceType deviceType;

    /**
     * Constructor for a device.
     * 
     * @param deviceRuleId The device rule ID.
     * @param deviceMac The mac address of this device.
     * @param deviceType The type of device this is (OBU, RSE, etc...).
     */
    public Device(long deviceRuleId, long deviceMac, DeviceType deviceType) {
        this.deviceRuleId = deviceRuleId;
        this.deviceMac = deviceMac;
        this.deviceType = deviceType;
    }

    /**
     * Standard equality operator for the device.
     * 
     * @param obj The device to check this device against.
     * @return whether or not this is the same device.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj != null && this.getClass().equals(obj.getClass())) {
            Device d = (Device)obj;
            boolean b = this.deviceMac == d.getMacAddress();
            b &= this.deviceRuleId == d.getDeviceRuleId();
            b &= this.deviceType.equals(d.getDeviceType());

            return b;
        }
        else {
            return false;
        }
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @Override
    public int hashCode() {
        // TODO - ttevendale - This is wrong since is doesn't match the equals, maps and other
        // collections may behave oddly
        return new HashCodeBuilder(63, 17).append(deviceMac).toHashCode();
    }

    /**
     * Gets the device mac address.
     * 
     * @return device mac address.
     */
    public long getMacAddress() {
        return deviceMac;
    }

    /**
     * Gets the type of this device.
     * 
     * @return the device type.
     */
    public DeviceType getDeviceType() {
        return deviceType;
    }

    /**
     * Gets the device rule ID.
     * 
     * @return the device rule ID.
     */
    public long getDeviceRuleId() {
        return deviceRuleId;
    }
}