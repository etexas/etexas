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
package org.etexascode.webapp.datamodel.device;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.webapp.datamodel.application.Application;

/**
 * A fixed cellular device.
 * 
 * @author ttevendale
 * @author emyers
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@DiscriminatorValue(DeviceType.Discriminator.FIXED_CELLULAR)
public class FixedCellularDevice extends StaticDevice {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The MAC address for this fixed cellular device. */
    @Column(name = "mac_address")
    private long macAddress;

    /**
     * Returns the MAC address for this fixed cellular device.
     * 
     * @return The long MAC address for this fixed cellular device.
     */
    public long getMacAddress() {

        return macAddress;
    }

    /**
     * Sets the MAC address for this fixed cellular device.
     * 
     * @param macAddress The long MAC address to set.
     */
    public void setMacAddress(long macAddress) {

        this.macAddress = macAddress;
    }

    @Override
    public boolean canHost(Application application) {

        return application.getDeviceType() == DeviceType.CELLULAR;
    }

    @Override
    protected Device specificCopy() {

        FixedCellularDevice fixedCellularDevice = new FixedCellularDevice();
        fixedCellularDevice.setMacAddress(this.getMacAddress());
        fixedCellularDevice.setX(this.getX());
        fixedCellularDevice.setY(this.getY());
        fixedCellularDevice.setZ(this.getZ());

        return fixedCellularDevice;
    }
}