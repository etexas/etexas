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

/**
 * A connected vehicle device profile for cellular devices.
 * 
 * @author ttevendale
 * @author emyers
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@DiscriminatorValue(DeviceType.Discriminator.CELLULAR)
public class CellularDeviceProfile extends DeviceProfile {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The minimum number of devices per affected vehicle. */
    @Column(name = "min_devices")
    private int minDevices;

    /** The maximum number of devices per affected vehicle. */
    @Column(name = "max_devices")
    private int maxDevices;

    /**
     * Returns the minimum number of devices per affected vehicle.
     * 
     * @return The integer minimum number of devices per affected vehicle.
     */
    public int getMinDevices() {

        return minDevices;
    }

    /**
     * Sets the minimum number of devices per affected vehicle.
     * 
     * @param minDevices The integer minimum number of devices per affected vehicle to set.
     */
    public void setMinDevices(int minDevices) {

        this.minDevices = minDevices;
    }

    /**
     * Returns the maximum number of devices per affected vehicle.
     * 
     * @return The integer maximum number of devices per affected vehicle.
     */
    public int getMaxDevices() {

        return maxDevices;
    }

    /**
     * Sets the maximum number of devices per affected vehicle.
     * 
     * @param maxDevices The integer maximum number of devices per affected vehicle to set.
     */
    public void setMaxDevices(int maxDevices) {

        this.maxDevices = maxDevices;
    }

    /**
     * The method to copy the specific parts of this device
     * 
     * @return The copied device
     */
    @Override
    protected CellularDeviceProfile specificCopy() {

        CellularDeviceProfile cellularDeviceProfile = new CellularDeviceProfile();
        cellularDeviceProfile.setMinDevices(this.getMinDevices());
        cellularDeviceProfile.setMaxDevices(this.getMaxDevices());
        cellularDeviceProfile.setPercentage(this.getPercentage());

        return cellularDeviceProfile;
    }
}