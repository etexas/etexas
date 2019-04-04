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
package org.etexascode.devicedata;

import java.util.List;

/**
 * Abstract Class underlying all devices
 *
 * @author ablatt
 * @author ttevendale
 */
public abstract class AbstractDeviceData implements IDeviceData {

    /**
     * The device rule ID
     */
    public final long deviceRuleId;

    /**
     * The device mac address
     */
    private long macAddress;

    /**
     * The apps on this device
     */
    public final List<IConnectedVehicleApp<?>> apps;

    /**
     * Constructor
     *
     * @param deviceRuleId The device rule ID.
     * @param apps The apps for this device
     */
    public AbstractDeviceData(long deviceRuleId, List<IConnectedVehicleApp<?>> apps) {
        this.deviceRuleId = deviceRuleId;
        this.apps = apps;
    }

    /**
     * Gets the list of apps connected to this device
     * 
     * @return apps The list of connected apps
     */
    @Override
    public List<IConnectedVehicleApp<?>> getApps() {
        return apps;
    }

    /**
     * Gets the device rule ID for this device
     * 
     * @return the device rule ID
     */
    @Override
    public long getDeviceRuleId() {
        return this.deviceRuleId;
    }

    /**
     * Gets the mac address for this device
     * 
     * @return the mac address
     */
    @Override
    public long getMacAddress() {
        return this.macAddress;
    }

    /**
     * Sets the mac address for this device
     * 
     * @param macAddress The new mac address
     */
    public void setMacAddress(long macAddress) {
        this.macAddress = macAddress;
    }
}
