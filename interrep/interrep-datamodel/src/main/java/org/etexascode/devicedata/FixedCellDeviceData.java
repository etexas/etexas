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

import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * A class containing the information for fixed cellular devices.
 * 
 * @author ttevendale
 */
public class FixedCellDeviceData extends AbstractStandaloneDeviceData {

    /**
     * the mac address of this device. Must be unique
     */
    public final long mac;

    /**
     * Constructor for this tuple
     * 
     * @param deviceRuleId The device rule ID
     * @param apps The apps for this device
     * @param mac The mac address of this device
     * @param x The x cooridnate of this device
     * @param y The y cooridnate of this device
     * @param z The z cooridnate of this device
     */
    public FixedCellDeviceData(long deviceRuleId, List<IConnectedVehicleApp<?>> apps, long mac, double x, double y, double z) {
        super(deviceRuleId, apps, x, y, z);
        this.mac = mac;
    }

    /**
     * Determines equality, mainly for testing purposes
     * 
     * @return True/False the id matches
     */
    @Override
    public boolean equalsId(IDable entity) {
        return getProperId().equals(entity.getProperId());
    }

    /**
     * Gets the proper id for the fixed cell device
     * 
     * @return mac The proper id for the cell device
     */
    @Override
    public String getProperId() {
        return String.format("FixedCellDeviceData:%d", mac);
    }
}
