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

import org.etexascode.interrep.datamodel.interfaces.IDistanceable;

/**
 * Abstract Class underlying all stand alone devices
 * 
 * @author ttevendale
 */
public abstract class AbstractStandaloneDeviceData extends AbstractDeviceData implements IDistanceable {

    /**
     * The x coordinate of this stand alone device
     */
    public final double x;

    /**
     * The y coordinate of this stand alone device
     */
    public final double y;

    /**
     * The z coordinate of this stand alone device
     */
    public final double z;

    /**
     * Constructor
     * 
     * @param deviceRuleId The device ID
     * @param apps The apps on this device
     * @param x The x coordinate of this stand alone device
     * @param y The y coordinate of this stand alone device
     * @param z The z coordinate of this stand alone device
     */
    public AbstractStandaloneDeviceData(long deviceRuleId, List<IConnectedVehicleApp<?>> apps, double x, double y, double z) {
        super(deviceRuleId, apps);
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
     * Gets the x coordinate of the stand alone device location
     * 
     * @return x The x coordinate of this stand alone device
     */
    @Override
    public double getX() {
        return x;
    }

    /**
     * Gets the y coordinate of the stand alone device location
     * 
     * @return The y coordinate of this stand alone device
     */
    @Override
    public double getY() {
        return y;
    }

    /**
     * Gets the z coordinate of the stand alone device location
     * 
     * @return The z coordinate of this stand alone device
     */
    @Override
    public double getZ() {
        return z;
    }
}
