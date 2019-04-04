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
package org.etexascode.interrep.datamodel;

import org.etexascode.interrep.datamodel.interfaces.IDistanceable;

/**
 * Simple implementation of the IDistanceable interface.
 *
 * @author ablatt
 */
public class DistanceImpl implements IDistanceable {

    public final double x;

    public final double y;

    public final double z;

    /**
     * No-arg constructor needed for JAXB because a different constructor is provided too.
     */
    public DistanceImpl() {
        x = 0.0;
        y = 0.0;
        z = 0.0;
    }

    /**
     * Constructor
     *
     * @param x X value for this point
     * @param y Y value for this point
     * @param z Z value for this point
     */
    public DistanceImpl(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
     * Gets the x value for this point
     * 
     * @return x The x value
     */
    @Override
    public double getX() {
        return x;
    }

    /**
     * Gets the y value for this point
     * 
     * @return y The y value
     */
    @Override
    public double getY() {
        return y;
    }

    /**
     * Gets the z value for this point
     * 
     * @return z The z value
     */
    @Override
    public double getZ() {
        return z;
    }
}
