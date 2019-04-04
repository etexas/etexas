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

import javax.persistence.MappedSuperclass;
import javax.xml.bind.annotation.XmlSeeAlso;

/**
 * A connected vehicle device with a fixed location.
 * 
 * @author bbadillo
 * @author emyers
 */
@MappedSuperclass
@XmlSeeAlso({ FixedCellularDevice.class, RseDevice.class })
public abstract class StaticDevice extends Device {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The x coordinate of this device. */
    protected double x;

    /** The y coordinate of this device. */
    protected double y;

    /** The z coordinate of this device. */
    protected double z;

    /**
     * Returns the x coordinate (cm) of this device.
     * 
     * @return The double x coordinate (cm) of this device.
     */
    public double getX() {

        return x;
    }

    /**
     * Sets the x coordinate (cm) of this device.
     * 
     * @param x The double x coordinate (cm) to set.
     */
    public void setX(double x) {

        this.x = x;
    }

    /**
     * Returns the y coordinate (cm) of this device.
     * 
     * @return The double y coordinate (cm) of this device.
     */
    public double getY() {

        return y;
    }

    /**
     * Sets the y coordinate (cm) of this device.
     * 
     * @param y The double y coordinate (cm) to set.
     */
    public void setY(double y) {

        this.y = y;
    }

    /**
     * Returns the z coordinate (cm) of this device.
     * 
     * @return The double z coordinate (cm) of this device.
     */
    public double getZ() {

        return z;
    }

    /**
     * Sets the z coordinate (cm) of this device.
     * 
     * @param z The double z coordinate (cm) to set.
     */
    public void setZ(double z) {

        this.z = z;
    }
}