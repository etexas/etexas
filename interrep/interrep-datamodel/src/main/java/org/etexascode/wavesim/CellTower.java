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
package org.etexascode.wavesim;

import java.io.Serializable;

/**
 * The Cell Tower.
 *
 * @author ttevendale
 */
public class CellTower implements Serializable {

    /**
     * Serial ID.
     */
    private static final long serialVersionUID = 5691822988275229792L;

    /**
     * The provider of the cell tower (verizon, sprint, etc.)
     */
    String provider;

    /**
     * The X coordinate of the cell tower
     */
    double x;

    /**
     * The Y coordinate of the cell tower
     */
    double y;

    /**
     * The Z coordinate of the cell tower
     */
    double z;

    /**
     * Gets the provider.
     *
     * @return the provider
     */
    public String getProvider() {
        return provider;
    }

    /**
     * Sets the provider.
     *
     * @param provider the new provider
     */
    public void setProvider(String provider) {
        this.provider = provider;
    }

    /**
     * Gets the x coordinate.
     *
     * @return the x coordinate
     */
    public double getX() {
        return x;
    }

    /**
     * Sets the x coordinate.
     *
     * @param x the new x coordinate
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Gets the y coordinate.
     *
     * @return the y coordinate
     */
    public double getY() {
        return y;
    }

    /**
     * Sets the y coordinate.
     *
     * @param y the new y coordinate
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * Gets the z coordinate.
     *
     * @return the z coordinate
     */
    public double getZ() {
        return z;
    }

    /**
     * Sets the z coordinate.
     *
     * @param z the new z coordinate
     */
    public void setZ(double z) {
        this.z = z;
    }
}