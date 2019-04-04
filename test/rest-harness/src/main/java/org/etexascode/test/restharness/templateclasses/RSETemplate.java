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
package org.etexascode.test.restharness.templateclasses;

import java.util.List;

/**
 * Template for RSE Devices.
 * 
 * @author ablatt
 */
public class RSETemplate extends DeviceTemplate {

    /**
     * x coordinate of the RSE device.
     */
    String x;

    /**
     * y coordinate of the RSE device.
     */
    String y;

    /**
     * z coordinate of the RSE device.
     */
    String z;

    /**
     * Generic Constructor.
     */
    public RSETemplate() {
        super(null, null, null);
        x = "0";
        y = "0";
        z = "0";
    }

    /**
     * Constructor with values.
     * 
     * @param devName The name to give to the device.
     * @param jarAppIds The ids of the apps which come from jars.
     * @param remoteAppIds The ids of the apps which are remote apps.
     * @param x x coordinate of the RSE device.
     * @param y y coordinate of the RSE device.
     * @param z z coordinate of the RSE device.
     */
    public RSETemplate(String devName, List<String> jarAppIds, List<String> remoteAppIds, double x, double y, double z) {
        super(devName, jarAppIds, remoteAppIds);
        this.x = "" + x;
        this.y = "" + y;
        this.z = "" + z;
    }

    /**
     * Getter.
     * 
     * @return x coordinate of the RSE device.
     */
    public String getX() {
        return x;
    }

    /**
     * Setter.
     * 
     * @param x x coordinate of the RSE device.
     */
    public void setX(double x) {
        this.x = "" + x;
    }

    /**
     * Getter.
     * 
     * @return y coordinate of the RSE device.
     */
    public String getY() {
        return y;
    }

    /**
     * Setter.
     * 
     * @param y y coordinate of the RSE device.
     */
    public void setY(double y) {
        this.y = "" + y;
    }

    /**
     * Getter.
     * 
     * @return z coordinate of the RSE device.
     */
    public String getZ() {
        return z;
    }

    /**
     * Setter.
     * 
     * @param z z coordinate of the RSE device.
     */
    public void setZ(double z) {
        this.z = "" + z;
    }
}
