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
 * Template for configuring devices.
 * 
 * @author ablatt
 */
public class DeviceTemplate {

    /**
     * The name to give to the device.
     */
    String deviceName;

    /**
     * The ids of the apps which come from jars.
     */
    List<String> jarAppIds; // assumes that the id presented here will match up with the id of an
                            // app in a jar uploaded as part of this template

    /**
     * The ids of the apps which are remote apps.
     */
    List<String> remoteAppIds;

    /**
     * Constructor.
     * 
     * @param devName The name to give to the device.
     * @param jarAppIds The ids of the apps which come from jars.
     * @param remoteAppIds The ids of the apps which are remote apps.
     */
    public DeviceTemplate(String devName, List<String> jarAppIds, List<String> remoteAppIds) {
        deviceName = devName;
        this.jarAppIds = jarAppIds;
        this.remoteAppIds = remoteAppIds;
    }

    /**
     * Getter.
     * 
     * @return The name to give to the device.
     */
    public String getDeviceName() {
        return deviceName;
    }

    /**
     * Setter.
     * 
     * @param deviceName The name to give to the device.
     */
    public void setDeviceName(String deviceName) {
        this.deviceName = deviceName;
    }

    /**
     * Getter.
     * 
     * @return The ids of the apps which come from jars.
     */
    public List<String> getJarAppIds() {
        return jarAppIds;
    }

    /**
     * Setter.
     * 
     * @param jarAppIds The ids of the apps which come from jars.
     */
    public void setJarAppIds(List<String> jarAppIds) {
        this.jarAppIds = jarAppIds;
    }

    /**
     * Getter.
     * 
     * @return The ids of the apps which are remote apps.
     */
    public List<String> getRemoteAppIds() {
        return remoteAppIds;
    }

    /**
     * Setter.
     * 
     * @param remoteAppIds The ids of the apps which are remote apps.
     */
    public void setRemoteAppIds(List<String> remoteAppIds) {
        this.remoteAppIds = remoteAppIds;
    }
}
