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
 * Defines, for the vehicle location manager, when a set of apps should be initialized and what
 * configurations should be used by those apps.
 * 
 * @author ablatt
 */
public class AppInitConfig {

    /**
     * The devices rule ID that these apps are attached to
     */
    public final long deviceRuleId;

    /**
     * The low end of the range to be used by the vehicle location manager to determine if this app
     * set should be used.
     */
    public final int min;

    /**
     * The high end of the range to be used by the vehicle location manager to determine if this app
     * set should be used.
     */
    public final int max;

    /**
     * The low end of the range to be used by the vehicle location manager to determine how many
     * devices should be put on the car.
     */
    public final int minNumDevices;

    /**
     * The high end of the range to be used by the vehicle location manager to determine how many
     * devices should be put on the car.
     */
    public final int maxNumDevices;

    /**
     * The apps to initialize
     */
    public final List<Class<?>> appDefs;

    /**
     * The configs to use for those apps (order is maintained along with those apps)
     */
    public final List<String[]> configs;

    /**
     * Constructor
     * 
     * @param deviceRuleId The device rule ID associated with this app config
     * @param appDefs The apps belonging to this scenario
     * @param configs The configurations belonging to appDefs
     * @param min The low end of the range to be used by the vehicle location manager
     * @param max The high end of the range to be used by the vehicle location manager
     * @param minNumDevices the minimum number of devices that will contain this appinitconfig in a
     *        vehicle
     * @param maxNumDevices the maximum number of devices that will contain this appinitconfig in a
     *        vehicle
     */
    public AppInitConfig(long deviceRuleId, List<Class<?>> appDefs, List<String[]> configs, int min, int max, int minNumDevices, int maxNumDevices) {
        this.deviceRuleId = deviceRuleId;
        this.appDefs = appDefs;
        this.configs = configs;
        this.min = min;
        this.max = max;
        this.minNumDevices = minNumDevices;
        this.maxNumDevices = maxNumDevices;
    }

    /**
     * (true/false) n is between min and max (inclusive)
     * 
     * @param n The number to check against the range
     * @return (true/false) n is between min and max (inclusive)
     */
    public boolean isInRange(int n) {
        return (n >= min) && (n <= max);
    }
}
