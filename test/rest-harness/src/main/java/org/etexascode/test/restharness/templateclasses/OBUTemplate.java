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
 * Template for OBU devices.
 * 
 * @author ablatt
 */
public class OBUTemplate extends DeviceTemplate {

    /**
     * Percentage of vehicles which have this obu configuation.
     */
    String percentage;

    /**
     * @param devName The name to give to the device.
     * @param jarAppIds The ids of the apps which come from jars.
     * @param remoteAppIds The ids of the apps which are remote apps.
     * @param percentage Percentage of vehicles which have this obu configuation.
     */
    public OBUTemplate(String devName, List<String> jarAppIds, List<String> remoteAppIds, String percentage) {
        super(devName, jarAppIds, remoteAppIds);
        this.percentage = percentage;
    }

    /**
     * Getter.
     * 
     * @return Percentage of vehicles which have this obu configuation.
     */
    public String getPercentage() {
        return percentage;
    }

    /**
     * Setter.
     * 
     * @param percentage Percentage of vehicles which have this obu configuation.
     */
    public void setPercentage(String percentage) {
        this.percentage = percentage;
    }
}