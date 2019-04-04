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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A default device logger implementation that collects log messages from apps.
 * 
 * @author bbadillo
 */
public class DeviceLogger {

    /**
     * Logger for temporary use only.
     */
    private static final Logger LOGGER = LoggerFactory
            .getLogger(DeviceLogger.class);

    /**
     * The current time in the simulation to log for.
     */
    private Double simTime;

    /**
     * The name of the equipment to log for.
     */
    private String deviceId;

    /**
     * Constructor for the app logger.
     * 
     * @param deviceId The name of the equipment to log for.
     */
    protected DeviceLogger(String deviceId) {
        this.deviceId = deviceId;
    }

    /**
     * Get the device id for logging.
     * 
     * @return The device id for logging.
     */
    protected String getDeviceId() {
        return deviceId;
    }

    /**
     * Get the simTime for logging.
     * 
     * @return The simTime for logging.
     */
    protected Double getSimTime() {
        return simTime;
    }

    /**
     * Reset the simTime for logging.
     * 
     * @param simTime
     */
    void setTime(Double simTime) {
        this.simTime = simTime;
    }

    /**
     * Log a message using the current time and saved device id.
     * 
     * @param appId Id for the CV App logging the message.
     * @param key Key used to identify the data.
     * @param message The message to log.
     */
    public void log(String appId, String key, String message) {
        log(appId, key, message, simTime);
    }

    /**
     * Log a message using the saved device id.
     * 
     * @param appId Id for the CV App logging the message.
     * @param key Key used to identify the data.
     * @param message The message to log.
     * @param simTime A time used when logging the message.
     */
    public void log(String appId, String key, String message, Double simTime) {
        StringBuilder sb = new StringBuilder("[");
        sb.append(deviceId);
        sb.append("] [");
        sb.append(appId);
        sb.append("] [");
        sb.append(simTime);
        sb.append("] [");
        sb.append(key);
        sb.append("] ");
        sb.append(message);

        LOGGER.info(sb.toString());
    }
}
