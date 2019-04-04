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
package org.etexascode.apps;

import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.LogData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The basic implementation of the app logger (used in the generic app)
 * 
 * @author ablatt
 */
public class AppLoggerImpl implements AppLogger {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(AppLoggerImpl.class);

    /**
     * All the app log data given to us by an app.
     */
    List<LogData> data = new LinkedList<LogData>();

    /**
     * The device id for use in the logs.
     */
    final long deviceId;

    /**
     * The name of the app being logged for.
     */
    final String appName;

    /**
     * The time to be used by this specific app logger.
     */
    final BigDecimal simTime;

    /**
     * Contructor
     * 
     * @param devId The id of the device (to be used in the logs)
     * @param appName The name of the app (to be used in the logs)
     * @param time The sim time (to be used in the logs)
     */
    public AppLoggerImpl(long devId, String appName, double time) {
        this.deviceId = devId;
        this.appName = appName;
        this.simTime = new BigDecimal(time);
    }

    @Override
    public void log(String key, String message) {
        data.add(new LogData(deviceId, appName, simTime, key, message));
    }

    /**
     * Getter. This is for internal use and not intended for normal users.
     * 
     * @return The log data for this app on this time step.
     */
    public List<LogData> getLogs() {
        return data;
    }
}
