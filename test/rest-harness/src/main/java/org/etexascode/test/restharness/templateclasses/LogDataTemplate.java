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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Template for downloading logs.
 * 
 * @author ablatt
 */
public class LogDataTemplate {

    /**
     * The file to write the logs to.
     */
    File logOutputLoc;

    /**
     * The minimum sim time to use (0.0 for "do not use").
     */
    double minSimTime = 0.0;

    /**
     * The maximum sim time to use (0.0 for "do not use).
     */
    double maxSimTime = 0.0;

    /**
     * The ids of the devices to get logs for (will typically be an empty list).
     */
    List<Long> devIds = new ArrayList<Long>(1);

    /**
     * The ids of the apps to get logs for.
     */
    List<String> appIds = new ArrayList<String>(1);

    /**
     * The keys to get logs for.
     */
    List<String> keys = new ArrayList<String>();

    /**
     * Constructor.
     * 
     * @param outputFile The file to write the logs to.
     */
    public LogDataTemplate(File outputFile) {
        logOutputLoc = outputFile;
    }

    /**
     * Getter.
     * 
     * @return The file to write the logs to.
     */
    public File getLogOutputLoc() {
        return logOutputLoc;
    }

    /**
     * Setter.
     * 
     * @param logOutputLoc The file to write the logs to.
     */
    public void setLogOutputLoc(File logOutputLoc) {
        this.logOutputLoc = logOutputLoc;
    }

    /**
     * Getter.
     * 
     * @return The minimum sim time to use (0.0 for "do not use").
     */
    public double getMinSimTime() {
        return minSimTime;
    }

    /**
     * Setter.
     * 
     * @param minSimTime The minimum sim time to use (0.0 for "do not use").
     */
    public void setMinSimTime(double minSimTime) {
        this.minSimTime = minSimTime;
    }

    /**
     * Getter.
     * 
     * @return The maximum sim time to use (0.0 for "do not use).
     */
    public double getMaxSimTime() {
        return maxSimTime;
    }

    /**
     * Setter.
     * 
     * @param maxSimTime The maximum sim time to use (0.0 for "do not use).
     */
    public void setMaxSimTime(double maxSimTime) {
        this.maxSimTime = maxSimTime;
    }

    /**
     * Getter.
     * 
     * @return The ids of the devices to get logs for (will typically be an empty list).
     */
    public List<Long> getDevIds() {
        return devIds;
    }

    /**
     * Setter.
     * 
     * @param devIds The ids of the devices to get logs for (will typically be an empty list).
     */
    public void setDevIds(List<Long> devIds) {
        this.devIds = devIds;
    }

    /**
     * Getter.
     * 
     * @return The ids of the apps to get logs for.
     */
    public List<String> getAppIds() {
        return appIds;
    }

    /**
     * Setter.
     * 
     * @param appIds The ids of the apps to get logs for.
     */
    public void setAppIds(List<String> appIds) {
        this.appIds = appIds;
    }

    /**
     * Getter.
     * 
     * @return The keys to get logs for.
     */
    public List<String> getKeys() {
        return keys;
    }

    /**
     * Setter.
     * 
     * @param keys The keys to get logs for.
     */
    public void setKeys(List<String> keys) {
        this.keys = keys;
    }
}
