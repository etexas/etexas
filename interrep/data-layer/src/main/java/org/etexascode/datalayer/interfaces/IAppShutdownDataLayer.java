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
package org.etexascode.datalayer.interfaces;

import java.util.List;

import org.etexascode.appslayerdata.AppShutdownLayerInput;
import org.etexascode.devicedata.LogData;

/**
 * Data layer for shutting down apps.
 * 
 * @author ablatt
 */
public interface IAppShutdownDataLayer {

    /**
     * Add the output of the shut down apps to the data layer
     * 
     * @param stepNum The step number we are outputting the logs for
     * @param logs The logs from the apps
     */
    public void putAppLogs(int stepNum, List<LogData> logs);

    /**
     * Get the apps to shut down at a specified time step
     * 
     * @param stepNum The step number to get apps for
     * @return The apps to shut down
     */
    public Iterable<AppShutdownLayerInput> getAppsToShutdown(int stepNum);

    /**
     * Get all the apps in the data layer and shut them down
     * 
     * @return The apps to shut down
     */
    public Iterable<AppShutdownLayerInput> getAllAppsForShutdown();
}
