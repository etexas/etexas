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
package org.etexascode.appshutdownlayer;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.apps.AppLoggerImpl;
import org.etexascode.appslayerdata.AppShutdownLayerInput;
import org.etexascode.datalayer.interfaces.IAppShutdownDataLayer;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.LogData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A linear implementation of the app shutdown layer. TODO ttevendale 1/2/2018 test this class with
 * integration testing, no reason to unit test
 * 
 * @author ablatt
 * @author jrutherford
 */
public class SimpleAppShutdown implements IAppShutdown {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SimpleAppShutdown.class);

    /**
     * A reference to the data layer
     */
    IAppShutdownDataLayer data;

    /**
     * Constructor
     * 
     * @param dataLayer The data layer to be used by this shutdown layer.
     */
    public SimpleAppShutdown(IAppShutdownDataLayer dataLayer) {
        data = dataLayer;
    }

    /**
     * Shutsdown specific apps at the current time step
     * 
     * @param stepNum the time step of the execution
     */
    @Override
    public void shutdown(int stepNum) {
        data.putAppLogs(stepNum, shutdownApps(data.getAppsToShutdown(stepNum)));
    }

    /**
     * Shutsdown all the apps currently running
     */
    @Override
    public void shutdownAll() {
        data.putAppLogs(-1, shutdownApps(data.getAllAppsForShutdown()));
    }

    /**
     * Performs the actual shutdown of the apps
     * 
     * @param apps The apps to shut down
     * @return The logs produced by the apps while they were being shut down
     */
    List<LogData> shutdownApps(Iterable<AppShutdownLayerInput> apps) {
        List<LogData> ret = new ArrayList<LogData>();
        StringBuilder errorMsg = new StringBuilder();
        for (AppShutdownLayerInput asd : apps) {

            IConnectedVehicleApp ap = asd.app;
            try {
                AppLoggerImpl logger = new AppLoggerImpl(0, null, 0);
                if (ap instanceof IAppLifecycle) {

                    if (ap instanceof IAppName) {
                        logger = new AppLoggerImpl(asd.deviceId, ((IAppName)asd.app).getAppName(), -1);
                    }
                    else {
                        logger = new AppLoggerImpl(asd.deviceId, asd.app.getClass().getSimpleName(), -1);
                    }

                    ((IAppLifecycle)asd.app).appShutdown(logger);
                }

                ret.addAll(logger.getLogs());
            }
            catch (Exception e) {
                LOGGER.debug("App Shutdown Exception Detected and Caught", e);
                errorMsg.append("Error shutting down ");
                if (ap instanceof IAppName) {
                    errorMsg.append(((IAppName)asd.app).getAppName());
                }
                else {
                    errorMsg.append(asd.app.getClass().getSimpleName());
                }
                errorMsg.append(". ");
            }
        }
        if (errorMsg.toString().length() > 0) {
            throw new RuntimeException(errorMsg.toString());
        }
        return ret;
    }
}
