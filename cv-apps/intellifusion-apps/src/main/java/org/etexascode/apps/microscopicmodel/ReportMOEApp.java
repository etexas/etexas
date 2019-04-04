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
package org.etexascode.apps.microscopicmodel;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.apps.IReportBaseApp;
import org.etexascode.apps.ReportDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReportMOEApp implements IReportBaseApp, IAppLifecycle {

    @AppConfigProperty(value = "20000", description = "The distance from the intersection to start tracking travel time.")
    double lengthFromIntersection;

    /**
     * Static logger
     */
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(ReportMOEApp.class);

    Map<Integer, MicroscopicModelDriver> driverMap = new HashMap<Integer, MicroscopicModelDriver>();

    @Override
    public void performUpdate(ReportDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        for (Entry<Integer, IVehicleManager> vehicleManagerEntry : device.getVehicleManagers().entrySet()) {

            MicroscopicModelDriver driver = driverMap.get(vehicleManagerEntry.getKey());

            if (driver == null) {

                driver = new MicroscopicModelDriver();
                driver.ttmoe.setLengthFromIntersection(lengthFromIntersection);
                driverMap.put(vehicleManagerEntry.getKey(), driver);
            }

            int intersection = vehicleManagerEntry.getKey();
            driver.update(vehicleManagerEntry.getValue(), device.getLaneManager(intersection), device.getSignalManager(intersection), simTime, logger);
        }
    }

    @Override
    public void init(String[] appConfigs) {

        lengthFromIntersection = Double.parseDouble(appConfigs[0]);

    }

    @Override
    public void appShutdown(AppLogger logger) {}

}
