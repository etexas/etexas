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
package org.etexascode.vehiclelocationmanager.shared;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import org.etexascode.devicedata.AppInitConfig;
import org.etexascode.devicedata.CellDeviceData;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.driver.SimDriverException;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Module for initializing apps for Vehicles.
 * 
 * @author ablatt
 */
public class VehicleAppInitializer {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(VehicleAppInitializer.class);

    /**
     * The top end (exclusive) of the range for the app configurations
     */
    public final int totalRangeMax = 1000;

    /**
     * Constructor (empty because there is no reason to put anything in here)
     */
    public VehicleAppInitializer() {}

    /**
     * Initialize apps for the input Vehicles
     * 
     * @param vehs The vehicles to initialize apps for
     * @param confs The app configuration settings provided by the user
     * @param randomSeed The random seed to use when determining if a vehicle should have apps
     *        configured for it
     * @return Apps-Vehicle-MAC tuples
     * @throws InstantiationException If the instantiation exception occurs.
     * @throws IllegalAccessException If the illegal access exception occurs.
     */
    public InitializedAppData dsrcInitApps(List<IVehicle> vehs, List<AppInitConfig> confs, int randomSeed) throws InstantiationException, IllegalAccessException {
        List<OBUDeviceData> dats = new LinkedList<OBUDeviceData>();
        Random r = new Random(randomSeed);

        for (IVehicle vi : vehs) {
            int randomNum = r.nextInt(totalRangeMax);
            for (AppInitConfig aic : confs) {
                if (aic.isInRange(randomNum)) {
                    try {
                        dats.add(new OBUDeviceData(aic.deviceRuleId, initConfig(aic), vi.getProperId()));
                    }
                    catch (RuntimeException e) {
                        LOGGER.error("App Initialization Exception Detected and Caught", e);
                        throw new SimDriverException("Error initialing vehicle apps", "Contact your System Administrator.");

                    }
                }
            }
        }

        return new InitializedAppData(dats, r.nextInt());
    }

    /**
     * Initialize apps for the input Vehicles
     * 
     * @param vehs The vehicles to initialize apps for
     * @param confs The app configuration settings provided by the user
     * @param randomSeed The random seed to use when determining if a vehicle should have apps
     *        configured for it
     * @return Apps-Vehicle-MAC tuples
     * @throws InstantiationException If the instantiation exception occurs.
     * @throws IllegalAccessException If the illegal access exception occurs.
     */
    public List<CellDeviceData> cellInitApps(List<IVehicle> vehs, List<AppInitConfig> confs, int randomSeed) throws InstantiationException, IllegalAccessException {
        List<CellDeviceData> dats = new LinkedList<CellDeviceData>();
        Random r = new Random(randomSeed);

        for (IVehicle vi : vehs) {
            int randomNum = r.nextInt(totalRangeMax);
            for (AppInitConfig aic : confs) {
                if (aic.isInRange(randomNum)) {
                    if (aic.maxNumDevices > 0) {
                        try {
                            int numDevices = r.nextInt(aic.maxNumDevices - aic.minNumDevices + 1);
                            numDevices += aic.minNumDevices;

                            for (int i = 0; i < numDevices; i++) {
                                dats.add(new CellDeviceData(aic.deviceRuleId, initConfig(aic), vi.getProperId(), Integer.toString(i)));
                            }
                        }
                        catch (RuntimeException e) {
                            LOGGER.error("App Initialization Exception Detected and Caught", e);
                            throw new SimDriverException("Error initialing vehicle apps", "Contact your System Administrator.");

                        }
                    }
                }
            }
        }

        return dats;
    }

    /**
     * Initializes the apps within a configuration
     * 
     * @param aic The app configuration to initialize
     * @return The list of configured apps
     * @throws InstantiationException
     * @throws IllegalAccessException
     */
    List<IConnectedVehicleApp<?>> initConfig(AppInitConfig aic) throws InstantiationException, IllegalAccessException {
        List<IConnectedVehicleApp<?>> apps = new ArrayList<IConnectedVehicleApp<?>>(aic.appDefs.size());
        for (int index = 0; index < aic.appDefs.size(); index++) {
            apps.add(initApp(aic, index));
        }
        return apps;
    }

    /**
     * Initializes a specific app
     * 
     * @param aic The app configuration
     * @param index The index to initialize
     * @return The initialized app
     * @throws InstantiationException
     * @throws IllegalAccessException
     */
    IConnectedVehicleApp<?> initApp(AppInitConfig aic, int index) throws InstantiationException, IllegalAccessException {
        @SuppressWarnings("rawtypes")
        Class c = aic.appDefs.get(index);
        IConnectedVehicleApp<?> a = (IConnectedVehicleApp<?>)c.newInstance();
        if (a instanceof IAppLifecycle) {
            ((IAppLifecycle)a).init(aic.configs.get(index));
        }
        return a;
    }
}
