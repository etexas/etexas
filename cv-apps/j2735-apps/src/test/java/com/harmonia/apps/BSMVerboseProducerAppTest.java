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
package com.harmonia.apps;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.etexascode.apps.AppLoggerImpl;
import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.OBUDevice;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Test setup for message system. Uses Playback Simulator Test Case 1. Sets configs for AppConfig
 * Creates List of AppConfigs for the device initialization Creates List of Apps on the device
 * Creates Object array with incoming messages. Creates a List<Object> for messages produced by the
 * BSM Apps
 * 
 * @author bmauldon
 */
public class BSMVerboseProducerAppTest {

    /** Static logger */
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(BSMVerboseProducerAppTest.class);

    /** Create lists of Device Apps. */
    List<IConnectedVehicleApp<? extends OBUDevice>> deviceApps = new LinkedList<IConnectedVehicleApp<? extends OBUDevice>>();

    /** List of returned BSM messages. */
    List<BasicMessage> bsmList = new ArrayList<BasicMessage>();

    /** Create apps. */
    IConnectedVehicleApp<? extends OBUDevice> appBSM = new BSMProducerApp();

    IOBUBaseApp appBSMV = new BSMVerboseProducerApp();

    /** Holds the device app being tested. */
    IConnectedVehicleApp<?> testDeviceApp = null;

    @Before
    public void setUp() throws Exception {
        // Add apps to list.
        String[] appConfigs = new String[] { "0.3" };
        if (appBSM instanceof IAppLifecycle) {
            ((IAppLifecycle)appBSMV).init(appConfigs);
        }
        if (appBSMV instanceof IAppLifecycle) {
            ((IAppLifecycle)appBSMV).init(appConfigs);
        }
        deviceApps.add(appBSM);
        deviceApps.add(appBSMV);

        // Create fake vehicle and vehicle info data.
        Vehicle vehicle = new Vehicle(2, 0, 0, 20, 30, 0);
        vehicle.setLatitude(1);
        vehicle.setLongitude(1);
        vehicle.setHeading(0.0);
        vehicle.setSpeed(0.0);
        vehicle.setAcceleration(0.0);
        // Create the device.
        // OBUDeviceData testDevice = thc.getTestDeviceOBU(deviceApps, 2);
        OBUDeviceInfo odi = new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(0), 2);

        testDeviceApp = appBSMV;

        // Update the apps.
        OBUDevice device = new OBUDevice(odi);
        appBSMV.performUpdate(device, new Object[0], new ArrayList<BasicMessage>(0), 0.5, new AppLoggerImpl(0, appBSM.getClass().getSimpleName(), 0.5));
        bsmList = device.getAppMessages();
    }

    @After
    public void tearDown() throws Exception {
        deviceApps.clear();
    }

    @Test
    public void testLifecycle() {
        BSMVerboseProducerApp app = (BSMVerboseProducerApp)appBSMV;
        String[] str = new String[1];
        str[0] = "2.5";
        app.init(str);
        assertEquals(app.frequency, 2.5, 0.001);
        app.appShutdown(null);
    }

    @Test
    public void testPerformUpdate() {
        assertTrue(bsmList.get(0).getData() instanceof BasicSafetyMessageVerbose);

    }

    @Test
    public void testGetAppId() {
        assertEquals(BSMVerboseProducerApp.APP_NAME_BSM_VERBOSE_PRODUCER_APP, ((IAppName)appBSMV).getAppName());

    }

}
