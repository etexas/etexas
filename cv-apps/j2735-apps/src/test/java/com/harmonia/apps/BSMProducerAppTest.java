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

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.eclipse.persistence.jaxb.JAXBContextProperties;
import org.etexascode.apps.AppLoggerImpl;
import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.OBUDevice;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.FormattedDSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.j2735.BasicSafetyMessage;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test setup for message system. Uses Playback Simulator Test Case 1. Sets configs for AppConfig
 * Creates List of AppConfigs for the device initialization Creates List of Apps on the device
 * Creates Object array with incoming messages. Creates a List<Object> for messages produced by the
 * BSM Apps
 * 
 * @author bmauldon
 */
public class BSMProducerAppTest {

    /** Create lists of Device Apps. */
    List<IConnectedVehicleApp<?>> deviceApps = new LinkedList<IConnectedVehicleApp<?>>();

    /** List of returned BSM messages. */
    List<BasicMessage> bsmList = new ArrayList<BasicMessage>();

    /** Create apps. */
    IOBUBaseApp appBSM = new BSMProducerApp();

    IConnectedVehicleApp<?> appBSMV = new BSMVerboseProducerApp();

    @Before
    public void setUp() throws Exception {
        // Add apps to list.
        deviceApps.add(appBSM);
        deviceApps.add(appBSMV);

        // Create fake vehicle and vehicle info data.
        Vehicle vehicle = new Vehicle(2, 0.0, 0.0, 20.0, 30.0, 0.0);
        vehicle.setLatitude(1);
        vehicle.setLongitude(1);
        vehicle.setHeading(0.0);
        vehicle.setSpeed(0);
        vehicle.setAcceleration(0);

        // Create the device.
        // OBUDeviceData testDevice = thc.getTestDeviceOBU(deviceApps, "2");
        OBUDeviceInfo odi = new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(0), 2);

        // Update the apps.
        String[] appConfigs = new String[] { "true", "0.3" };
        if (appBSM instanceof IAppLifecycle) {
            ((IAppLifecycle)appBSM).init(appConfigs);
        }
        OBUDevice device = new OBUDevice(odi);
        appBSM.performUpdate(device, new Object[0], new ArrayList<BasicMessage>(0), 0.5, new AppLoggerImpl(0, appBSM.getClass().getSimpleName(), 0.5));
        bsmList = device.getAppMessages();
        device.resetAppMessages();
    }

    @After
    public void tearDown() throws Exception {
        deviceApps.clear();
    }

    @Test
    public void testLifecycle() {
        BSMProducerApp app = (BSMProducerApp)appBSM;
        String[] str = new String[2];
        str[0] = "true";
        str[1] = "2.5";
        app.init(str);
        assertEquals(app.frequency, 2.5, 0.001);
        app.appShutdown(null);
    }

    @Test
    public void testGetAppId() {
        assertEquals(BSMProducerApp.APP_NAME_BSM_PRODUCER_APP, ((IAppName)appBSM).getAppName());
    }

    @Test
    public void testPerformUpdate() {
        if (bsmList.size() > 0) {
            BasicMessage message = bsmList.get(0);
            assert message.getData() instanceof byte[];
            if (message instanceof FormattedDSRCMessage) {
                assert ((FormattedDSRCMessage)message).getFormattedData() instanceof BasicSafetyMessage;
            }
        }
    }

    @Test
    public void testWriteJson() throws JAXBException {

        JAXBContext jc = org.eclipse.persistence.jaxb.JAXBContextFactory
                .createContext(new Class[] { BasicSafetyMessage.class }, null);
        Marshaller marsh = jc.createMarshaller();
        marsh.setProperty(JAXBContextProperties.MEDIA_TYPE, "application/json");
        Object formattedData = ((FormattedDSRCMessage)bsmList.get(0)).getFormattedData();
        BasicSafetyMessage bsm = (BasicSafetyMessage)formattedData;
        StringWriter writer = new StringWriter();
        marsh.marshal(bsm, writer);
        System.out.println(writer.toString());
    }
}
