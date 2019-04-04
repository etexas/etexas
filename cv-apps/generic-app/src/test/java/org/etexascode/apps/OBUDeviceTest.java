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

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class OBUDeviceTest {

    OBUDeviceInfo obuDeviceInfo;

    List<BasicMessage> messages;

    BasicMessage obuMessage;

    long deviceInfoMac;

    Vehicle vehicle;

    @Before
    public void setUp() throws Exception {

        messages = new ArrayList<BasicMessage>();
        obuMessage = new DSRCMessage("message", DSRCChannel.CH184, 321);
        messages.add(obuMessage);
        deviceInfoMac = 123;
        vehicle = new Vehicle(1, 500, 200, 0, 0, 0);
        obuDeviceInfo = new OBUDeviceInfo(vehicle, messages, deviceInfoMac);
    }

    @After
    public void tearDown() throws Exception {

        messages = null;
        obuMessage = null;
        obuDeviceInfo = null;
        vehicle = null;
    }

    @Test
    public void testGetVehicleInfo() {

        OBUDevice obuDevice = new OBUDevice(obuDeviceInfo);
        assertTrue(obuDevice.getVehicleInfo().equals(vehicle));
    }

    @Test
    public void testAddVehicleCommmand() {

        VehicleSpeedCommand vehicleCommand = new VehicleSpeedCommand(vehicle.getVehicleID(), VehicleSpeedCommand.NORMAL_ACCELERATE_TO_XX, 20);
        OBUDevice obuDevice = new OBUDevice(obuDeviceInfo);
        obuDevice.addVehicleCommand(vehicleCommand);
        assertTrue(obuDevice.getOutputCommands().get(0).equals(vehicleCommand));
    }

    @Test
    public void testGetMac() {

        OBUDevice obuDevice = new OBUDevice(obuDeviceInfo);
        assertTrue(obuDevice.getMac() == deviceInfoMac);
    }

    @Test
    public void testAddAppMessages() {

        OBUDevice obuDevice = new OBUDevice(obuDeviceInfo);
        obuDevice.addAppMessages(messages);
        List<BasicMessage> appMessages = obuDevice.getAppMessages();

        assertTrue(appMessages.size() == 1);
        assertTrue(appMessages.get(0).equals(obuMessage));
    }

    @Test
    public void testResetAppMessages() {

        OBUDevice obuDevice = new OBUDevice(obuDeviceInfo);
        obuDevice.addAppMessages(messages);

        assertTrue(obuDevice.getAppMessages().size() == 1);

        obuDevice.resetAppMessages();

        assertTrue(obuDevice.getAppMessages().size() == 0);
    }
}