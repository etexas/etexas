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

import org.etexascode.appslayerdata.CellularDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.CellMessage;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class CellularDeviceTest {

    CellularDeviceInfo cellDeviceInfo;

    IDistanceable location;

    List<BasicMessage> messages;

    BasicMessage cellMessage;

    long deviceInfoMac;

    @Before
    public void setUp() throws Exception {

        location = new DistanceImpl(1, 1, 1);
        messages = new ArrayList<BasicMessage>();
        cellMessage = new CellMessage("message", 321);
        messages.add(cellMessage);
        deviceInfoMac = 123;
        cellDeviceInfo = new CellularDeviceInfo(location, messages, deviceInfoMac);
    }

    @After
    public void tearDown() throws Exception {

        location = null;
        messages = null;
        cellMessage = null;
        cellDeviceInfo = null;
    }

    @Test
    public void testGetMac() {

        CellularDevice cellDevice = new CellularDevice(cellDeviceInfo);
        assertTrue(cellDevice.getMac() == deviceInfoMac);
    }

    @Test
    public void testGetLocation() {

        CellularDevice cellDevice = new CellularDevice(cellDeviceInfo);
        assertTrue(cellDevice.getLocation().equals(location));
    }

    @Test
    public void testAddAppMessages() {

        CellularDevice cellDevice = new CellularDevice(cellDeviceInfo);
        cellDevice.addAppMessages(messages);
        List<BasicMessage> appMessages = cellDevice.getAppMessages();

        assertTrue(appMessages.size() == 1);
        assertTrue(appMessages.get(0).equals(cellMessage));
    }

    @Test
    public void testResetAppMessages() {

        CellularDevice cellDevice = new CellularDevice(cellDeviceInfo);
        cellDevice.addAppMessages(messages);

        assertTrue(cellDevice.getAppMessages().size() == 1);

        cellDevice.resetAppMessages();

        assertTrue(cellDevice.getAppMessages().size() == 0);
    }
}