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
package org.etexascode.appslayerdata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jconnelly
 */

public class OBUDeviceInfoTest {

    IVehicle obuData = null;

    String properId = null;

    OBUDeviceInfo odi = null;

    long did = -1;

    List<BasicMessage> messageList = null;

    double delta = -1;

    @Before
    public void setup() {
        did = 9876;
        obuData = v1();
        messageList = createMessageList();
        odi = genOBUDeviceInfo();
        delta = 0.0005;
    }

    @After
    public void teardown() {
        did = -1;
        obuData = null;
        messageList = null;
        odi = null;
        delta = -1;
    }

    @Test
    public void testConstructor() {
        assertNotNull(odi);
    }

    @Test
    public void testEqualsIdTrue() {
        assertTrue(odi.equalsId(v1()));
    }

    @Test
    public void testEqualsIdFalse() {
        assertFalse(odi.equalsId(v2()));
    }

    @Test
    public void testGetProperId() {
        assertEquals(odi.getProperId(), v1().getProperId());
    }

    @Test
    public void testGetProperId2() {
        IVehicle vehicle = v1();
        OBUDeviceInfo obuDevInfo = new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(0), 25);
        assertEquals(obuDevInfo.getProperId(), vehicle.getProperId());

        // gets the cached properId
        assertEquals(obuDevInfo.getProperId(), vehicle.getProperId());
    }

    @Test
    public void testGetLocation() {
        assertEquals(odi.getLocation(), v1());
        assertEquals(odi.getLocation().getX(), v2().getX(), delta);
        assertEquals(odi.getLocation().getY(), v2().getY(), delta);
        assertEquals(odi.getLocation().getZ(), v2().getZ(), delta);
    }

    @Test
    public void testGetVehicle() {
        assertEquals(odi.getVehicle(), v1());
    }

    private OBUDeviceInfo genOBUDeviceInfo() {
        OBUDeviceInfo devInf = new OBUDeviceInfo(obuData, messageList, did);
        return devInf;
    }

    private IVehicle v1() {
        Vehicle v1 = new Vehicle(1, 10.0, 10.0, 25.0, 25.0, 0.0);
        return v1;
    }

    private IVehicle v2() {
        Vehicle v2 = new Vehicle(2, 10, 10, 25.0, 25.0, 0.0);
        return v2;
    }

    private List<BasicMessage> createMessageList() {
        List<BasicMessage> messageList = new ArrayList<BasicMessage>(2);
        BasicMessage message1 = new DSRCMessage(47, DSRCChannel.CH184, 4321);
        BasicMessage message2 = new DSRCMessage(53, DSRCChannel.CH184, 1234);
        messageList.add(0, message1);
        messageList.add(1, message2);
        return messageList;
    }

}
