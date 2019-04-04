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
public class IDeviceInfoTest {

    List<BasicMessage> messageList = null;

    long id = -1;

    IDeviceInfo devInfo = null;

    @Before
    public void setup() {
        id = 9876;
        messageList = this.createMessageList();
        devInfo = produceIDeviceInfo();
    }

    @After
    public void teardown() {
        id = -1;
        messageList = null;
        devInfo = null;
    }

    @Test
    public void testGetDeviceId() {
        assertEquals(devInfo.getDeviceId(), id);
    }

    @Test
    public void testGetMessages() {
        int i = 0;
        for (BasicMessage message : devInfo.getMessages()) {
            assertEquals(message, messageList.get(i));
            i++;
        }
    }

    @Test
    public void testGetLocation() {
        assertEquals(devInfo.getLocation(), v1());
    }

    private IDeviceInfo produceIDeviceInfo() {
        IDeviceInfo devinfo = new OBUDeviceInfo(v1(), messageList, id);
        return devinfo;
    }

    private IVehicle v1() {
        Vehicle v1 = new Vehicle(1, 10.0, 10.0, 25.0, 25.0, 0.0);
        return v1;
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
