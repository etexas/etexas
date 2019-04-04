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
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jconnelly
 */
public class AbstractDeviceInfoTest {

    List<BasicMessage> messages = null;

    long deviceId = 0;

    AbstractDeviceInfo adi = null;

    OBUDeviceInfo devinfo = null;

    @Before
    public void setup() {
        messages = messageList();
        deviceId = 9876;
        devinfo = new OBUDeviceInfo(null, messages, deviceId);
        adi = devinfo;
    }

    @After
    public void teardown() {
        messages = null;
        deviceId = 0;
        devinfo = null;
        adi = null;
    }

    @Test
    public void testConstructor() {
        assertEquals(adi, devinfo);
    }

    @Test
    public void testGetDeviceId() {
        assertEquals(adi.getDeviceId(), deviceId);
    }

    @Test
    public void testGetMessages() {
        int i = 0;
        for (BasicMessage message : adi.getMessages()) {
            assertEquals(message, messages.get(i));
            i++;
        }
    }

    private List<BasicMessage> messageList() {
        List<BasicMessage> messageList = new ArrayList<BasicMessage>(2);
        BasicMessage message1 = new DSRCMessage(47, DSRCChannel.CH184, 4321);
        BasicMessage message2 = new DSRCMessage(53, DSRCChannel.CH184, 1234);
        messageList.add(0, message1);
        messageList.add(1, message2);
        return messageList;
    }

}
