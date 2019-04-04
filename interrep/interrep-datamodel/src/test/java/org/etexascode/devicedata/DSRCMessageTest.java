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
package org.etexascode.devicedata;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.devicedata.testclasses.TestBasicMessageImpl;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class DSRCMessageTest {

    private static DSRCMessage message1 = new DSRCMessage("1", DSRCChannel.CH184, 1L, 1, 10L);

    @Test
    public void testConstructor() {

        Integer data = new Integer(23);
        DSRCChannel channel = DSRCChannel.CH184;
        long peerMac = 123;
        int size = 0;
        long originMac = 0;
        DSRCMessage message = new DSRCMessage(data, channel, peerMac);

        assertTrue(data.equals(message.getData()));
        assertTrue(channel.equals(message.getChannel()));
        assertTrue(peerMac == message.getPeerMACAddress());
        assertTrue(size == message.getSize());
        assertTrue(originMac == message.getOriginMACAddress());
    }

    @Test
    public void testConstructor2() {

        Integer data = new Integer(23);
        DSRCChannel channel = DSRCChannel.CH184;
        long peerMac = 123;
        int size = 500;
        long originMac = 0;
        DSRCMessage message = new DSRCMessage(data, channel, peerMac, size);

        assertTrue(data.equals(message.getData()));
        assertTrue(channel.equals(message.getChannel()));
        assertTrue(peerMac == message.getPeerMACAddress());
        assertTrue(size == message.getSize());
        assertTrue(originMac == message.getOriginMACAddress());
    }

    @Test
    public void testConstructor3() {

        Integer data = new Integer(23);
        DSRCChannel channel = DSRCChannel.CH184;
        long peerMac = 123;
        int size = 500;
        long originMac = 432;
        DSRCMessage message = new DSRCMessage(data, channel, peerMac, size, originMac);

        assertTrue(data.equals(message.getData()));
        assertTrue(channel.equals(message.getChannel()));
        assertTrue(peerMac == message.getPeerMACAddress());
        assertTrue(size == message.getSize());
        assertTrue(originMac == message.getOriginMACAddress());
    }

    @Test
    public void testCopy() {

        assertTrue(message1.equals(message1.copy()));
    }

    @Test
    public void testEquals() {

        DSRCChannel channel = DSRCChannel.CH184;
        DSRCChannel channel2 = DSRCChannel.CH182;

        BasicMessage message2 = new DSRCMessage(message1.getData(), channel, message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress() + 10);
        BasicMessage message3 = new DSRCMessage(message1.getData(), channel, message1.getPeerMACAddress(), message1.getSize() + 10, message1.getOriginMACAddress());
        BasicMessage message4 = new DSRCMessage(message1.getData(), channel, message1.getPeerMACAddress() + 10, message1.getSize(), message1.getOriginMACAddress());
        BasicMessage message5 = new DSRCMessage(message1.getData(), channel2, message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());
        BasicMessage message6 = new DSRCMessage(new Object(), channel, message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());

        assertTrue(message1.equals(message1));
        assertFalse(message1.equals(message2));
        assertFalse(message1.equals(message3));
        assertFalse(message1.equals(message4));
        assertFalse(message1.equals(message5));
        assertFalse(message1.equals(message6));
        assertFalse(message1.equals(new Object()));
    }

    @Test
    public void testHashCode() {

        TestBasicMessageImpl basicMessage = new TestBasicMessageImpl(message1.getData(), message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());
        assertTrue(message1.hashCode() == new HashCodeBuilder(851, 143).appendSuper(basicMessage.hashCode()).append(DSRCChannel.CH184).hashCode());
    }
}
