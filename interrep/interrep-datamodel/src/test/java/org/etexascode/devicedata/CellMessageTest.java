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
public class CellMessageTest {

    private static CellMessage message1 = new CellMessage("1", 1L, 1, 10L);

    @Test
    public void testConstructor() {

        Integer data = new Integer(23);
        long peerMac = 123;
        int size = 0;
        long originMac = 0;
        CellMessage message = new CellMessage(data, peerMac);

        assertTrue(data.equals(message.getData()));
        assertTrue(peerMac == message.getPeerMACAddress());
        assertTrue(size == message.getSize());
        assertTrue(originMac == message.getOriginMACAddress());
    }

    @Test
    public void testConstructor2() {

        Integer data = new Integer(23);
        long peerMac = 123;
        int size = 500;
        long originMac = 0;
        CellMessage message = new CellMessage(data, peerMac, size);

        assertTrue(data.equals(message.getData()));
        assertTrue(peerMac == message.getPeerMACAddress());
        assertTrue(size == message.getSize());
        assertTrue(originMac == message.getOriginMACAddress());
    }

    @Test
    public void testConstructor3() {

        Integer data = new Integer(23);
        long peerMac = 123;
        int size = 500;
        long originMac = 432;
        CellMessage message = new CellMessage(data, peerMac, size, originMac);

        assertTrue(data.equals(message.getData()));
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

        BasicMessage message2 = new CellMessage(message1.getData(), message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress() + 10);
        BasicMessage message3 = new CellMessage(message1.getData(), message1.getPeerMACAddress(), message1.getSize() + 10, message1.getOriginMACAddress());
        BasicMessage message4 = new CellMessage(message1.getData(), message1.getPeerMACAddress() + 10, message1.getSize(), message1.getOriginMACAddress());
        BasicMessage message5 = new CellMessage(new Object(), message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());

        assertTrue(message1.equals(message1));
        assertFalse(message1.equals(message2));
        assertFalse(message1.equals(message3));
        assertFalse(message1.equals(message4));
        assertFalse(message1.equals(message5));
        assertFalse(message1.equals(new Object()));
    }

    @Test
    public void testHashCode() {

        TestBasicMessageImpl basicMessage = new TestBasicMessageImpl(message1.getData(), message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());
        assertTrue(message1.hashCode() == new HashCodeBuilder(11, 193).appendSuper(basicMessage.hashCode()).hashCode());
    }
}
