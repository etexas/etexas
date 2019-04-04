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
import org.junit.Test;

/**
 * @author ttevendale
 */
public class FormattedCellMessageTest {

    private static FormattedCellMessage message1 = new FormattedCellMessage("1", "formatted1", 1L, 1, 10L);

    @Test
    public void testConstructor() {

        Integer data = new Integer(23);
        String formattedData = "formatted2";
        long peerMac = 123;
        int size = 1;
        long originMac = 0;
        FormattedCellMessage message = new FormattedCellMessage(data, formattedData, peerMac, size);

        assertTrue(data.equals(message.getData()));
        assertTrue(formattedData.equals(message.getFormattedData()));
        assertTrue(peerMac == message.getPeerMACAddress());
        assertTrue(size == message.getSize());
        assertTrue(originMac == message.getOriginMACAddress());
    }

    @Test
    public void testConstructor2() {

        Integer data = new Integer(23);
        String formattedData = "formatted2";
        long peerMac = 123;
        int size = 500;
        long originMac = 0;
        FormattedCellMessage message = new FormattedCellMessage(data, formattedData, peerMac, size, originMac);

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

        BasicMessage message2 = new FormattedCellMessage(message1.getData(), message1.getFormattedData(), message1.getPeerMACAddress(), message1.getSize(),
                message1.getOriginMACAddress() + 10);
        BasicMessage message3 = new FormattedCellMessage(message1.getData(), message1.getFormattedData(), message1.getPeerMACAddress(), message1.getSize() + 10,
                message1.getOriginMACAddress());
        BasicMessage message4 = new FormattedCellMessage(message1.getData(), message1.getFormattedData(), message1.getPeerMACAddress() + 10, message1.getSize(),
                message1.getOriginMACAddress());
        BasicMessage message5 = new FormattedCellMessage(message1.getData(), new Object(), message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());
        BasicMessage message6 = new FormattedCellMessage(new Object(), message1.getFormattedData(), message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());

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

        CellMessage cellMessage = new CellMessage(message1.getData(), message1.getPeerMACAddress(), message1.getSize(), message1.getOriginMACAddress());
        assertTrue(message1.hashCode() == new HashCodeBuilder(79, 139).appendSuper(cellMessage.hashCode()).append("formatted1").hashCode());
    }
}
