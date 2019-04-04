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

import org.junit.Before;
import org.junit.Test;

/**
 * @author janway
 */
public class WSMRequestTest {

    private static WSMRequest wsr = new WSMRequest();

    @Before
    public void setup() {
        wsr = new WSMRequest();
    }

    @Test
    public void testGetChannelIdentifier() {
        wsr.setChannelIdentifier(1);
        assertTrue(wsr.getChannelIdentifier() == 1);
    }

    @Test
    public void testGetDataRate() {
        wsr.setDataRate(2);
        assertTrue(wsr.getDataRate() == 2);
    }

    @Test
    public void testGetTransmitPowerLevel() {
        wsr.setTransmitPowerLevel(3);
        assertTrue(wsr.getTransmitPowerLevel() == 3);
    }

    @Test
    public void testGetProviderServiceIdentifier() {
        byte[] id = new byte[] { 4 };
        wsr.setProviderServiceIdentifier(id);
        assertTrue(wsr.getProviderServiceIdentifier()[0] == 4);
        assertFalse(wsr.getProviderServiceIdentifier() == id);
    }

    @Test
    public void testGetProviderServiceIdentifierNull() {
        wsr.setProviderServiceIdentifier(null);
        assertTrue(wsr.getProviderServiceIdentifier() == null);
    }

    @Test
    public void testGetUserPriority() {
        wsr.setUserPriority(5);
        assertTrue(wsr.getUserPriority() == 5);
    }

    @Test
    public void testGetWsmExpiryTime() {
        wsr.setWsmExpiryTime(6);
        assertTrue(wsr.getWsmExpiryTime() == 6);
    }

    @Test
    public void testGetLength() {
        wsr.setLength(7);
        assertTrue(wsr.getLength() == 7);
    }

    @Test
    public void testGetData() {
        wsr.setData("8");
        assertTrue(wsr.getData().equals("8"));
    }

    @Test
    public void testGetPeerMACAddress() {
        wsr.setPeerMACAddress(9L);
        assertTrue(wsr.getPeerMACAddress() == 9L);
    }

    @Test
    public void testGetWsmpHeaderExtensions() {
        wsr.setWsmpHeaderExtensions((byte)10);
        assertTrue(wsr.getWsmpHeaderExtensions() == (byte)10);
    }

    @Test
    public void testGetWaveElementID() {
        wsr.setWaveElementID(11);
        assertTrue(wsr.getWaveElementID() == 11);
    }

}
