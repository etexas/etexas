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
public class WaveShortMessageSAPImplTest {

    private static WaveShortMessageSAPImpl wsm;

    @Before
    public void setup() {
        wsm = new WaveShortMessageSAPImpl();
    }

    @Test
    public void testIsBroadcastMac() {
        byte[] mac1 = new byte[] { 1, 2, 3, 4, 5, 6 };
        byte[] mac2 = new byte[] { 1, 2, 3, 4, 5 };
        byte[] mac3 = new byte[] { (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF };

        assertTrue(WaveShortMessageSAPImpl.isBroadcastMac(mac3));
        assertFalse(WaveShortMessageSAPImpl.isBroadcastMac(mac1));
        assertFalse(WaveShortMessageSAPImpl.isBroadcastMac(mac2));
    }

    @Test
    public void testIsValidMac() {
        assertTrue(WaveShortMessageSAPImpl.isValidMac(17));
        assertFalse(WaveShortMessageSAPImpl.isValidMac(-1));
        assertTrue(WaveShortMessageSAPImpl.isValidMac(0));
        assertTrue(WaveShortMessageSAPImpl.isValidMac(WaveShortMessageSAP.BROADCAST_MAC));
        assertFalse(WaveShortMessageSAPImpl.isValidMac(WaveShortMessageSAP.BROADCAST_MAC + 1));
    }

    @Test
    public void testSend() {
        WSMRequest req = new WSMRequest();
        wsm.send(req);
        assertTrue(wsm.getPendingRequests().contains(req));
    }

    @Test
    public void testReceive() {
        BasicMessage message = new DSRCMessage(0, DSRCChannel.CH184, 0);
        wsm.getPendingIndications().add(message);
        assertTrue(wsm.receive().length == 1);
    }
}
