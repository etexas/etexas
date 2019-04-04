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
package org.etexascode.wavesim;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * @author ttevendale
 */
public class RxTest {

    @Test
    public void testConstructor() {
        long mac = 1234;
        Rx rx = new Rx(mac);
        assertTrue(mac == rx.mac);
        assertTrue(rx.messages.size() == 0);
    }

    @Test
    public void testEquals() {
        Rx rx = new Rx(1);
        Rx sameRx = new Rx(1);
        assertTrue(rx.equals(sameRx));

        Rx rx2 = new Rx(2);
        assertFalse(rx.equals(rx2));

        Rx rx3 = new Rx(1);
        rx3.messages.put("string", 23);
        assertFalse(rx.equals(rx3));

        assertFalse(rx.equals(new String()));
    }
}
