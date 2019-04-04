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

import java.util.ArrayList;

import org.junit.Test;

/**
 * @author ttevendale
 */
public class FixedCellDeviceDataTest {

    private static FixedCellDeviceData fcdd = new FixedCellDeviceData(0, new ArrayList<IConnectedVehicleApp<?>>(), 25, 2.5, 12.5, 50.5);

    @Test
    public void testEqualsId() {
        FixedCellDeviceData fcdd2 = new FixedCellDeviceData(0, new ArrayList<IConnectedVehicleApp<?>>(), fcdd.mac, fcdd.x, fcdd.y, fcdd.z);
        assertTrue(fcdd.equalsId(fcdd2));

        FixedCellDeviceData fcdd3 = new FixedCellDeviceData(0, new ArrayList<IConnectedVehicleApp<?>>(), fcdd.mac + 34, fcdd.x, fcdd.y, fcdd.z);
        assertFalse(fcdd.equalsId(fcdd3));
    }

    @Test
    public void testProperId() {
        String correctProperId = String.format("FixedCellDeviceData:%d", fcdd.mac);
        assertTrue(correctProperId.equals(fcdd.getProperId()));
    }
}
