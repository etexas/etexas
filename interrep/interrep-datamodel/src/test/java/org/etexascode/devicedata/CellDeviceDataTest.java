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
public class CellDeviceDataTest {

    private static CellDeviceData cdd = new CellDeviceData(0, new ArrayList<IConnectedVehicleApp<?>>(), "vehId1", "cellId1");

    @Test
    public void testEqualsAndEqualsId() {

        CellDeviceData cdd2 = new CellDeviceData(0, new ArrayList<IConnectedVehicleApp<?>>(), "vehId1", "cellId1");
        assertTrue(cdd.equals(cdd2));
        assertTrue(cdd.equalsId(cdd2));

        CellDeviceData cdd3 = new CellDeviceData(0, new ArrayList<IConnectedVehicleApp<?>>(), "vehId1", "cellId2");
        assertFalse(cdd.equals(cdd3));
        assertFalse(cdd.equalsId(cdd3));

        CellDeviceData cdd4 = new CellDeviceData(0, new ArrayList<IConnectedVehicleApp<?>>(), "vehId2", "cellId1");
        assertFalse(cdd.equals(cdd4));
        assertFalse(cdd.equalsId(cdd4));

        assertFalse(cdd.equals(new String()));
    }

    @Test
    public void testProperId() {
        String correctProperId = String.format("%s %s", cdd.vehicleId, cdd.cellId);
        assertTrue(correctProperId.equals(cdd.getProperId()));
    }
}
