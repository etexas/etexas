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

package org.etexascode.interrep.datamodel;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class VehicleInjectionCommandTest {

    VehicleInjectionRequest vir;

    Vehicle v;

    @Before
    public void setUp() throws Exception {
        v = new Vehicle();
        v.setVehicleID(5);
        v.setLaneID(1);
        v.setSpeed(15);
        vir = new VehicleInjectionRequest(v, 10, 0);
    }

    @After
    public void tearDown() throws Exception {
        v = null;
        vir = null;
    }

    @Test
    public void testEquals() {
        VehicleInjectionCommand vic = new VehicleInjectionCommand(vir);
        VehicleInjectionCommand vic2 = new VehicleInjectionCommand(vir);
        assertTrue(vic.equals(vic));
        assertFalse(vic.equals(null));
        assertFalse(vic.equals(new String()));
        vic2.setInjectionTime(vic.getInjectionTime() + 1);
        assertFalse(vic.equals(vic2));
        vic2.setInjectionTime(vic.getInjectionTime());
        vic2.setLaneID(vic.getLaneID() + 1);
        assertFalse(vic.equals(vic2));
        vic2.setLaneID(vic.getLaneID());
        vic2.setSpeed(vic2.getSpeed() + 1);
        assertFalse(vic.equals(vic2));
        vic2.setSpeed(vic.getSpeed());
        vic2.setVehicleID(vic.getVehicleID() + 1);
        assertFalse(vic.equals(vic2));
    }

    @Test
    public void testToString() {
        VehicleInjectionCommand vic = new VehicleInjectionCommand(vir);
        String testStr = "VehicleInjectionCommand [injectionTime=10.0, vehicleID=5, laneID=1, speed=15.0]";
        assertTrue(testStr.equals(vic.toString()));
    }

}
