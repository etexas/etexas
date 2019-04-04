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
package org.etexascode.apps.hybrid.defaultimplementation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.test.GenLaneFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SimpleNotDSRCVehiclesTest {

    VehicleManager prevVm = null;

    VehicleManager currVm = null;

    Vehicle v42 = null;

    Vehicle v1729 = null;

    SimpleNotDSRCVehicles sndv = null;

    LaneManager lm = null;

    List<Vehicle> vehDriveWay = null;

    List<Vehicle> vehDet = null;

    @Before
    public void setup() {
        v42 = genVeh(42);
        v42.setSpeed(0.0);
        v42.setAcceleration(0.0);
        v1729 = genVeh(1729);
        v1729.setSpeed(0.0);
        v1729.setAcceleration(0.0);
        prevVm = genPrevMan();
        currVm = genCurrMan();
        sndv = new SimpleNotDSRCVehicles();
        lm = GenLaneFunctions.genLaneManager();

        vehDet = new ArrayList<Vehicle>(1);
        vehDet.add(v42);

        vehDriveWay = new ArrayList<Vehicle>(1);
        vehDriveWay.add(v1729);
    }

    @After
    public void teardown() {
        prevVm = null;
        currVm = null;
        v42 = null;
        v1729 = null;
        sndv = null;
        lm = null;
        vehDriveWay = null;
        vehDet = null;
    }

    @Test
    public void testAlreadySeen1() {
        List<Vehicle> actual = sndv.alreadySeen(prevVm, currVm, lm, 0);
        assertEquals(1, actual.size());
        assertEquals(1729, actual.get(0).getVehicleID());
    }

    @Test
    public void testAlreadySeen2() {
        lm.getLaneById(1).setType(Lane.OUTBOUND);
        List<Vehicle> actual = sndv.alreadySeen(prevVm, currVm, lm, Integer.MAX_VALUE);
        assertEquals(0, actual.size());
    }

    @Test
    public void testGetVehiclesNotDSRC() {
        List<Vehicle> actual = sndv.getVehiclesNotDSRC(prevVm, prevVm, vehDriveWay, vehDet, lm, Integer.MAX_VALUE);
        assertEquals(2, actual.size());
        assertTrue(actual.contains(v1729));
        assertTrue(actual.contains(v42));
    }

    private VehicleManager genPrevMan() {
        VehicleManager ret = new VehicleManager();
        ret.addVehicle(v42);
        ret.addVehicle(v1729);
        return ret;
    }

    private VehicleManager genCurrMan() {
        VehicleManager ret = new VehicleManager();
        ret.addVehicle(v42);
        return ret;
    }

    private Vehicle genVeh(int id) {
        Vehicle ret = new Vehicle(id, 0.0, 0.0, 0.0, 0.0, 0.0);
        ret.setVehicleID(id);
        ret.setLaneID(1);
        return ret;
    }
}
