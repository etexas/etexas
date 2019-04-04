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
package org.etexascode.apps.microscopicmodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.test.TestAppLoggerReadOutput;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jconnelly
 */

public class TravelTimeMOETest {

    ILaneManager lmi = null;

    IVehicleManager vmi = null;

    Map<String, String> vsl = null;

    Map<String, Double> vs = null;

    double lfi = 0.0;

    double st = 0.0;

    double delta = 0.0;

    double stn = 0.0;

    TravelTimeMOE ttmoe = null;

    TestAppLoggerReadOutput testLogger = null;

    Map<String, Double> logVehicleStart = null;

    Map<String, String> logVehStartLane = null;

    @Before
    public void setup() {
        lmi = genLanMan();
        vmi = genVehMan();
        lfi = 550.0;
        st = 25.0;
        delta = .005;
        ttmoe = new TravelTimeMOE();
        ttmoe.setLengthFromIntersection(lfi);
        logVehicleStart = genLogVehicleStart();
        logVehStartLane = genLogVehStartLane();
        testLogger = new TestAppLoggerReadOutput();
    }

    @After
    public void teardown() {
        lmi = null;
        vmi = null;
        vsl = null;
        vs = null;
        lfi = 0.0;
        st = 0.0;
        stn = 0.0;
        delta = 0.0;
        ttmoe = null;
        logVehicleStart = null;
        logVehStartLane = null;
        testLogger = null;
    }

    @Test
    public void testConstructor() {
        TravelTimeMOE t = new TravelTimeMOE();
        assertTrue(t instanceof TravelTimeMOE);
    }

    @Test
    public void testUpdate() {
        ttmoe.update(vmi, lmi, st, testLogger);
        // ensures that the outbound lane if statement is not in the map
        assertFalse(ttmoe.vehStartLane.containsKey("Vehicle:1"));
        assertFalse(ttmoe.vehicleStart.containsKey("Vehicle:1"));
    }

    @Test
    public void testUpdate2() {
        ttmoe.update(vmi, lmi, st, testLogger);
        // ensures that the vehicle is added to the map
        assertTrue(ttmoe.vehStartLane.containsKey("Vehicle:3"));
        assertTrue(ttmoe.vehicleStart.containsKey("Vehicle:3"));
    }

    @Test
    public void testUpdate3() {
        ttmoe.update(vmi, lmi, st, testLogger);
        // verifies that the calculated time is correct
        assertEquals(0.5898, ttmoe.vehicleStart.get("Vehicle:4"), delta);
    }

    @Test
    public void testUpdate4() {
        ttmoe.update(vmi, lmi, st, testLogger);
        // ensures that vehicles outside the tracked length is not in map
        assertFalse(ttmoe.vehStartLane.containsKey("Vehicle:6"));
        assertFalse(ttmoe.vehicleStart.containsKey("Vehicle:6"));
    }

    @Test
    public void testUpdate5() {
        ttmoe.update(vmi, lmi, st, testLogger);
        // updates the vehicle manager to simulate vehicles moving from inbound
        // to outbound lanes and removing vehicles that were in last update
        vmi = genVehManTwo();
        stn = 30.0;
        // runs update with new vehicle manager information and time step
        ttmoe.update(vmi, lmi, stn, testLogger);
        // ensures that the lane is in map and lane type outbound to be set to
        // null and not in the map
        assertFalse(ttmoe.vehStartLane.containsKey("Vehicle:2"));
        assertFalse(ttmoe.vehicleStart.containsKey("Vehicle:2"));
    }

    @Test
    public void testUpdate6() {
        ttmoe.update(vmi, lmi, st, testLogger);
        // updates the vehicle manager to simulate vehicles moving from inbound
        // to outbound lanes and removing vehicles that were in last update
        vmi = genVehManTwo();
        stn = 30.0;
        // runs update with new vehicle manager information and time step
        ttmoe.update(vmi, lmi, stn, testLogger);
        // ensures that second for loop works correctly
        assertFalse(ttmoe.vehStartLane.containsKey("Vehicle:5"));
        assertFalse(ttmoe.vehicleStart.containsKey("Vehicle:5"));
    }

    @Test
    public void testGetLengthFromIntersection() {
        double actual = ttmoe.getLengthFromIntersection();
        assertEquals(lfi, actual, delta);
    }

    @Test
    public void testSetLengthFromIntersection() {

        double length = 100.0;

        ttmoe.setLengthFromIntersection(length);
        assertEquals(length, ttmoe.getLengthFromIntersection(), delta);
    }

    @Test
    public void testWriteLogs() {
        ttmoe.writeLogs("blah", "ster", 50.0, 55.0, testLogger);
        assertEquals("Travel time for blah, starting lane ster", testLogger.logs.get(0)[0]);
        assertEquals("5.0", testLogger.logs.get(0)[1]);
    }

    private ILaneManager genLanMan() {
        LaneManager lm = new LaneManager();
        lm.setLanes(laneList());
        return lm;
    }

    private Map<Integer, Lane> laneList() {
        Map<Integer, Lane> llist = new HashMap<Integer, Lane>();
        Lane l1 = new Lane();
        l1.setLaneId(1);
        l1.setType("INBOUND");
        LaneNode ln = new LaneNode();
        ln.setX(0.0);
        ln.setY(0.0);
        List<LaneNode> lst = new ArrayList<LaneNode>(1);
        lst.add(ln);
        l1.setLaneGeomList(lst);
        Lane l2 = new Lane();
        l2.setLaneId(2);
        l2.setType("OUTBOUND");
        l2.setLaneGeomList(lst);
        llist.put(1, l1);
        llist.put(2, l2);
        return llist;
    }

    private IVehicleManager genVehMan() {
        VehicleManager vm = new VehicleManager();
        vm.addVehicles(vehList());
        return vm;
    }

    private IVehicleManager genVehManTwo() {
        VehicleManager vm = new VehicleManager();
        vm.addVehicles(vehListTwo());
        return vm;
    }

    private List<Vehicle> vehList() {
        List<Vehicle> vlist = new ArrayList<Vehicle>();
        Vehicle veh1 = new Vehicle(1, 1.0, 1.0, 15.0, 15.0, 0.0);
        veh1.setSpeed(25.0);
        veh1.setLaneID(2);
        Vehicle veh2 = new Vehicle(2, 1.0, 1.0, 20.0, 20.0, 0.0);
        veh2.setSpeed(25.0);
        veh2.setLaneID(1);
        Vehicle veh3 = new Vehicle(3, 1.0, 1.0, 25.0, 25.0, 0.0);
        veh3.setSpeed(36.0);
        veh3.setLaneID(1);
        Vehicle veh4 = new Vehicle(4, 1.0, 1.0, 130.0, 130.0, 0.0);
        veh4.setSpeed(15.0);
        veh4.setLaneID(1);
        Vehicle veh5 = new Vehicle(5, 1.0, 1.0, 135.0, 135.0, 0.0);
        veh5.setSpeed(20.0);
        veh5.setLaneID(1);
        Vehicle veh6 = new Vehicle(6, 1.0, 1.0, 400.0, 400.0, 0.0);
        veh6.setSpeed(10.0);
        veh6.setLaneID(1);
        vlist.add(veh1);
        vlist.add(veh2);
        vlist.add(veh3);
        vlist.add(veh4);
        vlist.add(veh5);
        vlist.add(veh6);
        return vlist;
    }

    private List<Vehicle> vehListTwo() {
        List<Vehicle> vlist = new ArrayList<Vehicle>();
        Vehicle veh2 = new Vehicle(2, 1.0, 1.0, 20.0, 20.0, 0.0);
        veh2.setSpeed(25.0);
        veh2.setLaneID(2);
        Vehicle veh3 = new Vehicle(3, 1.0, 1.0, 25.0, 25.0, 0.0);
        veh3.setSpeed(36.0);
        veh3.setLaneID(1);
        Vehicle veh4 = new Vehicle(4, 1.0, 1.0, 130.0, 130.0, 0.0);
        veh4.setSpeed(15.0);
        veh4.setLaneID(1);
        vlist.add(veh2);
        vlist.add(veh3);
        vlist.add(veh4);
        return vlist;
    }

    private Map<String, Double> genLogVehicleStart() {
        Map<String, Double> ret = new HashMap<String, Double>();

        ret.put("Vehicle:1", 42.0);
        ret.put("Vehicle:2", 97.0);

        return ret;
    }

    private Map<String, String> genLogVehStartLane() {
        Map<String, String> ret = new HashMap<String, String>();

        ret.put("Vehicle:1", "Lane:1:0");
        ret.put("Vehicle:2", "Lane:2:0");

        return ret;
    }
}
