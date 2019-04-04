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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;

/**
 * @author ablatt
 */
public class SuperSimpleCarFollowingCalculatorTest {

    SuperSimpleCarFollowingCalculator sscfc = null;

    DistanceImpl id = null;

    ILane li = null;

    InterRepInfoModel irim = null;

    Map<Integer, List<Vehicle>> vehicles = null;

    List<IVehicle> vehList = null;

    @Before
    public void setup() {
        id = new DistanceImpl(1729.0, 1930.0, 0.0);
        li = getLaneForDist();

        LaneManager laneMan = new LaneManager();
        ((Lane)li).setLaneId(1);
        ((Lane)li).setSpeedLimitInMetersPerSecond(100);
        laneMan.getLanes().put(1, (Lane)li);
        VehicleManager vehMan = new VehicleManager();
        Vehicle veh = new Vehicle(1, 1, 20, 100, 200, 300);
        Vehicle veh2 = new Vehicle(2, 1, 20, 10000, 20000, 30000);
        veh.setLaneID(1);
        veh.setSpeed(5);
        veh2.setLaneID(1);
        veh2.setSpeed(5);
        vehMan.addVehicle(veh);
        vehMan.addVehicle(veh2);

        SignalManager sigMan = new SignalManager();
        DetectorManager detMan = new DetectorManager();

        vehicles = new HashMap<Integer, List<Vehicle>>();
        List<Vehicle> l1veh = new ArrayList<Vehicle>();
        l1veh.add(veh);
        l1veh.add(veh2);
        vehicles.put(1, l1veh);

        irim = new InterRepInfoModel(laneMan, vehMan, sigMan, detMan, new ReferencePoint[0], 2.0, 1.0);
        sscfc = new SuperSimpleCarFollowingCalculator();

        vehList = new ArrayList<IVehicle>();
        vehList.add(new Vehicle(1, 3, 90, 100, 200, 300));
        vehList.add(new Vehicle(2, 3, 90, 100, 200, 300));
        vehList.add(new Vehicle(3, 3, 90, 100, 200, 300));
    }

    @After
    public void teardown() {
        id = null;
        li = null;
        sscfc = null;
    }

    @Test
    public void testConstructor() {
        assertTrue(new SuperSimpleCarFollowingCalculator() instanceof SuperSimpleCarFollowingCalculator);
    }

    private ILane getLaneForDist() {
        Lane l = new Lane();

        LaneNode ln = new LaneNode();
        ln.setX(0.0);
        ln.setY(0.0);
        l.getLaneGeomList().add(ln);

        ln = new LaneNode();
        ln.setX(1000.0);
        ln.setY(1000.0);
        l.getLaneGeomList().add(ln);

        ln = new LaneNode();
        ln.setX(2000.0);
        ln.setY(2000.0);
        l.getLaneGeomList().add(ln);

        return l;
    }

    /*
     * class MyDist implements IDistanceable { double x = 0.0; double y = 0.0;
     * @Override public double getX() { return x; }
     * @Override public double getY() { return y; } }
     */

    @Test
    public void testPerformCarFollowing1() {
        Map<Integer, List<Vehicle>> in = new HashMap<Integer, List<Vehicle>>();
        Map<Integer, List<Vehicle>> out = sscfc.performCarFollowing(in, null);
        assertTrue(in == out);
    }

    @Test
    public void testPerformCarFollowing2() {
        assertEquals(vehicles, sscfc.performCarFollowing(vehicles, irim));
    }

    @Test
    public void testPerformCarFollowing3() {
        Map<Integer, List<Vehicle>> returned = sscfc.performCarFollowing(vehicles, irim);
        assertEquals(5, returned.get(1).get(0).getSpeed(), 0);
        assertEquals(2, returned.get(1).size(), 0);

        returned = sscfc.performCarFollowing(vehicles, irim);
        assertEquals(vehicles, returned);
        assertEquals(1, returned.get(1).size(), 0);
        assertEquals(100, returned.get(1).get(0).getSpeed(), 0.1);
    }

    @Test
    public void testDropVeh() {
        Vehicle v = new Vehicle(1, 3, 90, 10000, 200, 300);
        assertEquals(true, sscfc.dropVeh(v, li));
    }

    @Test
    public void testPlaceVehForLight1() {
        SignalManager sm = PowerMockito.mock(SignalManager.class);
        PowerMockito.when(sm.getSignalsByLaneId(Mockito.anyInt())).thenReturn(null);
        assertEquals(3, sscfc.placeVehForLight(vehList, li, sm).size());
    }

    @Test
    public void testPlaceVehForLight2() {
        SignalManager sm = PowerMockito.mock(SignalManager.class);
        List<SignalIndication> sigs = new ArrayList<SignalIndication>();
        sigs.add(null);
        PowerMockito.when(sm.getSignalsByLaneId(Mockito.anyInt())).thenReturn(sigs);
        assertEquals(3, sscfc.placeVehForLight(vehList, li, sm).size());
    }

    @Test
    public void testPlaceVehForLight3() {
        SignalManager sm = PowerMockito.mock(SignalManager.class);
        List<SignalIndication> sigs = new ArrayList<SignalIndication>();
        SignalIndication si = new SignalIndication();
        si.setColorIndication(Color.RED);
        sigs.add(si);
        PowerMockito.when(sm.getSignalsByLaneId(Mockito.anyInt())).thenReturn(sigs);
        assertEquals(4, sscfc.placeVehForLight(vehList, li, sm).size());
    }

    @Test
    public void testGetVehForLight() {
        IVehicle veh = sscfc.getVehForLight(li);
        assertEquals(-4.42, veh.getX(), 0.1);
        assertEquals(-4.42, veh.getY(), 0.1);
    }

    @Test
    public void testOver50sAway() {
        Vehicle v1 = new Vehicle(1, 3, 90, 800, 2000, 3000);
        Vehicle v2 = new Vehicle(2, 3, 90, 100000, 200000, 300000);
        assertEquals(true, sscfc.over50sAway(v1, v2, 5));

    }

    @Test
    public void testGetInFront() {
        IVehicle vret = sscfc.getInFront((Vehicle)vehList.get(1), vehList);
        assertEquals(vret, vehList.get(0));
    }

    @Test
    public void testGetMoveToward1() {
        IDistanceable d = sscfc.getMoveToward(id, li, 100);
        assertEquals(1000.0, d.getX(), 0.005);
        assertEquals(1000.0, d.getY(), 0.005);
    }

    @Test
    public void testGetMoveToward2() {
        IDistanceable d = sscfc.getMoveToward(id, li, 1500);
        assertEquals(0.0, d.getX(), 0.005);
        assertEquals(0.0, d.getY(), 0.005);
    }

    @Test
    public void testGetBefore() {
        IDistanceable d = sscfc.getBefore(id, li);
        assertEquals(1000.0, d.getX(), 0.005);
        assertEquals(1000.0, d.getY(), 0.005);
    }

    @Test
    public void testPerformMove() {
        Vehicle v = new Vehicle(1, 2, 200, 50, 100, 200);
        sscfc.performMove(v, new DistanceImpl(100, 200, 300), 20);
        assertEquals(58.94, v.getX(), 0.1);
        assertEquals(117.88, v.getY(), 0.1);
    }

    @Test
    public void testEq3() {
        double answer = sscfc.eq3(5, 10, 15, 20, 25);
        assertEquals(5.93, answer, 0.1);
    }

    @Test
    public void testC1() {
        double answer = sscfc.c1(5, 3, 0, 2);
        assertEquals(14, answer, 0);
    }

    @Test
    public void testC2() {
        double answer = sscfc.c2(3, 5, 0, 2);
        assertEquals(8, answer, 0);
    }

    @Test
    public void testC3() {
        double answer = sscfc.c3(5, 0, 0, 10, 1);
        assertEquals(1, answer, 0);
    }

    @Test
    public void testSubC1() {
        double answer = sscfc.subC1(5, 10, 15);
        assertEquals(6, answer, 0);
    }
}
