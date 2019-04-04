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

package org.etexascode.interrep;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.junit.Test;

/**
 * Tests all relevant functionality in UIDetector.
 * 
 * @author ablatt
 */
public class UtilsInterRepTest {

    private final double TOLERANCE = 0.005;

    public UtilsInterRepTest() {
        super();
    }

    @Test
    public void testConstructor() {
        assertNotNull(new UtilsInterRep());
    }

    @Test
    public void testAssignVehicleToLane() {
        LaneManager lm = new LaneManager();
        List<Vehicle> vehicles = new ArrayList<Vehicle>();
        List<Integer> correctLanes = new ArrayList<Integer>();
        populateAssignVehicleToLaneData(lm, vehicles, correctLanes);

        for (int k = 0; k < vehicles.size(); ++k) {
            Vehicle v = vehicles.get(k);
            UtilsInterRep.getLaneAssignment(v, lm);
            String failMsg = String.format("Vehicle at (%.3f, %.3f) should be in lane %d but was assigned to %d.", v.getX(), v.getY(), correctLanes.get(k).intValue(), v.getLaneID());
            assertEquals(failMsg, correctLanes.get(k).intValue(), v.getLaneID());
        }
        Vehicle v = new Vehicle(1, 0.0, 0.0, 0.0, 0.0, 0.0);
        v.setLaneID(5);
        int testarg = 5;
        UtilsInterRep.getLaneAssignment(v, lm);
        assertTrue(testarg == v.getLaneID());
    }

    private void populateAssignVehicleToLaneData(LaneManager lm, List<Vehicle> vehicles, List<Integer> correctLanes) {

        List<Double> xVals;
        List<Double> yVals;
        List<Double> wVals;

        Lane l1 = new Lane();
        xVals = Arrays.asList(100.0, 100.0, 100.0);
        yVals = Arrays.asList(100.0, 500.0, 1000.0);
        wVals = Arrays.asList(100.0, 100.0, 100.0);
        populateLane(l1, 1, xVals, yVals, wVals);

        Lane l2 = new Lane();
        xVals = Arrays.asList(200.0, 200.0, 200.0, 200.0, 200.0, 200.0);
        yVals = Arrays.asList(100.0, 200.0, 400.0, 600.0, 800.0, 1200.0);
        wVals = Arrays.asList(100.0, 100.0, 100.0, 100.0, 100.0, 100.0);
        populateLane(l2, 2, xVals, yVals, wVals);

        Lane l3 = new Lane();
        xVals = Arrays.asList(400.0, 600.0, 800.0);
        yVals = Arrays.asList(400.0, 600.0, 800.0);
        wVals = Arrays.asList(100.0, 100.0, 100.0);
        populateLane(l3, 3, xVals, yVals, wVals);

        Lane l4 = new Lane();
        xVals = Arrays.asList(400.0, 1000.0);
        yVals = Arrays.asList(0.0, 0.0);
        wVals = Arrays.asList(100.0, 100.0);
        populateLane(l4, 4, xVals, yVals, wVals);

        Lane l5 = new Lane();
        xVals = Arrays.asList(300.0, 300.0, 300.0, 300.0);
        yVals = Arrays.asList(-200.0, -400.0, -600.0, -1000.0);
        wVals = Arrays.asList(100.0, 75.0, 25.0, 200.0);
        populateLane(l5, 5, xVals, yVals, wVals);

        Lane l6 = new Lane();
        xVals = Arrays.asList(0.0, -100.0, -200.0, -300.0);
        yVals = Arrays.asList(-200.0, -200.0 - 100 * Math.sqrt(3.0), -300.0 - 100 * Math.sqrt(3.0), -300.0 - 200 * Math.sqrt(3.0));
        wVals = Arrays.asList(100.0, 100.0, 100.0, 100.0);
        populateLane(l6, 6, xVals, yVals, wVals);

        Lane l7 = new Lane();
        xVals = Arrays.asList(-400.0, -400.0 - 100 * Math.sqrt(3.0), -400.0 - 200 * Math.sqrt(3.0));
        yVals = Arrays.asList(-100.0, -200.0, -300.0);
        wVals = Arrays.asList(100.0, 100.0, 100.0);
        populateLane(l7, 7, xVals, yVals, wVals);

        Lane l8 = new Lane();
        xVals = new ArrayList<Double>();
        yVals = new ArrayList<Double>();
        wVals = new ArrayList<Double>();
        for (int k = 0; k <= 6; ++k) {
            double theta = -Math.PI / 2 - (k / 6.0) * (Math.PI / 2);
            xVals.add(1000 * Math.cos(theta) - 500.0);
            yVals.add(1000 * Math.sin(theta) + 1000.0);
            wVals.add(100.0);
        }
        populateLane(l8, 8, xVals, yVals, wVals);

        Map<Integer, Lane> lanes = lm.getLanes();
        lanes.put(l1.getLaneId(), l1);
        lanes.put(l2.getLaneId(), l2);
        lanes.put(l3.getLaneId(), l3);
        lanes.put(l4.getLaneId(), l4);
        lanes.put(l5.getLaneId(), l5);
        lanes.put(l6.getLaneId(), l6);
        lanes.put(l7.getLaneId(), l7);
        lanes.put(l8.getLaneId(), l8);

        addVehicle(0.0, 0.0, 0, vehicles, correctLanes);
        addVehicle(100.0, 300.0, 1, vehicles, correctLanes);
        addVehicle(120.0, 800.0, 1, vehicles, correctLanes);

        addVehicle(200.0, 100.0, 2, vehicles, correctLanes);
        addVehicle(200.0, 1150.0, 2, vehicles, correctLanes);

        addVehicle(500.0, 500.0, 3, vehicles, correctLanes);

        addVehicle(600.0, 30.0, 4, vehicles, correctLanes);
        addVehicle(600.0, -30.0, 4, vehicles, correctLanes);

        addVehicle(300.0, -300.0, 5, vehicles, correctLanes);
        addVehicle(300.0, -500.0, 5, vehicles, correctLanes);
        addVehicle(300.0, -800.0, 5, vehicles, correctLanes);
        addVehicle(330.0, -600.0, 0, vehicles, correctLanes);

        addVehicle(-150.0, -400.0, 6, vehicles, correctLanes);

        addVehicle(-600.0, -210.0, 7, vehicles, correctLanes);

        addVehicle(-1200.0, 300.0, 8, vehicles, correctLanes);
    }

    private void addVehicle(double x, double y, int correctLane, List<Vehicle> vehicles, List<Integer> correctLanes) {
        Vehicle ret = new Vehicle(1, 0.0, 0.0, x, y, 0.0);

        vehicles.add(ret);
        correctLanes.add(correctLane);
    }

    private void populateLane(Lane lane, int id, List<Double> xVals, List<Double> yVals, List<Double> wVals) {
        for (int k = 0; k < xVals.size(); ++k) {
            LaneNode n = new LaneNode(xVals.get(k), yVals.get(k));
            n.setWidth(wVals.get(k));
            lane.addLaneNode(n);
        }
        lane.setLaneId(id);
    }

    @Test
    public void testDeepCopy() {
        String input1 = "foobar";
        assertEquals("Failed to copy a String.", input1, UtilsInterRep.deepCopy(input1));

        Detector input2 = new Detector();
        assertEquals("Failed to copy a Detector.", input2.toString(), UtilsInterRep.deepCopy(input2).toString());

        Vehicle input3 = new Vehicle(1, 0.0, 0.0, 0.0, 0.0, 0.0);
        assertEquals("Failed to copy a Vehicle.", input3.toString(), UtilsInterRep.deepCopy(input3).toString());
    }

    @Test
    public void testGenDetEvent() {
        Vehicle v = new Vehicle(1, 10.0, 10.0, 0.0, 0.0, 0.0);
        v.setHeading(180.0);
        v.setSpeed(5.0);

        Detector d1 = new Detector();
        d1.setDetectorID(1);
        Polygon p1 = new Polygon();
        p1.addPoint(-50, -50);
        p1.addPoint(30, -50);
        p1.addPoint(30, 30);
        p1.addPoint(-50, 30);
        d1.setArea(p1);

        Detector d2 = new Detector();
        d2.setDetectorID(2);
        Polygon p2 = new Polygon();
        p2.addPoint(-100, -100);
        p2.addPoint(-99, -100);
        p2.addPoint(-99, -99);
        p2.addPoint(-100, -99);
        d2.setArea(p2);

        DetectorManager dm = new DetectorManager();
        dm.addDetector(1, d1);
        dm.addDetector(2, d2);

        DetectorEvent expected = new DetectorEvent();
        expected.setDetectorId(1);
        expected.setPresence(true);
        expected.setPulse(1);

        LaneManager lm = new LaneManager();
        Lane l = new Lane();
        LaneNode l1 = new LaneNode(0.0, 0.0);
        LaneNode l2 = new LaneNode(-2.0, -3.0);
        l.addLaneNode(l1);
        l.addLaneNode(l2);
        l.setLaneId(1);
        lm.getLanes().put(1, l);

        UtilsInterRep.genDetEvent(v, v, dm, dm, lm);

        assertEquals(expected, dm.getDetector(1).getDetEvent());
    }

    @Test
    public void testGenHeading() {
        Vehicle curr = new Vehicle(1, 0.0, 0.0, 0.0, 2.0, 0.0);
        curr.setLaneID(1);
        curr.setHeading(180.0);
        curr.setSpeed(2.0);
        Double currHeading = curr.getHeading();

        double[] newHeading = new double[] { Math.sin(Math.toRadians(currHeading)), Math.cos(Math.toRadians(currHeading)) };

        assertArrayEquals(newHeading, UtilsInterRep.genHeading(curr), TOLERANCE);
    }

    @Test
    public void testGenVehicleHeading() {
        LaneManager lm = new LaneManager();
        Lane l = new Lane();
        LaneNode l1 = new LaneNode(0.0, 0.0);
        LaneNode l2 = new LaneNode(-2.0, -3.0);
        l.addLaneNode(l1);
        l.addLaneNode(l2);
        l.setLaneId(1);
        lm.getLanes().put(1, l);

        Vehicle unassigned = new Vehicle(0, 0.0, 0.0, 0.0, 0.0, 0.0);
        unassigned.setHeading(0.0);

        Vehicle curr = new Vehicle(1, 0.0, 0.0, 4.0, 1.0, 0.0);
        curr.setLaneID(1);
        curr.setSpeed(10.0);

        Vehicle prev = new Vehicle(1, 0.0, 0.0, 0.0, 2.0, 0.0);
        prev.setLaneID(1);
        prev.setHeading(180.0);
        prev.setSpeed(2.0);

        Lane laneById = lm.getLaneById(curr.getLaneID());
        List<LaneNode> nodes = laneById.getLaneGeomList();
        LaneNode first = nodes.get(0);
        LaneNode second = nodes.get(1);

        Double currHeading = UtilsInterRep.calcAngle(first, second);

        Double prevHeading = UtilsInterRep.genVehicleHeading(curr, prev, lm);

        assertEquals(unassigned.getHeading(), UtilsInterRep.genVehicleHeading(unassigned, null, lm), TOLERANCE); // keeps
                                                                                                                 // old
                                                                                                                 // heading
                                                                                                                 // due
                                                                                                                 // to
                                                                                                                 // lack
                                                                                                                 // of
                                                                                                                 // lm
                                                                                                                 // and
                                                                                                                 // prev
                                                                                                                 // vehicle
        assertEquals(prev.getHeading(), UtilsInterRep.genVehicleHeading(prev, prev, lm), TOLERANCE); // keeps
                                                                                                     // old
                                                                                                     // heading
                                                                                                     // due
                                                                                                     // to
                                                                                                     // being
                                                                                                     // stopped
        assertEquals(currHeading, UtilsInterRep.genVehicleHeading(curr, null, lm), TOLERANCE); // sets
                                                                                               // heading
                                                                                               // from
                                                                                               // lane
        assertEquals(prevHeading, UtilsInterRep.genVehicleHeading(curr, prev, lm), TOLERANCE); // sets
                                                                                               // heading
                                                                                               // from
                                                                                               // previous
                                                                                               // position
    }

    @Test
    public void testCalcHeading() {
        // Horizontal lanes
        IDistanceable v1 = new DistanceImpl(5, 0, 0);
        IDistanceable v2 = new DistanceImpl(2, 0, 0);

        assertEquals(270.0, UtilsInterRep.calcAngle(v1, v2), TOLERANCE);
        v1 = new DistanceImpl(-5, 0, 0);
        v2 = new DistanceImpl(-2, 0, 0);
        assertEquals(90.0, UtilsInterRep.calcAngle(v1, v2), TOLERANCE);

        // Vertical lanes
        v1 = new DistanceImpl(0, 5, 0);
        v2 = new DistanceImpl(0, 2, 0);
        assertEquals(180.0, UtilsInterRep.calcAngle(v1, v2), TOLERANCE);
        v1 = new DistanceImpl(0, -5, 0);
        v2 = new DistanceImpl(0, -2, 0);
        assertEquals(0.0, UtilsInterRep.calcAngle(v1, v2), TOLERANCE);
    }

    @Test
    public void testGetLastNode() {
        Lane l = new Lane();
        LaneNode n1 = new LaneNode(1, 2);
        LaneNode n2 = new LaneNode(3, 4);
        l.addLaneNode(n1);
        l.addLaneNode(n2);
        assertEquals(n2, UtilsInterRep.getLastNode(l));
    }

    @Test
    public void testVehicleInLogoutZone() {

    }

    @Test
    public void testGenVehicleAcceleration() {
        Vehicle v1 = new Vehicle(1, 10, 10, 35, 35, 0.0);
        v1.setSpeed(15);

        Vehicle v2 = new Vehicle(2, 10, 10, 15, 15, 0.0);
        v2.setSpeed(10);

        double timeStepInterval = 12;
        double acctest = UtilsCalculations.getAcceleration(v1, v2, timeStepInterval);
        v1.setAcceleration(UtilsCalculations.getAcceleration(v1, v2, timeStepInterval));
        assertTrue(acctest == v1.getAcceleration());
    }
}
