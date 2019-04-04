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
package org.etexascode.apps;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.junit.Test;

/**
 * @author cdeisher
 * @author ttevendale
 */
public class UtilsEqualsTest {

    @Test
    public void testLaneNodeCloselyEquals() {

        LaneNode laneNode1 = null;
        LaneNode laneNode2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(laneNode1, laneNode2));

        laneNode1 = new LaneNode();

        // one null
        assertFalse(UtilsEquals.closelyEquals(laneNode1, laneNode2));
        assertFalse(UtilsEquals.closelyEquals(laneNode2, laneNode1));

        laneNode2 = new LaneNode();

        // same
        assertTrue(UtilsEquals.closelyEquals(laneNode1, laneNode2));
    }

    @Test
    public void testLaneNodeCloselyEquals2() {

        double offset = UtilsEquals.CM_OFFSET_TOLERANCE;
        double doubleOffset = UtilsEquals.DOUBLE_TOLERANCE;
        LaneNode laneNode1 = new LaneNode(0, 0, 0, 0);
        LaneNode laneNode2 = new LaneNode(0 + offset, 0 + offset, 0 + doubleOffset, 0 + doubleOffset);

        // just within x, y, z, and width tolerance
        assertTrue(UtilsEquals.closelyEquals(laneNode1, laneNode2));

        laneNode2.setX(laneNode2.getX() + 0.1);

        // just out of x tolerance
        assertFalse(UtilsEquals.closelyEquals(laneNode1, laneNode2));

        laneNode2.setX(laneNode2.getX() - 0.1);
        laneNode2.setY(laneNode2.getY() + 0.1);

        // just out of y tolerance
        assertFalse(UtilsEquals.closelyEquals(laneNode1, laneNode2));

        laneNode2.setY(laneNode2.getY() - 0.1);
        laneNode2.setZ(laneNode2.getZ() + 0.1);

        // just out of z tolerance
        assertFalse(UtilsEquals.closelyEquals(laneNode1, laneNode2));

        laneNode2.setZ(laneNode2.getZ() - 0.1);
        laneNode2.setWidth(laneNode2.getWidth() + 0.1);

        // just out of width tolerance
        assertFalse(UtilsEquals.closelyEquals(laneNode1, laneNode2));
    }

    @Test
    public void testLaneCloselyEquals() {

        Lane lane1 = null;
        Lane lane2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(lane1, lane2));

        lane1 = new Lane();

        // one null
        assertFalse(UtilsEquals.closelyEquals(lane1, lane2));
        assertFalse(UtilsEquals.closelyEquals(lane2, lane1));

        lane2 = new Lane();

        // same
        assertTrue(UtilsEquals.closelyEquals(lane1, lane2));
    }

    @Test
    public void testLaneCloselyEquals2() {

        Lane lane1 = new Lane();
        Lane lane2 = new Lane();

        // same
        assertTrue(UtilsEquals.closelyEquals(lane1, lane2));

        lane1.setLaneId(1);
        lane2.setLaneId(2);

        // different lane ID
        assertFalse(UtilsEquals.closelyEquals(lane1, lane2));

        lane2.setLaneId(1);
        lane1.setApproachId(1);
        lane2.setApproachId(2);

        // different approach ID
        assertFalse(UtilsEquals.closelyEquals(lane1, lane2));

        lane2.setApproachId(1);
        lane1.setLaneGeomList(null);
        lane2.setLaneGeomList(null);

        // same lanes with null lane nodes
        assertTrue(UtilsEquals.closelyEquals(lane1, lane2));

        lane1.setLaneGeomList(new ArrayList<LaneNode>());

        // same lanes with only one null lane nodes
        assertFalse(UtilsEquals.closelyEquals(lane1, lane2));

        lane2.setLaneGeomList(new ArrayList<LaneNode>());

        // same lanes with empty lane nodes
        assertTrue(UtilsEquals.closelyEquals(lane1, lane2));
    }

    @Test
    public void testLaneCloselyEquals3() {

        Lane lane1 = new Lane();
        Lane lane2 = new Lane();

        // same
        assertTrue(UtilsEquals.closelyEquals(lane1, lane2));

        lane1.setLaneGeomList(new ArrayList<LaneNode>());
        List<LaneNode> nodes = new ArrayList<LaneNode>();
        nodes.add(new LaneNode(0, 0));
        lane2.setLaneGeomList(nodes);

        // different sized lane node lists
        assertFalse(UtilsEquals.closelyEquals(lane1, lane2));

        lane1.setLaneGeomList(nodes);

        // same lane nodes lists
        assertTrue(UtilsEquals.closelyEquals(lane1, lane2));

        nodes = new ArrayList<LaneNode>();
        nodes.add(new LaneNode(10, 10));
        lane2.setLaneGeomList(nodes);

        // same sized node lists but not closely equals nodes
        assertFalse(UtilsEquals.closelyEquals(lane1, lane2));
    }

    @Test
    public void testLaneManangerCloselyEquals() {

        LaneManager laneManager1 = null;
        LaneManager laneManager2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(laneManager1, laneManager2));

        laneManager1 = new LaneManager();

        // one null
        assertFalse(UtilsEquals.closelyEquals(laneManager1, laneManager2));
        assertFalse(UtilsEquals.closelyEquals(laneManager2, laneManager1));

        laneManager2 = new LaneManager();

        // same
        assertTrue(UtilsEquals.closelyEquals(laneManager1, laneManager2));
    }

    @Test
    public void testLaneManangerCloselyEquals2() {

        LaneManager laneManager1 = new LaneManager();
        LaneManager laneManager2 = new LaneManager();

        laneManager1.setLatitude(0);
        laneManager1.setLongitude(0);
        laneManager2.setLatitude(0 + UtilsEquals.LATITUDE_TOLERANCE);
        laneManager2.setLongitude(0 + UtilsEquals.LONGITUDE_TOLERANCE);

        // just within latitude and longitude tolerance
        assertTrue(UtilsEquals.closelyEquals(laneManager1, laneManager2));

        laneManager2.setLatitude(laneManager2.getLatitude() + 0.000001);

        // just out of latitude tolerance
        assertFalse(UtilsEquals.closelyEquals(laneManager1, laneManager2));

        laneManager2.setLatitude(laneManager2.getLatitude() - 0.000001);
        laneManager2.setLongitude(laneManager2.getLongitude() + 0.000001);

        // just out of longitude tolerance
        assertFalse(UtilsEquals.closelyEquals(laneManager1, laneManager2));
    }

    @Test
    public void testLaneManangerCloselyEquals3() {

        LaneManager laneManager1 = new LaneManager();
        LaneManager laneManager2 = new LaneManager();

        Map<Integer, Lane> laneMap1 = new HashMap<Integer, Lane>();
        Map<Integer, Lane> laneMap2 = new HashMap<Integer, Lane>();

        laneMap1.put(1, new Lane());
        laneMap2.put(1, new Lane());
        laneMap2.put(2, new Lane());

        laneManager1.setLanes(laneMap1);
        laneManager2.setLanes(laneMap2);

        // different lane IDs the lane managers
        assertFalse(UtilsEquals.closelyEquals(laneManager1, laneManager2));
        assertFalse(UtilsEquals.closelyEquals(laneManager2, laneManager1));

        laneManager2.setLanes(laneMap1);

        // same lanes
        assertTrue(UtilsEquals.closelyEquals(laneManager1, laneManager2));

        laneMap2 = new HashMap<Integer, Lane>();
        Lane lane = new Lane();
        lane.addLaneNode(new LaneNode(0, 0));
        laneMap2.put(1, lane);
        laneManager2.setLanes(laneMap2);

        // same lane ID but different lanes
        assertFalse(UtilsEquals.closelyEquals(laneManager1, laneManager2));
    }

    @Test
    public void testLaneMovementCloselyEquals() {

        LaneMovement laneMovement1 = new LaneMovement();
        LaneMovement laneMovement2 = new LaneMovement();

        laneMovement1.setMovementId(1);
        laneMovement2.setMovementId(1);
        laneMovement1.setMovement(Movement.RIGHT_TURN_ON_RED);
        laneMovement2.setMovement(Movement.RIGHT_TURN_ON_RED);

        // same
        assertTrue(UtilsEquals.closelyEquals(laneMovement1, laneMovement2));

        laneMovement2.setMovement(Movement.LEFT_TURN);

        // different movement
        assertFalse(UtilsEquals.closelyEquals(laneMovement1, laneMovement2));

        laneMovement2.setMovement(Movement.RIGHT_TURN_ON_RED);
        laneMovement2.setMovementId(2);

        // different movement ID
        assertFalse(UtilsEquals.closelyEquals(laneMovement1, laneMovement2));
    }

    @Test
    public void testSignalIndicationCloselyEquals() {

        SignalIndication signal1 = null;
        SignalIndication signal2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(signal1, signal2));

        signal1 = new SignalIndication();

        // one null
        assertFalse(UtilsEquals.closelyEquals(signal1, signal2));
        assertFalse(UtilsEquals.closelyEquals(signal2, signal1));

        signal2 = new SignalIndication();

        // same
        assertTrue(UtilsEquals.closelyEquals(signal1, signal2));
    }

    @Test
    public void testSignalIndicationCloselyEquals2() {

        SignalIndication signal1 = new SignalIndication();
        SignalIndication signal2 = new SignalIndication();

        signal1.setLaneId(1);
        signal1.setColorIndication(Color.GREEN);
        signal1.setStateIndication(State.STEADY);
        signal1.setTypeIndication(Type.BALL);
        signal1.setTimeToChange(10);

        signal2.setLaneId(1);
        signal2.setColorIndication(Color.GREEN);
        signal2.setStateIndication(State.STEADY);
        signal2.setTypeIndication(Type.BALL);
        signal2.setTimeToChange(10);

        // same
        assertTrue(UtilsEquals.closelyEquals(signal1, signal2));

        signal2.setTimeToChange(signal2.getTimeToChange() + UtilsEquals.DOUBLE_TOLERANCE);

        // same / just within tolerance
        assertTrue(UtilsEquals.closelyEquals(signal1, signal2));

        signal2.setTimeToChange(signal2.getTimeToChange() + 0.00001);

        // time to change out of tolerance
        assertFalse(UtilsEquals.closelyEquals(signal1, signal2));

        signal2.setTimeToChange(signal2.getTimeToChange() - 0.00001);
        signal2.setTypeIndication(Type.LEFT_ARROW);

        // different type
        assertFalse(UtilsEquals.closelyEquals(signal1, signal2));

        signal2.setTypeIndication(Type.BALL);
        signal2.setStateIndication(State.FLASHING);

        // different state
        assertFalse(UtilsEquals.closelyEquals(signal1, signal2));

        signal2.setStateIndication(State.STEADY);
        signal2.setColorIndication(Color.RED);

        // different color
        assertFalse(UtilsEquals.closelyEquals(signal1, signal2));

        signal2.setColorIndication(Color.GREEN);
        signal2.setLaneId(2);

        // different lane ID
        assertFalse(UtilsEquals.closelyEquals(signal1, signal2));
    }

    @Test
    public void testSignalManagerCloselyEquals() {

        SignalManager signalManager1 = null;
        SignalManager signalManager2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(signalManager1, signalManager2));

        signalManager1 = new SignalManager();

        // one null
        assertFalse(UtilsEquals.closelyEquals(signalManager1, signalManager2));
        assertFalse(UtilsEquals.closelyEquals(signalManager2, signalManager1));

        signalManager2 = new SignalManager();

        // same
        assertTrue(UtilsEquals.closelyEquals(signalManager1, signalManager2));
    }

    @Test
    public void testSignalManagerCloselyEquals2() {

        SignalManager signalManager1 = new SignalManager();
        SignalManager signalManager2 = new SignalManager();

        SignalIndication signal1 = new SignalIndication();
        SignalIndication signal2 = new SignalIndication();
        SignalIndication signal3 = new SignalIndication();

        signal1.setLaneId(1);
        signal1.setColorIndication(Color.GREEN);
        signal1.setStateIndication(State.STEADY);
        signal1.setTypeIndication(Type.BALL);
        signal1.setTimeToChange(10);

        signal2.setLaneId(2);
        signal2.setColorIndication(Color.GREEN);
        signal2.setStateIndication(State.STEADY);
        signal2.setTypeIndication(Type.BALL);
        signal2.setTimeToChange(10);

        signal3.setLaneId(3);
        signal3.setColorIndication(Color.GREEN);
        signal3.setStateIndication(State.STEADY);
        signal3.setTypeIndication(Type.BALL);
        signal3.setTimeToChange(10);

        signalManager2.addSignal(signal2);

        // number of unique lanes is different
        assertFalse(UtilsEquals.closelyEquals(signalManager1, signalManager2));

        signalManager1.addSignal(signal2);
        signalManager1.addSignal(signal3);
        signalManager2.addSignal(signal1);

        // contains different unique lanes
        assertFalse(UtilsEquals.closelyEquals(signalManager1, signalManager2));
    }

    @Test
    public void testSignalManagerCloselyEquals3() {

        SignalManager signalManager1 = new SignalManager();
        SignalManager signalManager2 = new SignalManager();

        SignalIndication signal1 = new SignalIndication();
        SignalIndication signal2 = new SignalIndication();

        signal1.setLaneId(1);
        signal1.setColorIndication(Color.GREEN);
        signal1.setStateIndication(State.STEADY);
        signal1.setTypeIndication(Type.BALL);
        signal1.setTimeToChange(10);

        signal2.setLaneId(1);
        signal2.setColorIndication(Color.RED);
        signal2.setStateIndication(State.STEADY);
        signal2.setTypeIndication(Type.BALL);
        signal2.setTimeToChange(10);

        signalManager2.addSignal(signal1);
        signalManager2.addSignal(signal1);
        signalManager1.addSignal(signal1);

        // different number of signals for a lane
        assertFalse(UtilsEquals.closelyEquals(signalManager1, signalManager2));

        signalManager1.addSignal(signal1);

        // makes sure that the value comes out true again after bringing the number of signals in
        // each list to be the same
        assertTrue(UtilsEquals.closelyEquals(signalManager1, signalManager2));

        signalManager1.addSignal(signal2);
        signalManager2.addSignal(signal1);

        // tests that the signals don't match
        assertFalse(UtilsEquals.closelyEquals(signalManager1, signalManager2));
        assertFalse(UtilsEquals.closelyEquals(signalManager2, signalManager1));
    }

    @Test
    public void testVehicleCloselyEquals() {

        Vehicle vehicle1 = null;
        Vehicle vehicle2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle1 = new Vehicle(1, 500, 200, 0, 0, 0);
        vehicle1.setSpeed(20);

        // one null
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));
        assertFalse(UtilsEquals.closelyEquals(vehicle2, vehicle1));

        vehicle2 = new Vehicle(1, 500, 200, 0, 0, 0);
        vehicle2.setSpeed(20);

        // same
        assertTrue(UtilsEquals.closelyEquals(vehicle1, vehicle2));
    }

    @Test
    public void testVehicleCloselyEquals2() {

        double offset = UtilsEquals.DOUBLE_TOLERANCE;
        Vehicle vehicle1 = new Vehicle(1, 500, 200, 0, 0, 0);
        vehicle1.setLaneID(1);
        vehicle1.setSpeed(20);
        vehicle1.setHeading(0);
        Vehicle vehicle2 = new Vehicle(1, 500 + offset, 200 + offset, 0 + offset, 0 + offset, 0 + offset);
        vehicle2.setLaneID(1);
        vehicle2.setSpeed(19.999999 + offset);
        vehicle2.setHeading(0 + offset);

        // just within tolerance
        assertTrue(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setVehicleID(2);

        // different Vehicle ID
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setVehicleID(1);
        vehicle2.setLaneID(2);

        // different Lane ID
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setLaneID(1);
        vehicle2.setX(vehicle2.getX() + 0.00001);

        // different x
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setX(vehicle2.getX() - 0.00001);
        vehicle2.setY(vehicle2.getY() + 0.00001);

        // different y
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setY(vehicle2.getY() - 0.00001);
        vehicle2.setZ(vehicle2.getZ() + 0.00001);

        // different z
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setZ(vehicle2.getZ() - 0.00001);
        vehicle2.setSpeed(vehicle2.getSpeed() + 0.00001);

        // different speed
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setSpeed(vehicle2.getSpeed() - 0.00001);
        vehicle2.setLength(vehicle2.getLength() + 0.00001);

        // different length
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setLength(vehicle2.getLength() - 0.00001);
        vehicle2.setWidth(vehicle2.getWidth() + 0.00001);

        // different width
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));

        vehicle2.setWidth(vehicle2.getWidth() - 0.00001);
        vehicle2.setHeading(vehicle2.getHeading() + 0.00001);

        // different heading
        assertFalse(UtilsEquals.closelyEquals(vehicle1, vehicle2));
    }

    @Test
    public void testVehicleManangerCloselyEquals() {

        VehicleManager vehicleManager1 = null;
        VehicleManager vehicleManager2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(vehicleManager1, vehicleManager2));

        vehicleManager1 = new VehicleManager();

        // one null
        assertFalse(UtilsEquals.closelyEquals(vehicleManager1, vehicleManager2));
        assertFalse(UtilsEquals.closelyEquals(vehicleManager2, vehicleManager1));

        vehicleManager2 = new VehicleManager();

        // same
        assertTrue(UtilsEquals.closelyEquals(vehicleManager1, vehicleManager2));
    }

    @Test
    public void testVehicleManangerCloselyEquals2() {

        VehicleManager vehicleManager1 = new VehicleManager();
        VehicleManager vehicleManager2 = new VehicleManager();

        Vehicle vehicle1 = new Vehicle(1, 500, 200, 0, 0, 0);
        vehicle1.setSpeed(10);
        Vehicle vehicle1b = new Vehicle(1, 200, 500, 100, -10, 0);
        vehicle1b.setSpeed(10);
        Vehicle vehicle2 = new Vehicle(2, 500, 200, 0, 0, 0);
        vehicle2.setSpeed(10);

        vehicleManager1.addVehicle(vehicle1);
        vehicleManager2.addVehicle(vehicle1);
        vehicleManager2.addVehicle(vehicle2);

        // has different unique vehicle proper IDs
        assertFalse(UtilsEquals.closelyEquals(vehicleManager1, vehicleManager2));
        assertFalse(UtilsEquals.closelyEquals(vehicleManager2, vehicleManager1));

        vehicleManager2.removeVehicle(vehicle2.getProperId());

        // same
        assertTrue(UtilsEquals.closelyEquals(vehicleManager1, vehicleManager2));

        vehicleManager2.removeVehicle(vehicle1.getProperId());
        vehicleManager2.addVehicle(vehicle1b);

        // different vehicles with the same ID
        assertFalse(UtilsEquals.closelyEquals(vehicleManager1, vehicleManager2));
    }

    @Test
    public void testDetectorEventCloselyEquals() {

        DetectorEvent detectorEvent1 = null;
        DetectorEvent detectorEvent2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));

        detectorEvent1 = new DetectorEvent();

        // one null
        assertFalse(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));
        assertFalse(UtilsEquals.closelyEquals(detectorEvent2, detectorEvent1));

        detectorEvent2 = new DetectorEvent();

        // same
        assertTrue(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));
    }

    @Test
    public void testDetectorEventCloselyEquals2() {

        DetectorEvent detectorEvent1 = new DetectorEvent();
        DetectorEvent detectorEvent2 = new DetectorEvent();

        detectorEvent1.setDetectorId(1);
        detectorEvent1.setPulse(10);
        detectorEvent1.setPresence(true);
        detectorEvent1.setLength(100);
        detectorEvent1.setSpeed(20);

        detectorEvent2.setDetectorId(1);
        detectorEvent2.setPulse(10);
        detectorEvent2.setPresence(true);
        detectorEvent2.setLength(99.999999 + UtilsEquals.DOUBLE_TOLERANCE);
        detectorEvent2.setSpeed(19.999999 + UtilsEquals.DOUBLE_TOLERANCE);

        // just within tolerance
        assertTrue(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));

        detectorEvent2.setDetectorId(2);

        // different ID
        assertFalse(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));

        detectorEvent2.setDetectorId(1);
        detectorEvent2.setPulse(5);

        // different pulse
        assertFalse(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));

        detectorEvent2.setPulse(10);
        detectorEvent2.setPresence(false);

        // different pulse
        assertFalse(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));

        detectorEvent2.setPresence(true);
        detectorEvent2.setLength(detectorEvent2.getLength() + 0.00001);

        // different pulse
        assertFalse(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));

        detectorEvent2.setLength(detectorEvent2.getLength() - 0.00001);
        detectorEvent2.setSpeed(detectorEvent2.getSpeed() + 0.00001);

        // different pulse
        assertFalse(UtilsEquals.closelyEquals(detectorEvent1, detectorEvent2));
    }

    @Test
    public void testDetectorCloselyEquals() {

        Detector detector1 = null;
        Detector detector2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(detector1, detector2));

        detector1 = new Detector();

        // one null
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));
        assertFalse(UtilsEquals.closelyEquals(detector2, detector1));

        detector2 = new Detector();

        // same
        assertTrue(UtilsEquals.closelyEquals(detector1, detector2));
    }

    @Test
    public void testDetectorCloselyEquals2() {

        Detector detector1 = new Detector();
        Detector detector2 = new Detector();

        detector1.setDetectorID(1);
        detector1.setLengthDetectCap(true);
        detector1.setPresenceDetectCap(true);
        detector1.setPulseDetectCap(true);
        detector1.setSpeedDetectCap(true);

        detector2.setDetectorID(1);
        detector2.setLengthDetectCap(true);
        detector2.setPresenceDetectCap(true);
        detector2.setPulseDetectCap(true);
        detector2.setSpeedDetectCap(true);

        // same
        assertTrue(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setDetectorID(2);

        // different ID
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setDetectorID(1);
        detector2.setLengthDetectCap(false);

        // different length detect capacity
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setLengthDetectCap(true);
        detector2.setPresenceDetectCap(false);

        // different presence detect capacity
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setPresenceDetectCap(true);
        detector2.setPulseDetectCap(false);

        // different pulse detect capacity
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setPulseDetectCap(true);
        detector2.setSpeedDetectCap(false);

        // different pulse detect capacity
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setSpeedDetectCap(true);
        detector2.setDetEvent(new DetectorEvent());

        // one Detector has an event and the other doesn't
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));
        assertFalse(UtilsEquals.closelyEquals(detector2, detector1));
    }

    @Test
    public void testDetectorCloselyEquals3() {

        Detector detector1 = new Detector();
        Detector detector2 = new Detector();

        List<Integer> laneIds = new ArrayList<Integer>();
        laneIds.add(1);

        detector1.setDetectorID(1);
        detector1.setLengthDetectCap(true);
        detector1.setPresenceDetectCap(true);
        detector1.setPulseDetectCap(true);
        detector1.setSpeedDetectCap(true);
        detector1.setLaneIDs(laneIds);

        detector2.setDetectorID(1);
        detector2.setLengthDetectCap(true);
        detector2.setPresenceDetectCap(true);
        detector2.setPulseDetectCap(true);
        detector2.setSpeedDetectCap(true);
        detector2.setLaneIDs(laneIds);

        // same
        assertTrue(UtilsEquals.closelyEquals(detector1, detector2));

        DetectorEvent detectorEvent1 = new DetectorEvent();
        DetectorEvent detectorEvent2 = new DetectorEvent();

        detectorEvent1.setDetectorId(1);
        detectorEvent1.setPulse(10);
        detectorEvent1.setPresence(true);
        detectorEvent1.setLength(100);
        detectorEvent1.setSpeed(20);

        detectorEvent2.setDetectorId(3);
        detectorEvent2.setPulse(15);
        detectorEvent2.setPresence(false);
        detectorEvent2.setLength(50);
        detectorEvent2.setSpeed(10);

        detector1.setDetEvent(detectorEvent1);
        detector2.setDetEvent(detectorEvent1);

        // same with detector event
        assertTrue(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setDetEvent(detectorEvent2);

        // different detector event
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setDetEvent(detectorEvent1);
        laneIds = new ArrayList<Integer>();
        laneIds.add(1);
        laneIds.add(2);
        detector2.setLaneIDs(laneIds);

        // different lane IDs
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));
        assertFalse(UtilsEquals.closelyEquals(detector2, detector1));
    }

    @Test
    public void testDetectorCloselyEquals4() {

        Detector detector1 = new Detector();
        Detector detector2 = new Detector();

        detector1.setDetectorID(1);
        detector1.setLengthDetectCap(true);
        detector1.setPresenceDetectCap(true);
        detector1.setPulseDetectCap(true);
        detector1.setSpeedDetectCap(true);
        detector1.setArea(null);

        detector2.setDetectorID(1);
        detector2.setLengthDetectCap(true);
        detector2.setPresenceDetectCap(true);
        detector2.setPulseDetectCap(true);
        detector2.setSpeedDetectCap(true);
        detector2.setArea(null);

        // same
        assertTrue(UtilsEquals.closelyEquals(detector1, detector2));

        Polygon polygon1 = new Polygon(new int[] { 0, 0, 10, 10 }, new int[] { 0, 10, 10, 0 }, 4);
        Polygon polygon2 = new Polygon(new int[] { 0, 0, 10 }, new int[] { 0, 10, 10 }, 3);

        detector2.setArea(polygon1);

        // one Detector has an event and the other doesn't
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));
        assertFalse(UtilsEquals.closelyEquals(detector2, detector1));

        detector1.setArea(polygon1);

        // same with area (Polygon)
        assertTrue(UtilsEquals.closelyEquals(detector1, detector2));

        detector2.setArea(polygon2);

        // different area (Polygon)
        assertFalse(UtilsEquals.closelyEquals(detector1, detector2));
    }

    @Test
    public void testDetectorManagerCloselyEquals() {

        DetectorManager detectorManager1 = null;
        DetectorManager detectorManager2 = null;

        // both null
        assertTrue(UtilsEquals.closelyEquals(detectorManager1, detectorManager2));

        detectorManager1 = new DetectorManager();

        // one null
        assertFalse(UtilsEquals.closelyEquals(detectorManager1, detectorManager2));
        assertFalse(UtilsEquals.closelyEquals(detectorManager2, detectorManager1));

        detectorManager2 = new DetectorManager();

        // same
        assertTrue(UtilsEquals.closelyEquals(detectorManager1, detectorManager2));
    }

    @Test
    public void testDetectorManagerCloselyEquals2() {

        DetectorManager detectorManager1 = new DetectorManager();
        DetectorManager detectorManager2 = new DetectorManager();

        Detector detector1 = new Detector();
        Detector detector2 = new Detector();

        detector1.setDetectorID(1);
        detector1.setLengthDetectCap(true);
        detector1.setPresenceDetectCap(true);
        detector1.setPulseDetectCap(true);
        detector1.setSpeedDetectCap(true);

        detector2.setDetectorID(2);
        detector2.setLengthDetectCap(true);
        detector2.setPresenceDetectCap(true);
        detector2.setPulseDetectCap(true);
        detector2.setSpeedDetectCap(true);

        // same
        assertTrue(UtilsEquals.closelyEquals(detectorManager1, detectorManager2));

        detectorManager1.addDetector(detector1.getDetectorID(), detector1);
        detectorManager2.addDetector(detector1.getDetectorID(), detector1);
        detectorManager2.addDetector(detector2.getDetectorID(), detector2);

        // different unique IDs
        assertFalse(UtilsEquals.closelyEquals(detectorManager1, detectorManager2));
        assertFalse(UtilsEquals.closelyEquals(detectorManager2, detectorManager1));

        detectorManager2.clearDetectors();
        detectorManager2.addDetector(detector1.getDetectorID(), detector1);

        // with the same detectors
        assertTrue(UtilsEquals.closelyEquals(detectorManager1, detectorManager2));

        detectorManager2.clearDetectors();
        detectorManager2.addDetector(detector1.getDetectorID(), detector2);

        // different detector with the same ID (in the map)
        assertFalse(UtilsEquals.closelyEquals(detectorManager1, detectorManager2));

    }
}
