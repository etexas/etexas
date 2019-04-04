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
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;

public class SubtestsForInterRep {

    public static boolean checkInterRepState(InterRep interRep) {
        return getAcceptedState(interRep.getCurrentTimeStep()).equals(getCurrentState(interRep));
    }

    private static TestState getAcceptedState(int currentTimeStep) {
        switch (currentTimeStep) {
            case 1:
                return genState1();
            case 2:
                return genState2();
            case 3:
                return genState3();
        }

        return null;
    }

    private static TestState getCurrentState(InterRep interRep) {
        return new TestState(interRep.vehicleManager, interRep.laneManager, interRep.detectorManager, interRep.signalManager);
    }

    private static TestState genState1() {
        return new TestState(genVehicle1(), genLane1(), genDetector1(), genSignal1());
    }

    private static TestState genState2() {
        return new TestState(genVehicle2(), genLane2(), genDetector2(), genSignal2());
    }

    private static TestState genState3() {
        return new TestState(genVehicle3(), genLane3(), genDetector3(), genSignal3());
    }

    private static VehicleManager genVehicle1() {
        VehicleManager ret = new VehicleManager();
        List<Vehicle> l = new ArrayList<Vehicle>();

        Vehicle v = new Vehicle(1, 0.0, 0.0, 0.0, 0.0, 0.0);
        v.setHeading(0.0);
        v.setLaneID(0);
        // v.setLength(0.0);
        v.setSpeed(0.0);
        // v.setVehicleID(0);
        // v.setWidth(0.0);
        // v.setX(0.0);
        // v.setY(0.0);
        // v.setZ(0.0);

        l.add(v);

        v = new Vehicle(2, 0.0, 0.0, 0.0, 0.0, 0.0);
        v.setHeading(0.0);
        v.setLaneID(0);
        v.setLength(0.0);
        v.setSpeed(0.0);
        v.setVehicleID(0);
        v.setWidth(0.0);
        v.setX(0.0);
        v.setY(0.0);
        v.setZ(0.0);

        l.add(v);

        ret.addVehicles(l);
        return ret;
    }

    private static VehicleManager genVehicle2() {
        VehicleManager ret = new VehicleManager();
        List<Vehicle> l = new ArrayList<Vehicle>();

        Vehicle v = new Vehicle(1, 0.0, 0.0, 0.0, 0.0, 0.0);
        v.setHeading(0.0);
        v.setLaneID(0);
        v.setLength(0.0);
        v.setSpeed(0.0);
        v.setVehicleID(0);
        v.setWidth(0.0);
        v.setX(0.0);
        v.setY(0.0);
        v.setZ(0.0);

        l.add(v);

        v = new Vehicle(2, 0.0, 0.0, 0.0, 0.0, 0.0);
        v.setHeading(0.0);
        v.setLaneID(0);
        v.setLength(0.0);
        v.setSpeed(0.0);
        v.setVehicleID(0);
        v.setWidth(0.0);
        v.setX(0.0);
        v.setY(0.0);
        v.setZ(0.0);

        l.add(v);

        ret.addVehicles(l);
        return ret;
    }

    private static VehicleManager genVehicle3() {
        VehicleManager ret = new VehicleManager();
        List<Vehicle> l = new ArrayList<Vehicle>();

        Vehicle v = new Vehicle(1, 0.0, 0.0, 0.0, 0.0, 0.0);
        v.setHeading(0.0);
        v.setLaneID(0);
        v.setLength(0.0);
        v.setSpeed(0.0);
        v.setVehicleID(0);
        v.setWidth(0.0);
        v.setX(0.0);
        v.setY(0.0);
        v.setZ(0.0);

        l.add(v);

        v = new Vehicle(2, 0.0, 0.0, 0.0, 0.0, 0.0);
        v.setHeading(0.0);
        v.setLaneID(0);
        v.setLength(0.0);
        v.setSpeed(0.0);
        v.setVehicleID(0);
        v.setWidth(0.0);
        v.setX(0.0);
        v.setY(0.0);
        v.setZ(0.0);

        l.add(v);

        ret.addVehicles(l);
        return ret;
    }

    private static LaneManager genLane1() {
        LaneManager ret = new LaneManager();

        ret.setLatitude(0.0);
        ret.setLongitude(0.0);
        ret.setElevation(0.0);

        Map<Integer, Lane> retMap = new HashMap<Integer, Lane>();

        Lane l = new Lane();

        l.setLaneId(0);
        l.setApproachId(0);
        l.setType("UNSET");

        List<LaneNode> lnl = new ArrayList<LaneNode>();

        LaneNode ln = new LaneNode();

        ln.setWidth(0.0);
        ln.setX(0.0);
        ln.setY(0.0);
        ln.setZ(0.0);

        lnl.add(ln);

        ln = new LaneNode();

        ln.setWidth(0.0);
        ln.setX(0.0);
        ln.setY(0.0);
        ln.setZ(0.0);

        lnl.add(ln);

        l.setLaneGeomList(lnl);

        Map<Integer, LaneMovement> lmm = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();

        lm.setMovementId(0);
        lm.setMovement(LaneMovement.Movement.LEFT_TURN);

        lmm.put(lm.getMovementId(), lm);

        lm = new LaneMovement();

        lm.setMovementId(0);
        lm.setMovement(LaneMovement.Movement.LEFT_TURN);

        lmm.put(lm.getMovementId(), lm);

        l.setLaneMovements(lmm);

        ret.setLanes(retMap);

        return ret;
    }

    private static LaneManager genLane2() {
        LaneManager ret = new LaneManager();

        ret.setLatitude(0.0);
        ret.setLongitude(0.0);
        ret.setElevation(0.0);

        Map<Integer, Lane> retMap = new HashMap<Integer, Lane>();

        Lane l = new Lane();

        l.setLaneId(0);
        l.setApproachId(0);
        l.setType("UNSET");

        List<LaneNode> lnl = new ArrayList<LaneNode>();

        LaneNode ln = new LaneNode();

        ln.setWidth(0.0);
        ln.setX(0.0);
        ln.setY(0.0);
        ln.setZ(0.0);

        lnl.add(ln);

        ln = new LaneNode();

        ln.setWidth(0.0);
        ln.setX(0.0);
        ln.setY(0.0);
        ln.setZ(0.0);

        lnl.add(ln);

        l.setLaneGeomList(lnl);

        Map<Integer, LaneMovement> lmm = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();

        lm.setMovementId(0);
        lm.setMovement(LaneMovement.Movement.LEFT_TURN);

        lmm.put(lm.getMovementId(), lm);

        lm = new LaneMovement();

        lm.setMovementId(0);
        lm.setMovement(LaneMovement.Movement.LEFT_TURN);

        lmm.put(lm.getMovementId(), lm);

        l.setLaneMovements(lmm);

        ret.setLanes(retMap);

        return ret;
    }

    private static LaneManager genLane3() {
        LaneManager ret = new LaneManager();

        ret.setLatitude(0.0);
        ret.setLongitude(0.0);
        ret.setElevation(0.0);

        Map<Integer, Lane> retMap = new HashMap<Integer, Lane>();

        Lane l = new Lane();

        l.setLaneId(0);
        l.setApproachId(0);
        l.setType("UNSET");

        List<LaneNode> lnl = new ArrayList<LaneNode>();

        LaneNode ln = new LaneNode();

        ln.setWidth(0.0);
        ln.setX(0.0);
        ln.setY(0.0);
        ln.setZ(0.0);

        lnl.add(ln);

        ln = new LaneNode();

        ln.setWidth(0.0);
        ln.setX(0.0);
        ln.setY(0.0);
        ln.setZ(0.0);

        lnl.add(ln);

        l.setLaneGeomList(lnl);

        Map<Integer, LaneMovement> lmm = new HashMap<Integer, LaneMovement>();

        LaneMovement lm = new LaneMovement();

        lm.setMovementId(0);
        lm.setMovement(LaneMovement.Movement.LEFT_TURN);

        lmm.put(lm.getMovementId(), lm);

        lm = new LaneMovement();

        lm.setMovementId(0);
        lm.setMovement(LaneMovement.Movement.LEFT_TURN);

        lmm.put(lm.getMovementId(), lm);

        l.setLaneMovements(lmm);

        ret.setLanes(retMap);

        return ret;
    }

    private static DetectorManager genDetector1() {
        DetectorManager ret = new DetectorManager();

        Detector d = new Detector();

        d.setDetectorID(0);
        d.setLengthDetectCap(false);
        d.setPresenceDetectCap(false);
        d.setPulseDetectCap(false);
        d.setSpeedDetectCap(false);

        Polygon p = new Polygon();

        p.addPoint(0, 0);

        d.setArea(p);

        DetectorEvent de = new DetectorEvent();

        de.setDetectorId(0);
        de.setLength(0.0);
        de.setPresence(false);
        de.setPulse(0);
        de.setSpeed(0.0);

        d.setDetEvent(de);

        List<Integer> il = new ArrayList<Integer>();

        il.add(0);

        d.setLaneIDs(il);

        ret.addDetector(d.getDetectorID(), d);

        // ---------------------
        d = new Detector();

        d.setDetectorID(0);
        d.setLengthDetectCap(false);
        d.setPresenceDetectCap(false);
        d.setPulseDetectCap(false);
        d.setSpeedDetectCap(false);

        p = new Polygon();

        p.addPoint(0, 0);

        d.setArea(p);

        de = new DetectorEvent();

        de.setDetectorId(0);
        de.setLength(0.0);
        de.setPresence(false);
        de.setPulse(0);
        de.setSpeed(0.0);

        d.setDetEvent(de);

        il = new ArrayList<Integer>();

        il.add(0);

        d.setLaneIDs(il);

        ret.addDetector(d.getDetectorID(), d);

        return ret;
    }

    private static DetectorManager genDetector2() {
        DetectorManager ret = new DetectorManager();

        Detector d = new Detector();

        d.setDetectorID(0);
        d.setLengthDetectCap(false);
        d.setPresenceDetectCap(false);
        d.setPulseDetectCap(false);
        d.setSpeedDetectCap(false);

        Polygon p = new Polygon();

        p.addPoint(0, 0);

        d.setArea(p);

        DetectorEvent de = new DetectorEvent();

        de.setDetectorId(0);
        de.setLength(0.0);
        de.setPresence(false);
        de.setPulse(0);
        de.setSpeed(0.0);

        d.setDetEvent(de);

        List<Integer> il = new ArrayList<Integer>();

        il.add(0);

        d.setLaneIDs(il);

        ret.addDetector(d.getDetectorID(), d);

        // ---------------------
        d = new Detector();

        d.setDetectorID(0);
        d.setLengthDetectCap(false);
        d.setPresenceDetectCap(false);
        d.setPulseDetectCap(false);
        d.setSpeedDetectCap(false);

        p = new Polygon();

        p.addPoint(0, 0);

        d.setArea(p);

        de = new DetectorEvent();

        de.setDetectorId(0);
        de.setLength(0.0);
        de.setPresence(false);
        de.setPulse(0);
        de.setSpeed(0.0);

        d.setDetEvent(de);

        il = new ArrayList<Integer>();

        il.add(0);

        d.setLaneIDs(il);

        ret.addDetector(d.getDetectorID(), d);

        return ret;
    }

    private static DetectorManager genDetector3() {
        DetectorManager ret = new DetectorManager();

        Detector d = new Detector();

        d.setDetectorID(0);
        d.setLengthDetectCap(false);
        d.setPresenceDetectCap(false);
        d.setPulseDetectCap(false);
        d.setSpeedDetectCap(false);

        Polygon p = new Polygon();

        p.addPoint(0, 0);

        d.setArea(p);

        DetectorEvent de = new DetectorEvent();

        de.setDetectorId(0);
        de.setLength(0.0);
        de.setPresence(false);
        de.setPulse(0);
        de.setSpeed(0.0);

        d.setDetEvent(de);

        List<Integer> il = new ArrayList<Integer>();

        il.add(0);

        d.setLaneIDs(il);

        ret.addDetector(d.getDetectorID(), d);

        // ---------------------
        d = new Detector();

        d.setDetectorID(0);
        d.setLengthDetectCap(false);
        d.setPresenceDetectCap(false);
        d.setPulseDetectCap(false);
        d.setSpeedDetectCap(false);

        p = new Polygon();

        p.addPoint(0, 0);

        d.setArea(p);

        de = new DetectorEvent();

        de.setDetectorId(0);
        de.setLength(0.0);
        de.setPresence(false);
        de.setPulse(0);
        de.setSpeed(0.0);

        d.setDetEvent(de);

        il = new ArrayList<Integer>();

        il.add(0);

        d.setLaneIDs(il);

        ret.addDetector(d.getDetectorID(), d);

        return ret;
    }

    private static SignalManager genSignal1() {
        SignalManager ret = new SignalManager();

        SignalIndication si = new SignalIndication();

        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);

        SignalIndication si2 = new SignalIndication();

        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.FLASHING);
        si2.setTypeIndication(SignalIndication.Type.UTURN_ARROW);

        ret.addSignal(si);
        ret.addSignal(si2);

        return ret;
    }

    private static SignalManager genSignal2() {
        SignalManager ret = new SignalManager();

        SignalIndication si = new SignalIndication();

        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);

        SignalIndication si2 = new SignalIndication();

        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.FLASHING);
        si2.setTypeIndication(SignalIndication.Type.UTURN_ARROW);

        ret.addSignal(si);
        ret.addSignal(si2);

        return ret;
    }

    private static SignalManager genSignal3() {
        SignalManager ret = new SignalManager();

        SignalIndication si = new SignalIndication();

        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);

        SignalIndication si2 = new SignalIndication();

        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.FLASHING);
        si2.setTypeIndication(SignalIndication.Type.UTURN_ARROW);

        ret.addSignal(si);
        ret.addSignal(si2);

        return ret;
    }
}
