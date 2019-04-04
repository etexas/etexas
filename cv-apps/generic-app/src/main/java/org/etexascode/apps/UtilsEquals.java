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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorEvent;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsSpecialEquals;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Contains methods to compare datamodel objects for equality, under relaxed conditions: If a field
 * isn't conveyed through a J2735 message, then it isn't required to be equal. Certain fields have
 * greater tolerances, such as centimeter distances calculated from latitude and longitude.
 * 
 * @author janway
 * @author ablatt
 */
public class UtilsEquals {

    /**
     * Static logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(UtilsEquals.class);

    // TODO: janway - need a more scientific way to get these than "seems appropriate"
    /**
     * The tolerance to use for latitude values (needs to be more precise than other doubles).
     */
    public static final double LATITUDE_TOLERANCE = 0.000005;

    /**
     * The tolerance to use for longitude values (needs to be more precise than other doubles).
     */
    public static final double LONGITUDE_TOLERANCE = LATITUDE_TOLERANCE;

    /**
     * The tolerance to use for centimeter offsets based on values calculated from lat/long.
     */
    public static final double CM_OFFSET_TOLERANCE = 2;

    /**
     * The tolerance to use for normal double equality checking.
     */
    public static final double DOUBLE_TOLERANCE = 0.00005;

    // ------- LaneNode -------
    /**
     * Determines if two lane nodes are close enough to equal.
     * 
     * @param ln1 The first lane node.
     * @param ln2 The second lane node.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(ILaneNode ln1, ILaneNode ln2) {
        if ((ln1 == null) || (ln2 == null)) {
            return ln1 == ln2;
        }
        if (Math.abs(ln2.getX() - ln1.getX()) > CM_OFFSET_TOLERANCE)
            return false;
        if (Math.abs(ln2.getY() - ln1.getY()) > CM_OFFSET_TOLERANCE)
            return false;
        if (Math.abs(ln2.getZ() - ln1.getZ()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(ln2.getWidth() - ln1.getWidth()) > DOUBLE_TOLERANCE)
            return false;
        return true;
    }

    // ------- Lane -------
    /**
     * Determines if two lanes are close enough to equal.
     * 
     * @param l1 The first lane.
     * @param l2 The second lane.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(ILane l1, ILane l2) {
        if ((l1 == null) || (l2 == null)) {
            return l1 == l2;
        }
        if (l1.getLaneId() != l2.getLaneId())
            return false;
        if (l1.getApproachId() != l2.getApproachId())
            return false;

        List<? extends ILaneNode> geom1 = l1.getLaneGeomList();
        List<? extends ILaneNode> geom2 = l2.getLaneGeomList();
        if ((geom1 == null || geom2 == null)) {
            if (geom1 != geom2)
                return false;
        }
        else {
            if (geom1.size() != geom2.size())
                return false;
            for (int k = 0; k < geom1.size(); ++k) {
                if (!closelyEquals(geom1.get(k), geom2.get(k)))
                    return false;
            }
        }

        // TODO: janway - movement information seems to be put into the messages, but not taken out
        // on the receiving end.
        // Set<Integer> movementIds1 = l1.getLaneMovementIds();
        // Set<Integer> movementIds2 = l2.getLaneMovementIds();
        // if (!movementIds1.containsAll(movementIds2) || movementIds2.containsAll(movementIds1))
        // return false;
        // for (Integer id : movementIds1) {
        // if (!closelyEquals(l1.getLaneInfoById(id), l2.getLaneInfoById(id)))
        // return false;
        // }
        //
        // if ((l1.getType() == null || l2.getType() == null)) {
        // if (l1.getType() != l2.getType())
        // return false;
        // } else {
        // if (!l1.getType().equals(l2.getType()))
        // return false;
        // }

        // MapData messages don't have speed limit information?
        // if (Math.abs(l1.getSpeedLimitInMetersPerSecond() - l2.getSpeedLimitInMetersPerSecond()) >
        // DOUBLE_TOLERANCE)
        // return false;

        return true;
    }

    // ------- LaneManager -------
    /**
     * Determines if two lane managers are close enough to equal.
     * 
     * @param lm1 The first lane manager.
     * @param lm2 The second lane manager.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(ILaneManager lm1, ILaneManager lm2) {
        if ((lm1 == null) || (lm2 == null)) {
            return lm1 == lm2;
        }
        if (Math.abs(lm1.getLatitude() - lm2.getLatitude()) > LATITUDE_TOLERANCE)
            return false;
        if (Math.abs(lm1.getLongitude() - lm2.getLongitude()) > LONGITUDE_TOLERANCE)
            return false;
        // the MapData message Position3D objects have an elevation field, but we aren't using it.
        // if (Math.abs(lm1.getElevation() - lm2.getElevation()) > DOUBLE_TOLERANCE)
        // return false;

        Set<Integer> laneIds1 = lm1.getLaneIds();
        Set<Integer> laneIds2 = lm2.getLaneIds();
        if (!laneIds1.containsAll(laneIds2) || !laneIds2.containsAll(laneIds1))
            return false;
        for (Integer id : laneIds1) {
            if (!closelyEquals(lm1.getLaneById(id), lm2.getLaneById(id)))
                return false;
        }

        // TODO: janway - should two LMs be considered different if they have different calculator
        // types?
        // if (lm1.getGeoCalculatorType() != lm2.getGeoCalculatorType())
        // return false;
        return true;
    }

    // ------- LaneMovement -------
    /**
     * Determines if two lane movements are close enough to equal.
     * 
     * @param m1 The first lane movement.
     * @param m2 The second lane movement.
     * @return True if each parameter falls with the tolerance, false otherwise.
     */
    public static boolean closelyEquals(ILaneMovement m1, ILaneMovement m2) {
        return (m1.getMovementId() == m2.getMovementId()) && (m1.getMovement() == m2.getMovement());
    }

    // ------- SignalIndication -------
    /**
     * Determines if two signal indications are close enough to equal.
     * 
     * @param si1 The first signal indication.
     * @param si2 The second signal indication.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(ISignalIndication si1, ISignalIndication si2) {
        if ((si1 == null) || (si2 == null)) {
            return si1 == si2;
        }
        return (si1.getLaneId() == si2.getLaneId()) && (si1.getColorIndication() == si2.getColorIndication()) && (si1.getStateIndication() == si2.getStateIndication())
                && (si1.getTypeIndication() == si2.getTypeIndication()) && (Math.abs(si1.getTimeToChange() - si2.getTimeToChange()) < DOUBLE_TOLERANCE);
    }

    // ------- SignalManager -------
    /**
     * Determines if two signal managers are close enough to equal.
     * 
     * @param sm1 The first signal manager.
     * @param sm2 The second signal manager.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(ISignalManager sm1, ISignalManager sm2) {
        if ((sm1 == null || sm2 == null)) {
            return sm1 == sm2;
        }

        Set<Integer> sigM1 = sm1.keysForSigMap();
        Set<Integer> sigM2 = sm2.keysForSigMap();

        if (sigM1.size() != sigM2.size())
            return false;
        if (sigM1.containsAll(sigM2)) {
            for (Integer i : sigM1) {
                List<? extends ISignalIndication> sil1 = sm1.getSignalsByLaneId(i);
                List<? extends ISignalIndication> sil2 = sm2.getSignalsByLaneId(i);

                if (sil1.size() != sil2.size()) {
                    return false;
                }
                else {
                    // Checks that the two lists have all the same indications, but not
                    // necessarily in the same order.
                    // sil2 contains all of sil1

                    for (int k = 0; k < sil1.size(); ++k) {
                        boolean matched = false;
                        for (int j = 0; j < sil2.size(); ++j) {
                            if (closelyEquals(sil1.get(k), sil2.get(j)))
                                matched = true;
                        }
                        if (!matched)
                            return false;
                    }
                    // sil1 contains all of sil2
                    for (int k = 0; k < sil2.size(); ++k) {
                        boolean matched = false;
                        for (int j = 0; j < sil1.size(); ++j) {
                            if (closelyEquals(sil1.get(j), sil2.get(k)))
                                matched = true;
                        }
                        if (!matched)
                            return false;
                    }
                }
            }
        }
        else {
            return false;
        }
        return true;
    }

    // ------- Vehicle -------
    /**
     * Determines if two vehicles are close enough to equal.
     * 
     * @param v1 The first vehicle.
     * @param v2 The second vehicle.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(IVehicle v1, IVehicle v2) {
        if ((v1 == null) || (v2 == null))
            return v1 == v2;
        if (v1.getVehicleID() != v2.getVehicleID())
            return false;
        // During parsing of a BSM we already assign the lane, so I think it could be checked here.
        if (v1.getLaneID() != v2.getLaneID())
            return false;
        if (Math.abs(v1.getX() - v2.getX()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(v1.getY() - v2.getY()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(v1.getZ() - v2.getZ()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(v1.getSpeed() - v2.getSpeed()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(v1.getLength() - v2.getLength()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(v1.getWidth() - v2.getWidth()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(v1.getHeading() - v2.getHeading()) > DOUBLE_TOLERANCE)
            return false;

        return true;
    }

    // ------- VehicleManager -------
    /**
     * Determines if two vehicle managers are close enough to equal.
     * 
     * @param vm1 The first vehicle manager.
     * @param vm2 The second vehicle manager.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(IVehicleManager vm1, IVehicleManager vm2) {
        if ((vm1 == null) || (vm2 == null)) {
            return vm1 == vm2;
        }

        // if (Math.abs(vm1.getCurrentMessageTime() - vm2.getCurrentMessageTime()) >
        // DOUBLE_TOLERANCE)
        // return false;

        // TODO: janway - do different calculator types cause inequality?
        // if (vm1.getGeoCalculatorType() != vm2.getGeoCalculatorType())
        // return false;

        Set<String> vehicleIds1 = vm1.getAllVehicleIds();
        Set<String> vehicleIds2 = vm2.getAllVehicleIds();
        if (!vehicleIds1.containsAll(vehicleIds2) || !vehicleIds2.containsAll(vehicleIds1))
            return false;
        for (String id : vehicleIds1) {
            if (!closelyEquals(vm1.getVehicle(id), vm2.getVehicle(id))) {
                return false;
            }
        }

        return true;
    }

    // ------- DetectorEvent -------
    /**
     * Determines if two detector events are close enough to equal.
     * 
     * @param de1 The first detector event.
     * @param de2 The second detector event.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(IDetectorEvent de1, IDetectorEvent de2) {
        if ((de1 == null) || (de2 == null)) {
            return de1 == de2;
        }
        if ((de1.getDetectorId() != de2.getDetectorId()) || (de1.getPulse() != de2.getPulse()) || (de1.isPresence() != de2.isPresence()))
            return false;
        if (Math.abs(de1.getLength() - de2.getLength()) > DOUBLE_TOLERANCE)
            return false;
        if (Math.abs(de1.getSpeed() - de2.getSpeed()) > DOUBLE_TOLERANCE)
            return false;
        return true;
    }

    // ------- Detector -------
    /**
     * Determines if two detectors are close enough to equal.
     * 
     * @param d1 The first detector.
     * @param d2 The second detector.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(IDetector d1, IDetector d2) {
        if ((d1 == null) || (d2 == null)) {
            return d1 == d2;
        }
        if (d1.getDetectorID() != d2.getDetectorID())
            return false;
        if (!((d1.isLengthDetectCap() == d2.isLengthDetectCap()) && (d1.isPresenceDetectCap() == d2.isPresenceDetectCap()) && (d1.isPulseDetectCap() == d2.isPulseDetectCap()) && (d1
                .isSpeedDetectCap() == d2.isSpeedDetectCap())))
            return false;

        if ((d1.getDetEvent() == null) || (d2.getDetEvent() == null)) {
            if (d1.getDetEvent() != d2.getDetEvent())
                return false;
        }
        else {
            if (!closelyEquals(d1.getDetEvent(), d2.getDetEvent()))
                return false;
        }

        Set<Integer> laneIds1 = new HashSet<Integer>(d1.getLaneIDs());
        Set<Integer> laneIds2 = new HashSet<Integer>(d2.getLaneIDs());

        if (!laneIds1.containsAll(laneIds2) || !laneIds2.containsAll(laneIds1))
            return false;

        if ((d1.getArea() == null) || (d2.getArea() == null)) {
            if (d1.getArea() != d2.getArea())
                return false;
        }
        else {
            if (!UtilsSpecialEquals.equals(d1.getArea(), d2.getArea()))
                return false;
        }

        return true;
    }

    // ------- DetectorManager -------
    /**
     * Determines if two detector managers are close enough to equal.
     * 
     * @param dm1 The first detector manager.
     * @param dm2 The second detector manager.
     * @return True if each parameter falls within the tolerance, false otherwise.
     */
    public static boolean closelyEquals(IDetectorManager dm1, IDetectorManager dm2) {
        if ((dm1 == null) || (dm2 == null)) {
            return dm1 == dm2;
        }
        Set<Integer> ids1 = dm1.getKeys();
        Set<Integer> ids2 = dm2.getKeys();
        if (!ids1.containsAll(ids2) || !ids2.containsAll(ids1))
            return false;

        for (Integer id : ids1) {
            if (!closelyEquals(dm1.getDetector(id), dm2.getDetector(id)))
                return false;
        }

        return true;
    }
}
