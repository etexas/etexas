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

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.BSM;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.Intersection;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.MapData.Intersections;
import org.etexascode.j2735.Position3D;
import org.etexascode.j2735.SPAT;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utilities for converting messages into their respective data types. TODO ttevendale 1/4/2018 look
 * into deleting this class since we'll be using the new spec which is quite a bit different
 * 
 * @author ablatt
 */
public class UtilsMessageImports {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(UtilsMessageImports.class);

    /**
     * Converts a BSM into a Vehicle. GeoCaclType options: Geodetic 2D: 1 Sherical: 2 Geodetic 3D: 3
     * Cartesian: 4
     * 
     * @param data The basic safety message data.
     * @param manager The LaneManager the Vehicle will be placed into.
     * @param geoCalcType The type of calculator to use. Default to 1 if you are not sure.
     * @return The vehicle.
     */
    public static Vehicle convertBSMToVehicle(BSM data, LaneManager manager, int geoCalcType) {
        double[] offsets = UtilsLatLongConversion.convertLatLongToCentimeterOffset(manager.getLatitude(), manager.getLongitude(), data.getLatitude(), data.getLongitude(), geoCalcType);
        // Changes to conform to new vehicle constructor
        Vehicle ret = new Vehicle(data.getTempID(), data.getVehicleLength(), data.getVehicleWidth(), (Math.round(offsets[0])), (Math.round(offsets[1])), data.getElevation());

        // elevation in bsmData's elevation is measured in decimeters

        if (offsets[1] > 0 && offsets[1] < 1.0) {
            ret.setY(Math.floor(offsets[1]));
        }
        else if (offsets[1] > 0) {
            ret.setY(Math.ceil(offsets[1]));
        }
        else if (offsets[1] < 0) {

            ret.setY(Math.floor(offsets[1]));
        }

        ret.setSpeed(data.getSpeed());

        // This is to resolve an issue that the acceleration never gets set from the BSM and is only
        // set into the Longitude Acceleration

        ret.setAcceleration((double)data.getLongitudeAcceleration());
        UtilsInterRep.getLaneAssignment(ret, manager);
        ret.setHeading(UtilsUnitConversion.convertBSMHeadingToDegrees(data.getHeading()));

        ret.setLatitude(data.getLatitude());
        ret.setLongitude(data.getLongitude());
        ret.setElev(data.getElevation());

        return ret;
    }

    /**
     * Converts a BasicSafetyMessage into a Vehicle. GeoCaclType options: Geodetic 2D: 1 Sherical: 2
     * Geodetic 3D: 3 Cartesian: 4
     * 
     * @param bsm The basic safety message.
     * @param manager The LaneManager the Vehicle will be placed into.
     * @param geoCalcType The type of calculator to use. Default to 1 if you are not sure.
     * @return The vehicle.
     */
    public static Vehicle importBSM(BasicSafetyMessage bsm, LaneManager manager, int geoCalcType) {
        BSM bsm2 = UtilsBSMData.getBSMData(bsm);
        return convertBSMToVehicle(bsm2, manager, geoCalcType);
    }

    /**
     * Converts a BasicSafetyMessageVerbose into a Vehicle. GeoCaclType options: Geodetic 2D: 1
     * Sherical: 2 Geodetic 3D: 3 Cartesian: 4
     * 
     * @param bsmv The basic safety message verbose.
     * @param manager The LaneManager the Vehicle will be placed into.
     * @param geoCalcType The type of calculator to use. Default to 1 if you are not sure.
     * @return The vehicle.
     */
    public static Vehicle importBSMV(BasicSafetyMessageVerbose bsmv, LaneManager manager, int geoCalcType) {
        BSM bsm2 = UtilsBSMData.getBSMVerboseData(bsmv);
        return convertBSMToVehicle(bsm2, manager, geoCalcType);
    }

    /**
     * Convert a MapData message into a list of the lanes described by the MapData message.
     * 
     * @param data The map data message
     * @return The list of lanes.
     */
    public static List<Lane> importLanes(MapData data) {
        return MapDataUtil.getLaneVectorsFromMapData(data);
    }

    /**
     * Get the reference coordinate (Lat, Long) from a MapData message.
     * 
     * @param data The map data message
     * @return The coordinates.
     */
    public static double[] importLatLong(MapData data) {
        double[] ret = new double[2];
        Intersections intersections = data.getIntersections();
        List<Intersection> intersectionList = intersections.getIntersection();
        // Iterate over the the intersections represented by this message.
        Iterator<Intersection> intersectionIterator = intersectionList.iterator();
        while (intersectionIterator.hasNext()) {
            Intersection intersection = intersectionIterator.next();

            Position3D ref = intersection.getRefPoint();
            ret[0] = UtilsUnitConversion.convertToDegrees(ref.getLat());
            ret[1] = UtilsUnitConversion.convertToDegrees(ref.getLong());
        }

        return ret;
    }

    /**
     * Convert a SPAT data message into a list of signals described by the SPAT message.
     * 
     * @param data The signal phase and timing message.
     * @return The list of signal indications.
     */

    public static List<SignalIndication> importSignal(SPAT data) {
        return SpatDataUtil.getSignalInfoFromSpat(data);

    }

    /**
     * Takes a set of LaneManagers that each represent a "view" of the intersection (from different
     * reference points) and combines them.
     * 
     * @param lms The LaneManagers.
     * @param geoCalcType The type of calculator to use when dealing with latitude and longitude.
     * @return The combined LaneManager.
     */
    private static LaneManager stitchLaneManagers(List<LaneManager> lms, int geoCalcType) {
        LaneManager ret = new LaneManager();
        // Set ret's reference point to that of the first manager in the list
        double refLat = lms.get(0).getLatitude();
        double refLong = lms.get(0).getLongitude();
        ret.setLatitude(refLat);
        ret.setLongitude(refLong);

        Map<Integer, List<Lane>> lanesMap = new HashMap<Integer, List<Lane>>();
        for (LaneManager lm : lms) {
            // Convert manager to use ret's reference point
            centerLaneManagerOnReferencePoint(lm, refLat, refLong, geoCalcType);

            // Find all lanes with the same id and put them together in a list
            for (Entry<Integer, Lane> entry : lm.getLanes().entrySet()) {
                if (!lanesMap.containsKey(entry.getKey())) {
                    lanesMap.put(entry.getKey(), new ArrayList<Lane>());
                }
                lanesMap.get(entry.getKey()).add(entry.getValue());
            }
        }

        // apply stitchLanes to each list and put the result in ret
        Map<Integer, Lane> lanes = ret.getLanes();
        for (Entry<Integer, List<Lane>> entry : lanesMap.entrySet()) {
            lanes.put(entry.getKey(), stitchLanes(entry.getValue()));
        }

        return ret;
    }

    /**
     * Takes a list of Lanes that each represent the same lane but from different reference points.
     * Attempts to recreate the original lane.
     * 
     * @param lanes Representations of the lane as it's seen from different reference points.
     * @return The recreated lane.
     */
    private static Lane stitchLanes(List<Lane> lanes) {
        Lane ret = new Lane(lanes.get(0));

        List<LaneNode> newGeomList = new LinkedList<LaneNode>();

        // keeps track of the next unprocessed node for each lane
        Map<Integer, Integer> candidates = new HashMap<Integer, Integer>();
        for (int k = 0; k < lanes.size(); ++k) {
            candidates.put(k, 0);
        }

        // loop until every node is either inserted or discarded
        while (!candidates.isEmpty()) {
            int selection = -1;

            // make selection and update list
            if (newGeomList.isEmpty()) {
                selection = getKeyOfClosest(0, 0, candidates, lanes);
                newGeomList.add(lanes.get(selection).getLaneGeomList().get(candidates.get(selection)));
            }
            else if ((selection = getKeyOfEquivalent(newGeomList.get(newGeomList.size() - 1), candidates, lanes)) != -1) {
                // duplicate node. do nothing.
            }
            else if ((selection = getKeyOfClosestOnLine(newGeomList, candidates, lanes)) != -1) {
                LaneNode prevNode = newGeomList.get(newGeomList.size() - 1);
                if (prevNode.getWidth() == lanes.get(selection).getLaneGeomList().get(candidates.get(selection)).getWidth()) {
                    newGeomList.remove(newGeomList.size() - 1);
                }
                newGeomList.add(lanes.get(selection).getLaneGeomList().get(candidates.get(selection)));
            }
            else {
                LaneNode prevNode = newGeomList.get(newGeomList.size() - 1);
                selection = getKeyOfClosest(prevNode.getX(), prevNode.getY(), candidates, lanes);
                if (selection == -1) {
                    throw new RuntimeException("Something went horribly wrong.");
                }
                newGeomList.add(lanes.get(selection).getLaneGeomList().get(candidates.get(selection)));
            }

            // update candidates
            candidates.put(selection, candidates.get(selection) + 1);
            if (candidates.get(selection) >= lanes.get(selection).getLaneGeomList().size()) {
                candidates.remove(selection);
            }
        }

        ret.setLaneGeomList(newGeomList);
        return ret;
    }

    /**
     * Helper function for stitch lanes. Finds a candidate node that matches another node, if any.
     * 
     * @param prevNode The node to match.
     * @param candidates The nodes to check for matches.
     * @param lanes The lanes that contain the nodes.
     * @return The index of a match if found, -1 if not.
     */
    private static int getKeyOfEquivalent(LaneNode prevNode, Map<Integer, Integer> candidates, List<Lane> lanes) {
        for (Entry<Integer, Integer> entry : candidates.entrySet()) {
            if (UtilsEquals.closelyEquals(prevNode, lanes.get(entry.getKey()).getLaneGeomList().get(entry.getValue()))) {
                return entry.getKey();
            }
        }
        return -1;
    }

    /**
     * Helper function for stitch lanes. Finds the closest candidate node to a given point.
     * 
     * @param x The x-coordinate of the point.
     * @param y The y-coordinate of the point.
     * @param candidates The list of candidates.
     * @param lanes The lanes that contain the nodes.
     * @return The index of the closest candidate.
     */
    private static int getKeyOfClosest(double x, double y, Map<Integer, Integer> candidates, List<Lane> lanes) {
        double minDist = Double.MAX_VALUE;
        int index = -1;
        for (Entry<Integer, Integer> entry : candidates.entrySet()) {
            LaneNode candidate = lanes.get(entry.getKey()).getLaneGeomList().get(entry.getValue());
            double dist = UtilsCalculations.getDist(x, y, candidate.getX(), candidate.getY());
            if ((dist < minDist) || (index == -1)) {
                minDist = dist;
                index = entry.getKey();
            }
        }
        return index;
    }

    /**
     * Helper function for stitch lanes. Finds the closest candidate node that's collinear with the
     * previous two selected candidates.
     * 
     * @param newGeomList The lane-in-progess that has the previously selected nodes.
     * @param candidates The list of candidates.
     * @param lanes The lanes that contain the nodes.
     * @return The index of the closest collinear candidate, if any. -1 otherwise.
     */
    private static int getKeyOfClosestOnLine(List<LaneNode> newGeomList, Map<Integer, Integer> candidates, List<Lane> lanes) {
        if (newGeomList.size() < 2) {
            return -1;
        }

        LaneNode ln1 = newGeomList.get(newGeomList.size() - 2);
        LaneNode ln2 = newGeomList.get(newGeomList.size() - 1);

        Map<Integer, Integer> candidatesOnLine = new HashMap<Integer, Integer>();
        for (Entry<Integer, Integer> entry : candidates.entrySet()) {
            LaneNode ln3 = lanes.get(entry.getKey()).getLaneGeomList().get(entry.getValue());

            double tolerance = 0.00005 * Collections.max(Arrays.asList(Math.abs(ln1.getX()), Math.abs(ln1.getY()), Math.abs(ln2.getX()), Math.abs(ln2.getY()), Math.abs(ln3.getX()),
                    Math.abs(ln3.getY())));
            if (Math.abs((ln1.getY() - ln2.getY()) * (ln1.getX() - ln3.getX()) - // checks if 3
                                                                                 // points are
                                                                                 // collinear
                    (ln1.getY() - ln3.getY()) * (ln1.getX() - ln2.getX())) < tolerance) { // (same
                                                                                          // slope
                                                                                          // between
                                                                                          // 2
                                                                                          // pairs)
                candidatesOnLine.put(entry.getKey(), entry.getValue());
            }
        }

        return getKeyOfClosest(ln2.getX(), ln2.getY(), candidatesOnLine, lanes);
    }

    /**
     * Converts an Intersection object to a LaneManager.
     * 
     * @param intersection The Intersection to convert.
     * @return The resulting LaneManager.
     */
    private static LaneManager parseIntersection(Intersection intersection) {
        List<Lane> lanes = MapDataUtil.getLaneVectorsFromIntersectionObject(intersection);
        LaneManager ret = new LaneManager();

        ret.setLatitude(UtilsUnitConversion.convertToDegrees(intersection.getRefPoint().getLat()));
        ret.setLongitude(UtilsUnitConversion.convertToDegrees(intersection.getRefPoint().getLong()));

        Map<Integer, Lane> lanesMap = ret.getLanes();
        for (Lane l : lanes) {
            lanesMap.put(l.getLaneId(), l);
        }

        return ret;
    }

    /**
     * Converts a MapData message to a LaneManager.
     * 
     * @param msg The message to convert.
     * @param geoCalcType The geographic calculator type.
     * @return The resulting LaneManager.
     */
    public static LaneManager parseMapDataMessage(MapData msg, int geoCalcType) {

        List<LaneManager> lms = new LinkedList<LaneManager>();

        // Convert the list of Intersections to a list of LaneManagers
        MapData.Intersections intersections = msg.getIntersections();
        for (Intersection intersection : intersections.getIntersection()) {
            lms.add(parseIntersection(intersection));
        }

        return stitchLaneManagers(lms, geoCalcType);
    }

    /**
     * Converts a MapData message to a LaneManager, using a default geoCalculatorType of GEODETIC2D.
     * 
     * @param msg The message to convert.
     * @return The resulting LaneManager.
     */
    public static LaneManager parseMapDataMessage(MapData msg) {
        return parseMapDataMessage(msg, UtilsLatLongConversion.GEODETIC2D);
    }

    /**
     * Gets the intersection ID from a MapData message.
     * 
     * @param msg The message.
     * @return The intersection ID.
     */
    public int getIntersectionIdFromMapData(MapData msg) {
        MapData.Intersections intersections = msg.getIntersections();
        List<Intersection> inters = intersections.getIntersection();
        if (inters.size() > 0) {
            Intersection inter = inters.get(0);
            return ByteBuffer.wrap(inter.getId()).getInt();
        }

        return 0;
    }

    /**
     * Shifts a LaneManager to be relative to a given reference point.
     * 
     * @param lm The LaneManager to shift.
     * @param refLat The new latitude.
     * @param refLong The new longitude.
     * @param geoCalcType The type of calculator to use for latitude/longitude operations.
     */
    public static void centerLaneManagerOnReferencePoint(LaneManager lm, double refLat, double refLong, int geoCalcType) {
        double[] offsets = UtilsLatLongConversion.convertLatLongToCentimeterOffset(refLat, refLong, lm.getLatitude(), lm.getLongitude(), geoCalcType);
        double dx = Math.round(offsets[0] * 1000) / 1000.0;
        double dy = Math.round(offsets[1] * 1000) / 1000.0; // this truncation should match what's
                                                            // in the MapData producer

        for (Lane l : lm.getLanes().values()) {
            for (LaneNode ln : l.getLaneGeomList()) {
                ln.setX(ln.getX() + dx);
                ln.setY(ln.getY() + dy);
            }
        }

        lm.setLatitude(refLat);
        lm.setLongitude(refLong);
    }

}
