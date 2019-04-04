/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */

package org.etexascode.api;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SimMetaData;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;

/**
 * A class which contains utility methods for creating and populating managers based on the data in
 * the TEXAS model. The important methods in the class are populateDetectorManager(),
 * populateSignalManager() and populateLaneManager(ModelData modelData).
 * 
 * @author ablatt
 */
public class UtilsStaticData {

    /**
     * Create and populate a detector manager.
     * 
     * @return The populated detector manager.
     */
    public static DetectorManager populateDetectorManager() {
        DetectorManager detMan = new DetectorManager();

        return detMan;
    }

    /**
     * Create and populate a lane manager. The lane manager is most analogous to the map data
     * message.
     * 
     * @param modelData The model data.
     * @param mapDataWrapper The map data wrapper.
     * @return The populated lane manager.
     */
    public static LaneManager populateLaneManager(ModelData modelData, MapDataRetriever mapDataWrapper) {
        LaneManager lanMan = new LaneManager();

        lanMan.setLatitude(modelData.getLatitude());
        lanMan.setLongitude(modelData.getLongitude());
        lanMan.setElevation(0); // TODO: ablatt - put the correct elevation here
                                // when that information becomes available

        Map<Integer, Lane> lanes = lanMan.getLanes();

        int numLanes = mapDataWrapper.getNRLAN();
        double centerx = modelData.getCenterX();
        double centery = modelData.getCenterY();
        double minX = mapDataWrapper.getBASELX(1) - centerx;
        double maxX = minX;
        double minY = mapDataWrapper.getBASELY(1) - centery;
        double maxY = minY;
        double xMidpoint;
        double yMidpoint;

        // populate minX, maxX, minY, maxY
        for (int i = 2; i <= numLanes; i++) {
            double x = mapDataWrapper.getBASELX(i) - centerx;
            double y = mapDataWrapper.getBASELY(i) - centery;

            if (x < minX) {
                minX = x;
            }
            else if (x > maxX) {
                maxX = x;
            }

            if (y < minY) {
                minY = y;
            }
            else if (y > maxY) {
                maxY = y;
            }
        }

        // populate midpoints
        xMidpoint = UtilsUnitConversion.convertFeetToCentimeters((minX + maxX) / 2);
        yMidpoint = UtilsUnitConversion.convertFeetToCentimeters((minY + maxY) / 2);

        // cdeisher - moved from lane to lanemanger for efficiency
        // populate and convert LGEOM list for all lanes
        int[] lGeom = mapDataWrapper.getLGEOM();
        for (int j = 0; j < lGeom.length; j++) {
            lGeom[j] = (int)UtilsUnitConversion.convertFeetToCentimeters((double)lGeom[j]);
        }

        for (int i = 1; i <= numLanes; i++) {
            double[] edges = new double[] { UtilsUnitConversion.convertFeetToCentimeters(mapDataWrapper.getBASELX(i) - modelData.getCenterX()),
                    UtilsUnitConversion.convertFeetToCentimeters(mapDataWrapper.getBASELY(i) - modelData.getCenterY()),
                    UtilsUnitConversion.convertFeetToCentimeters(mapDataWrapper.getENDLNX(i) - modelData.getCenterX()),
                    UtilsUnitConversion.convertFeetToCentimeters(mapDataWrapper.getENDLNY(i) - modelData.getCenterY()) };
            Lane l = populateLane(i, mapDataWrapper, edges, xMidpoint, yMidpoint, lGeom);
            lanes.put(i, l);
        }

        return lanMan;
    }

    public static SimMetaData populateMetaData(ModelData model) {
        SimMetaData ret = new SimMetaData();

        ret.setMaxSteps(model.getMaxDT());
        ret.setStepSize(model.getDTSize());
        ret.setSimHeight(model.getYMax() - model.getYMin());
        ret.setSimWidth(model.getXMax() - model.getXMin());
        ret.setFirstStep(1);

        return ret;
    }

    /**
     * Create and populate a single lane on the map.
     * 
     * @param laneID The id of that lane.
     * @param mapDataWrapper The means of obtaining data on the lane from TEXAS
     * @param edges The edges of the lane
     * @param xMidpoint
     * @param yMidpoint
     * @return The populated lane
     */
    static Lane populateLane(int laneID, MapDataRetriever mapDataWrapper, double[] edges, double xMidpoint, double yMidpoint, int[] lGeom) {
        Lane ret = new Lane();

        ret.setLaneId(laneID);
        int approachID = mapDataWrapper.getISNA(laneID);
        int legID = mapDataWrapper.getLIBAR(approachID);

        ret.setApproachId(approachID);
        ret.setType(Lane.INBOUND);

        if (legID <= 0) {
            legID = mapDataWrapper.getLOBAR(approachID);
            ret.setType(Lane.OUTBOUND);
        }

        // Calculate the number of lanes
        // from the number of speed limits returned.
        // This will depend on the number of inbound and outbound.
        // lanes. Ex. 16 lanes will produce 8 speed limits.
        // If there are 2 inbound and 2 outbound lanes per leg.
        int lsl[] = mapDataWrapper.getISLIM();
        int numLs = numLanesFromSpeedLimit(lsl);

        // Get the speed limit and convert it from
        // ft/s to m/s.
        ret.setSpeedLimitInMetersPerSecond(getSpeedLimit(Lane.INBOUND, ret.getType(), lsl, numLs, legID));

        int width = (int)UtilsUnitConversion.convertFeetToCentimeters((double)mapDataWrapper.getLWID(laneID));

        modifyEdgesForTurningLane(edges, lGeom, laneID, xMidpoint, yMidpoint);

        ret.setLaneGeomList(convertEdgesToNodes(edges, width));

        // TODO: ablatt - add code for identifying which lanes the current lane
        // connects to

        // Parses the movement code from texas into LaneMovement objects
        int movementCode = mapDataWrapper.getLTURN(laneID);
        ret.setLaneMovements(parseMovementCode(movementCode));
        // if (ret.getLaneId() == 1)
        // {
        // ret.getLaneGeomList().get(0).setY(ret.getLaneGeomList().get(0).getY()
        // + 610);
        // }
        if (ret.getType() == Lane.INBOUND) {
            LaneNode last = ret.getLaneGeomList().get(ret.getLaneGeomList().size() - 1);
            if (edges[1] == edges[3]) {
                // Horizontal
                if (edges[0] < xMidpoint) {
                    last.setX(last.getX() - 610);
                }
                else {

                    last.setX(last.getX() + 610);
                }
            }
            else if (edges[0] == edges[2]) {
                // Vertical
                if (edges[1] > yMidpoint) {
                    last.setY(last.getY() + 610);
                }
                else {
                    last.setY(last.getY() - 610);
                }
            }
        }

        return ret;
    }

    static double getSpeedLimit(String inboundStr, String laneType, int[] lsl, int numLs, int legID) {
        if (inboundStr.equals(laneType)) {
            return UtilsUnitConversion.convertFeetToMeters(lsl[legID - 1]);
        }
        else {
            return UtilsUnitConversion.convertFeetToMeters(lsl[(numLs / 2) + legID - 1]);
        }
    }

    static void modifyEdgesForTurningLane(double[] edges, int[] laneGeometry, int laneID, double xMidpoint, double yMidpoint) {
        // Gets the current lane (i) and multiplies by 4 due to how a
        // multi-dimensional array is converted from FORTRAN mapdata
        int geomIndex = (laneID - 1) * 4;
        boolean b1 = laneGeometry[geomIndex] == laneGeometry[geomIndex + 2];
        boolean b2 = laneGeometry[geomIndex + 1] == laneGeometry[geomIndex + 3];

        if (b1 && b2) {
            // Leaves the lane path alone, which has no merge area or turning
            // lane
        }
        else if ((laneGeometry[geomIndex] == laneGeometry[geomIndex + 1]) && !(laneGeometry[geomIndex + 2] == laneGeometry[geomIndex + 3])) {
            // There is a turning lane, so parse it
            parseTurningLane(edges, laneGeometry, geomIndex, xMidpoint, yMidpoint);
        }
        else {
            parseMergingLane(edges, laneGeometry, geomIndex, xMidpoint, yMidpoint);
        } // TODO: ablatt - what to do if the there is both a turning lane and a
          // merge section? or is that not possible?

    }

    static int numLanesFromSpeedLimit(int[] lsl) {
        for (int i = 0; i < lsl.length; i++) {
            if (lsl[i] == 0) {
                return i;
            }
        }

        return 0;
    }

    static List<LaneNode> convertEdgesToNodes(double[] edges, int width) {
        List<LaneNode> ret = new ArrayList<LaneNode>(2);

        LaneNode ln = new LaneNode();

        ln.setX(edges[0]);
        ln.setY(edges[1]);
        ln.setWidth(width);
        ln.setZ(0.0);

        ret.add(ln);

        ln = new LaneNode();

        ln.setX(edges[2]);
        ln.setY(edges[3]);
        ln.setWidth(width);
        ln.setZ(0.0);

        ret.add(ln);

        return ret;
    }

    /**
     * Creates LaneMovement objects based on a movementCode.
     * 
     * @param movementCode The code received from texas.
     * @return The list of LaneMovements.
     */
    static Map<Integer, LaneMovement> parseMovementCode(int movementCode) {
        Map<Integer, LaneMovement> lmm = new HashMap<Integer, LaneMovement>();

        // The code has 4 meaningful bits each indicating whether a movement
        // type is allowed for this lane.
        // From LSB to MSB they correspond to: right turn, straight, left turn,
        // U-turn
        if ((movementCode & 1) != 0) {
            LaneMovement lm = new LaneMovement();
            lm.setMovement(Movement.RIGHT_TURN);
            lm.setMovementId(1);
            lmm.put(lm.getMovementId(), lm);
        }
        if ((movementCode & 2) != 0) {
            LaneMovement lm = new LaneMovement();
            lm.setMovement(Movement.STRAIGHT);
            lm.setMovementId(2);
            lmm.put(lm.getMovementId(), lm);
        }
        if ((movementCode & 4) != 0) {
            LaneMovement lm = new LaneMovement();
            lm.setMovement(Movement.LEFT_TURN);
            lm.setMovementId(4);
            lmm.put(lm.getMovementId(), lm);
        }
        if ((movementCode & 8) != 0) {
            LaneMovement lm = new LaneMovement();
            lm.setMovement(Movement.U_TURN);
            lm.setMovementId(8);
            lmm.put(lm.getMovementId(), lm);
        }

        return lmm;
    }

    /**
     * Compute the true edges of the turning lane. Place these new edges into the old edges array.
     * 
     * @param edges The edges of the turning lane before adjusting for there not being a merging
     *        section. The new edges will be placed into this array.
     * @param laneGeometry The lane's geometry from TEXAS.
     * @param geomIndex The starting index in laneGeometry for this turning lane.
     * @param xMidpoint The x coordinate of the midpoint of the lane.
     * @param yMidpoint The y coordinate of the midpoint of the lane.
     */
    static void parseTurningLane(double[] edges, int[] laneGeometry, int geomIndex, double xMidpoint, double yMidpoint) {
        // Current lane is a turning lane, does not contain a merging section
        int turningLaneLength = 175 + laneGeometry[geomIndex + 3] - laneGeometry[geomIndex + 2];

        if (edges[1] == edges[3]) {
            // Horizontal
            if (edges[0] < xMidpoint) {
                edges[2] = edges[0] + turningLaneLength;
            }
            else {
                edges[2] = edges[0] - turningLaneLength;
            }

        }
        else if (edges[0] == edges[2]) {
            // Vertical
            if (edges[1] > yMidpoint) {
                edges[1] = edges[3] + turningLaneLength;
            }
            else {
                edges[1] = edges[3] - turningLaneLength;
            }
        }
    }

    /**
     * Compute the true edges of the merging lane. Place these new edges into the old edges array.
     * 
     * @param edges The edges of the turning lane before adjusting for there not being a turning
     *        section. The new edges will be placed into this array.
     * @param laneGeometry The lane's geometry from TEXAS.
     * @param geomIndex The starting index in laneGeometry for this turning lane.
     * @param xMidpoint The x coordinate of the midpoint of the lane.
     * @param yMidpoint The y coordinate of the midpoint of the lane.
     */
    static void parseMergingLane(double[] edges, int[] laneGeometry, int geomIndex, double xMidpoint, double yMidpoint) {
        // Current lane has a merging section, but not a turning lane
        int mergeLaneLength = laneGeometry[geomIndex + 1] - laneGeometry[geomIndex] + 1000;
        if (edges[1] == edges[3]) {
            if (edges[0] < xMidpoint) {
                edges[2] = edges[0] + mergeLaneLength;
            }
            else {
                edges[2] = edges[0] - mergeLaneLength;
            }
        }
        else if (edges[0] == edges[2]) {
            if (edges[1] > yMidpoint) {
                edges[1] = edges[3] + mergeLaneLength;
            }
            else {
                edges[1] = edges[3] - mergeLaneLength;
            }
        }
    }
}
