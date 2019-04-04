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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.codec.binary.Base64;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.j2735.Approach;
import org.etexascode.j2735.Approach.DrivingLanes;
import org.etexascode.j2735.ApproachObject;
import org.etexascode.j2735.Intersection;
import org.etexascode.j2735.Intersection.Approaches;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.MapData.Intersections;
import org.etexascode.j2735.VehicleReferenceLane;

/**
 * A utility class for binding MapData data to Intellidrive API data TODO ttevendale 1/2/2018 decide
 * if this class needs to be deleted after updating to new spec. Currently the new spec is quite
 * different
 * 
 * @author bbadillo
 * @author ablatt
 */
public class MapDataUtil {

    /**
     * Extract data about traffic lanes (such as the lane geometry) from a SAE J2735 MapData message
     * java object.
     * 
     * @param mapData A SAE J2735 MapData message java object.
     * @return A list of traffic lane geometry information.
     */
    public static List<Lane> getLaneVectorsFromMapData(MapData mapData) {
        List<Lane> ret = new LinkedList<Lane>();

        // Drill down into the message object.
        Intersections intersections = mapData.getIntersections();
        for (Intersection intersection : intersections.getIntersection()) {
            ret.addAll(getLaneVectorsFromIntersectionObject(intersection));
        }

        return ret;
    }

    /**
     * Extract data about traffic lanes (such as the lane geometry) from a SAE J2735 Intersection
     * java object.
     * 
     * @param intersection A SAE J2735 Intersection java object.
     * @return A list of traffic lane geometry information.
     */
    public static List<Lane> getLaneVectorsFromIntersectionObject(Intersection intersection) {
        List<Lane> ret = new LinkedList<Lane>();

        Integer widthOfLanes = intersection.getLaneWidth();
        Approaches approaches = intersection.getApproaches();
        // Iterate over the approaches within the current intersection.
        for (ApproachObject approachObject : approaches.getApproachObject()) {
            Approach approach = approachObject.getApproach();
            if (approach != null) {
                ret.addAll(parseApproach(approach, Lane.INBOUND, widthOfLanes));
            }

            Approach egress = approachObject.getEgress();
            if (egress != null) {
                ret.addAll(parseApproach(egress, Lane.OUTBOUND, widthOfLanes));
            }
        }

        return ret;
    }

    /**
     * Parse an individual approach into a set of lanes
     * 
     * @param approach Approach to parse (cannot be null)
     * @param type The type of lane
     * @param widthOfLanes The width of the lane if no width is specified for an individual point
     * @return A list of lanes parsed from the approach
     */
    private static List<Lane> parseApproach(Approach approach, String type, Integer widthOfLanes) {
        LinkedList<Lane> ret = new LinkedList<Lane>();
        DrivingLanes inboundLanes = approach.getDrivingLanes();
        for (VehicleReferenceLane lane : inboundLanes.getVehicleReferenceLane()) {
            // Determine the actual lane width...
            Integer laneWidth = lane.getLaneWidth();
            if (laneWidth == null) {
                laneWidth = widthOfLanes;
            }

            // Convert the lane into Intellidrive API node objects.
            List<String> nodeList = lane.getNodeList();

            List<LaneNode> laneGeomList = convertMessageNodeList(nodeList, laneWidth);

            // Create an Intellidrive API object with the data and put in the return list.
            Lane newLane = new Lane();
            newLane.setLaneId(lane.getLaneNumber()[0]);
            newLane.setApproachId(approach.getId());
            newLane.setLaneGeomList(laneGeomList);

            if (Lane.INBOUND.equals(type)) {
                newLane.setType(Lane.INBOUND);
            }
            else if (Lane.OUTBOUND.equals(type)) {
                newLane.setType(Lane.OUTBOUND);
            }
            else {
                throw new IllegalStateException();
            }
            ret.add(newLane);
        }

        return ret;
    }

    /**
     * Convert the MapData lane node list into Intellidrive API node objects.
     * 
     * @param nodeList The MapData Base64 encoded node list.
     * @param laneWidth The default lane width to use if no lane width is specified in a node.
     * @return A list of lane geometry nodes for use in the Intellidrive API
     */
    static protected List<LaneNode> convertMessageNodeList(List<String> nodeList, Integer laneWidth) {
        List<LaneNode> retList = new LinkedList<LaneNode>();
        Iterator<String> iterator = nodeList.iterator();
        while (iterator.hasNext()) {
            String base64EncodedString = iterator.next();
            byte[] decodedBase64 = Base64.decodeBase64(base64EncodedString);
            ByteBuffer bb = ByteBuffer.wrap(decodedBase64);

            LaneNode node = new LaneNode();
            // Convert the bytes 1 and 2 as the 16 bit integer x value
            node.setX(bb.getShort(0));

            // Convert the bytes 3 and 4 as the 16 bit integer y value
            node.setY(bb.getShort(2));

            try {
                // Convert the bytes 5 and 6 as the 16 bit integer z value
                node.setZ(bb.getShort(4));

                // Convert the bytes 7 and 8 as the 16 bit integer width value
                node.setWidth(bb.getShort(6));
            }
            catch (IndexOutOfBoundsException e) {
                node.setWidth(laneWidth);
            }

            retList.add(node);
        }

        return retList;
    }

    /**
     * Implements the Cohen-Sutherland line clipping algorithm.
     * 
     * @param x0 The x-coordinate of the first point.
     * @param y0 The y-coordinate of the first point.
     * @param x1 The x-coordinate of the second point.
     * @param y1 The y-coordinate of the second point.
     * @param xmin The x-value of the left of the bounding rectangle.
     * @param xmax The x-value of the right of the bounding rectangle.
     * @param ymin The y-value of the bottom of the bounding rectangle.
     * @param ymax The y-value of the top of the bounding rectangle.
     * @return The end points of the clipped line in the order: x0, y0, x1, y1. Returns null if the
     *         line is entirely out of bounds.
     */
    public static double[] cohenSutherland(double x0, double y0, double x1, double y1, double xmin, double xmax, double ymin, double ymax) {
        // Algorithm adapted from en.wikipedia.org/Cohen-Sutherland_algorithm
        final int INSIDE = 0;
        final int LEFT = 1;
        final int RIGHT = 2;
        final int BOTTOM = 4;
        final int TOP = 8;

        while (true) {
            int outcode0 = INSIDE;
            if (x0 < xmin)
                outcode0 |= LEFT;
            else if (x0 > xmax)
                outcode0 |= RIGHT;
            if (y0 < ymin)
                outcode0 |= BOTTOM;
            else if (y0 > ymax)
                outcode0 |= TOP;

            int outcode1 = INSIDE;
            if (x1 < xmin)
                outcode1 |= LEFT;
            else if (x1 > xmax)
                outcode1 |= RIGHT;
            if (y1 < ymin)
                outcode1 |= BOTTOM;
            else if (y1 > ymax)
                outcode1 |= TOP;

            // If the line is entirely within bounds, return itself. (trivially accept)
            if ((outcode0 | outcode1) == INSIDE) {
                return new double[] { x0, y0, x1, y1 };
            }
            // If the line is entirely out of bounds, return null. (trivially reject)
            else if ((outcode0 & outcode1) != INSIDE) {
                return null;
            }
            // Otherwise we have to calculate a clipping point.
            else {
                double newx, newy;

                int outcodeOut = (outcode0 != INSIDE) ? outcode0 : outcode1;

                if ((outcodeOut & TOP) != 0) { // point is above the clip rectangle
                    newx = x0 + (x1 - x0) * (ymax - y0) / (y1 - y0);
                    newy = ymax;
                }
                else if ((outcodeOut & BOTTOM) != 0) { // point is below the clip rectangle
                    newx = x0 + (x1 - x0) * (ymin - y0) / (y1 - y0);
                    newy = ymin;
                }
                else if ((outcodeOut & RIGHT) != 0) { // point is to the right of clip rectangle
                    newy = y0 + (y1 - y0) * (xmax - x0) / (x1 - x0);
                    newx = xmax;
                }
                else { // point is to the left of clip rectangle
                    newy = y0 + (y1 - y0) * (xmin - x0) / (x1 - x0);
                    newx = xmin;
                }

                if (outcodeOut == outcode0) {
                    x0 = newx;
                    y0 = newy;
                }
                else {
                    x1 = newx;
                    y1 = newy;
                }
            }
        }
    }

    /**
     * Calculates how much of the lane geometry can't be represented by a MapData message.
     * 
     * @param lm The LaneManager.
     * @param rps The reference points.
     * @param geoCalculatorType The type of calculator to use for latitude/longitude calculations.
     * @return The percent of the lane area that isn't covered.
     */
    public static double calculateMapCoverage(ILaneManager lm, List<ReferencePoint> rps, int geoCalculatorType) {
        // Make a list of all the lane segments.
        List<LaneSegment> segments = new LinkedList<LaneSegment>();
        for (ILane lane : lm) {
            LaneNode prev = null;
            for (ILaneNode currInfo : lane) {
                LaneNode curr = new LaneNode(currInfo.getX(), currInfo.getY(), currInfo.getZ(), currInfo.getWidth());

                if (prev != null) {
                    LaneSegment ls = new LaneSegment(prev, curr);
                    segments.add(ls);
                }

                prev = curr;
            }
        }

        // Calculate the total area.
        double totalArea = 0.0;
        for (LaneSegment ls : segments) {
            totalArea += ls.getArea();
        }

        // Find which areas aren't covered
        List<LaneSegment> uncoveredList = segments;
        for (ReferencePoint rp : rps) {
            double[] refPtOffsetCm = UtilsLatLongConversion.convertLatLongToCentimeterOffset(lm.getLatitude(), lm.getLongitude(), rp.getLatitude(), rp.getLongitude(), geoCalculatorType);
            double cx = refPtOffsetCm[0];
            double cy = refPtOffsetCm[1];

            List<LaneSegment> stillUncovered = new LinkedList<LaneSegment>();

            for (LaneSegment ls : uncoveredList) {
                stillUncovered.addAll(ls.getUncoveredSegments(cx, cy, Short.MIN_VALUE, Short.MAX_VALUE, Short.MIN_VALUE, Short.MAX_VALUE));
            }

            uncoveredList = stillUncovered;
        }

        // Calculate the uncovered area.
        double uncoveredArea = 0.0;
        for (LaneSegment ls : uncoveredList) {
            uncoveredArea += ls.getArea();
        }

        return (1 - (uncoveredArea / totalArea)) * 100.0;
    }

    /**
     * Provides helper functionality for determining MapData coverage.
     * 
     * @author janway
     */
    public static class LaneSegment {

        /**
         * Tolerance for double equality.
         */
        static private final double TOLERANCE = .00005;

        /**
         * The first node in this segment (nearer the intersection).
         */
        LaneNode node1;

        /**
         * The second node in this segment (further from the intersection).
         */
        LaneNode node2;

        /**
         * Constructor.
         * 
         * @param n1 Near node.
         * @param n2 Far node.
         */
        public LaneSegment(LaneNode n1, LaneNode n2) {
            node1 = n1;
            node2 = n2;
        }

        /**
         * Calculates the area of the segment.
         * 
         * @return Area = average width of nodes times distance between nodes.
         */
        public double getArea() {
            return ((node1.getWidth() + node2.getWidth()) / 2) * UtilsCalculations.getDistance(node1, node2);
        }

        /**
         * Returns the sub-segments that are outside the bounding box.
         * 
         * @param cx The x-coordinate of the reference point.
         * @param cy The y-coordinate of the reference point.
         * @param negX The maximum negative x-offset.
         * @param posX The maximum positive x-offset.
         * @param negY The maximum negative y-offset.
         * @param posY The maximum positive y-offset.
         * @return The segments outside the bounding box.
         */
        public List<LaneSegment> getUncoveredSegments(double cx, double cy, int negX, int posX, int negY, int posY) {
            List<LaneSegment> ret = new LinkedList<LaneSegment>();

            if (Math.abs(getArea()) < TOLERANCE) {
                return ret;
            }

            // Finds the points where the line intersects with the bounding box. If it is entirely
            // within the box it returns the endpoints,
            // if entirely outside it returns null.
            double[] clipPoints = MapDataUtil.cohenSutherland(node1.getX(), node1.getY(), node2.getX(), node2.getY(), cx + negX, cx + posX, cy + negY, cy + posY);

            if (clipPoints == null) {
                ret.add(this);
            }
            else {
                LaneNode newNode1 = new LaneNode(node1);
                LaneNode newNode2 = new LaneNode(node2);
                newNode1.setX(clipPoints[0]);
                newNode1.setY(clipPoints[1]);
                newNode2.setX(clipPoints[2]);
                newNode2.setY(clipPoints[3]);

                LaneSegment ls1 = new LaneSegment(newNode1, node1);
                LaneSegment ls2 = new LaneSegment(node2, newNode2);

                // If the line isn't clipped, then the new point is the old point, which makes an
                // area of 0, so we ignore it.
                if (!(Math.abs(ls1.getArea()) < TOLERANCE)) {
                    ret.add(ls1);
                }
                if (!(Math.abs(ls2.getArea()) < TOLERANCE)) {
                    ret.add(ls2);
                }
            }
            return ret;
        }
    }
}
