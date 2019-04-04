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
import java.awt.geom.Area;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A class containing functions which operate on or concerning the data structures contained in
 * InterRep.
 * 
 * @author ablatt
 * @author janway
 * @author bmauldon
 */
public class UtilsInterRep {

    /**
     * Static private final logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(UtilsInterRep.class);

    /**
     * The ratio between the turn of the steering wheel and the turn of the wheels (degrees).
     * Wikipedia states that most passenger cars have steering ratios between 12:1 and 20:1. The
     * selected 16:1 ratio represents the median value.
     */
    private static final double STEERING_RATIO = 16.0;

    /**
     * Assigns a vehicle to a lane and stores that lane id in the vehicle. Leaves null if not.
     * Returns the lane that the IDistanceable is in regardless of type, but sets Vehicles to be in
     * lane.
     * 
     * @param v vehicle or point that will be assigned to a lane
     * @param lm lane manager that has vehicle added to it
     * @return the lane v was in, -1 if none
     */
    public static int getLaneAssignment(IDistanceable v, ILaneManager lm) {
        int lanNum = -1;
        Map<Integer, Lane> laneMap = lm.getLanes();

        for (Entry<Integer, Lane> entry : laneMap.entrySet()) {
            Integer i = entry.getKey();
            if (isPointInLane(v, entry.getValue())) {
                if (v instanceof IVehicle) {
                    ((Vehicle)v).setLaneID(i);
                }
                lanNum = i;
            }
        }
        return lanNum;
    }

    /**
     * Build a polygon out of 2 lane Nodes.
     * 
     * @param ln1 first lane node to build polygon from
     * @param ln2 second lane node to build polygon from
     * @return The polygon.
     */
    public static Polygon buildPolygonFromLaneNodes(ILaneNode ln1, ILaneNode ln2) {
        // Get the vector of the path from p1 to p2
        double tx = ln2.getX() - ln1.getX();
        double ty = ln2.getY() - ln1.getY();

        // Get the normal orthogonal vector of the path from p1 to p2
        double otx = ty;
        double oty = -tx;
        double otMag = Math.sqrt(otx * otx + oty * oty);
        otx = otx / otMag;
        oty = oty / otMag;

        // Find the four points of the rectangle
        double p1HalfWidth = ln1.getWidth() / 2;
        double p2HalfWidth = ln2.getWidth() / 2;

        Polygon poly = new Polygon();

        poly.addPoint((int)(ln1.getX() - otx * p1HalfWidth), (int)(ln1.getY() - oty * p1HalfWidth));
        poly.addPoint((int)(ln1.getX() + otx * p1HalfWidth), (int)(ln1.getY() + oty * p1HalfWidth));
        poly.addPoint((int)(ln2.getX() + otx * p2HalfWidth), (int)(ln2.getY() + oty * p2HalfWidth));
        poly.addPoint((int)(ln2.getX() - otx * p2HalfWidth), (int)(ln2.getY() - oty * p2HalfWidth));

        return poly;
    }

    /**
     * Returns a deep copy of an object.
     * 
     * @param srcObj The source of the copy.
     * @return The newly copied object.
     */
    public static Object deepCopy(Object srcObj) {
        ObjectOutputStream oos = null;
        ObjectInputStream ois = null;
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            oos = new ObjectOutputStream(bos);
            oos.writeObject(srcObj);
            oos.flush();

            ByteArrayInputStream bin = new ByteArrayInputStream(bos.toByteArray());
            ois = new ObjectInputStream(bin);
            Object newObj = ois.readObject();

            oos.close();
            ois.close();
            return newObj;
        }
        catch (ClassNotFoundException ex) {
            LOGGER.info("ClassNotFoundException", ex);
            throw new RuntimeException(ex);
        }
        catch (IOException e) {
            LOGGER.info("IOException", e);
            throw new RuntimeException(e);
        }
    }

    /**
     * Generate a detector event for vehicle v if that vehicle passes over a detector in
     * DetectorManager detectorManager.
     * 
     * @param v The vehicle at the current time step.
     * @param prev The vehicle as it was at the previous time step.
     * @param detectorManager The detector manager for the current time step.
     * @param prevDetectorManager The detector manager for the previous time step. Used to calculate
     *        presence change events.
     * @param laneManager The lane manager for the current time step.
     */
    public static void genDetEvent(Vehicle v, Vehicle prev, DetectorManager detectorManager, DetectorManager prevDetectorManager, LaneManager laneManager) {
        double[] heading = genHeading(v);

        Area vehicleArea = UtilsCalculations.genAreaFromVehicle(v, heading);

        // Lazy initialize this.
        Area prevVehicleArea = null;

        for (Detector detector : detectorManager.getDetectorCollection()) {

            Detector prevDetector = prevDetectorManager.getDetector(detector.getDetectorID());
            if (UtilsCalculations.areaIsOverDetector(vehicleArea, detector)) {
                DetectorEvent detectorEvent = detector.getDetEvent();
                if (detector.getDetEvent() == null) {
                    detectorEvent = new DetectorEvent();
                    detectorEvent.setDetectorId(detector.getDetectorID());
                }

                // Set presence when vehicle is over detector. However, do not set vehicle length or
                // speed until vehicle comes off detector.
                // That is a capability for more than just presence detection.
                detectorEvent.setPresence(true);

                // If this is a change from non-presence to presence then pulse.
                if (prevDetector != null && prevDetector.getDetEvent() != null) {
                    DetectorEvent prevDetectorEvent = prevDetector.getDetEvent();
                    if (!prevDetectorEvent.isPresence()) {
                        // Create a pulse when there is current presence and no previous presence.
                        detectorEvent.setPulse(detectorEvent.getPulse() + 1);
                    }
                }
                else {
                    // Create a pulse when there is current presence but no previous detection.
                    detectorEvent.setPulse(detectorEvent.getPulse() + 1);
                }

                detector.setDetEvent(detectorEvent);
            }
            else if (prev != null && prevDetector != null && prevDetector.getDetEvent() != null && prevDetector.getDetEvent().isPresence()) {

                if (prevVehicleArea == null) {
                    prevVehicleArea = UtilsCalculations.genAreaFromVehicle(prev, genHeading(prev));
                }

                if (UtilsCalculations.areaIsOverDetector(prevVehicleArea, detector)) {

                    // Create a pulse when the presence turns to false making sure that this vehicle
                    // is the one getting off the detector.
                    DetectorEvent detectorEvent = detector.getDetEvent();
                    if (detector.getDetEvent() == null) {
                        detectorEvent = new DetectorEvent();
                        detectorEvent.setDetectorId(detector.getDetectorID());
                    }

                    // Add one to pulse for the detector, however don't set presence to false
                    // because another vehicle can still be on the detector.
                    detectorEvent.setPulse(detectorEvent.getPulse() + 1);
                    detectorEvent.setSpeed(v.getSpeed());
                    detectorEvent.setLength(v.getLength());
                    detector.setDetEvent(detectorEvent);
                }
            }
        }
    }

    // TODO the heading methods all included a line for angle correction
    // 360-(currHeading-90) since calcAngle returns an angle between 0-360
    // this gives an angle between 450-90. Also, calculating a heading requires a
    // fixed point, such as North. We need to use the lat/long for that.

    /**
     * Generates the Vehicle heading even if null.
     * 
     * @param curr The vehicle from the current time step.
     * @param prev The vehicle from the previous time step.
     * @param lm The lane manager.
     * @return The heading for the vehicle.
     */
    public static double genVehicleHeading(IVehicle curr, IVehicle prev, ILaneManager lm) {
        Double currHeading = curr.getHeading();

        // If there is no previous vehicle data, then use the direction of the lane it is in if
        // possible.
        if (prev == null) {
            Lane laneById = lm.getLaneById(curr.getLaneID());
            if (laneById != null) {
                List<LaneNode> nodes = laneById.getLaneGeomList();
                // TODO: janway - use the pair of nodes the vehicle is between
                LaneNode first = nodes.get(0);
                LaneNode second = nodes.get(1);
                currHeading = calcAngle(first, second);

            }
            else {
                // Not enough info to do anything, so just leave it.
            }
        } // If the vehicle is stopped, use the previous heading.
          // else if (curr.getX() == prev.getX() && curr.getY() == prev.getY()) {
        else if (curr.getSpeed() <= UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(5.0)) {
            currHeading = prev.getHeading();
        } // Otherwise, calculate heading based on the movement between the current position and the
          // previous position.
        else {
            currHeading = calcAngle(prev, curr);

        }
        return currHeading;
    }

    /**
     * Generate the heading of vehicle currently in the form double[]{RISE, RUN} Note: RISE/RUN ==
     * slope.
     * 
     * @param curr The vehicle from the current time step.
     * @return The vehicle's current heading as rise/run.
     */
    public static double[] genHeading(IVehicle curr) {
        double currHeading = curr.getHeading();

        return new double[] { Math.sin(Math.toRadians(currHeading)), Math.cos(Math.toRadians(currHeading)) };
    }

    /**
     * A utility method to calculate an angle (in degrees) from a vector such that 0.0 less than or
     * equal heading less than 359.9875 Java - 0d is east and the degrees are positive in the
     * counter-clockwise direction, i.e. 90d is north. We need 0d north, and the degrees are
     * positive in the clockwise direction, i.e. east is 90d for the SAE J2735 spec. WE need to
     * reverse direction and rotate by 90d
     * 
     * @param point1 the first point
     * @param point2 the second point
     * @return The heading of the vector in degrees.
     */
    static public double calcAngle(IDistanceable point1, IDistanceable point2) {
        double x1 = point1.getX();
        double y1 = point1.getY();

        double x2 = point2.getX();
        double y2 = point2.getY();

        double dx = x2 - x1;
        double dy = y2 - y1;
        double angle = Math.toDegrees(Math.atan2(dy, dx));
        // convert from -180,180 to 0,360
        if (angle < 0.0) {
            angle += 360;
        }
        // reverse direction
        angle = 360 - angle;
        // rotate 90d from east to north
        if (angle < 270) {
            angle = angle + 90;
        }
        else if (angle >= 270 && angle <= 360) {
            angle = angle - 270;
        }
        else {
            LOGGER.debug("Bad heading angle, not between 0,360");
        }
        // J2735 spec requirement.
        if (359.9875 < angle) {
            angle = 359.9875;
        }
        return angle;
    }

    /**
     * Get the base node of a lane
     * 
     * @param lane The lane
     * @return The base node.
     */
    public static ILaneNode getBaseNode(ILane lane) {
        return lane.getLaneGeomList().get(0);

    }

    /**
     * Get the node farthest from the intersection for the lane
     * 
     * @param lane The lane to get the node information from
     * @return The farthest node from intersection.
     */
    public static ILaneNode getLastNode(ILane lane) {
        return lane.getLaneGeomList().get(lane.getLaneGeomList().size() - 1);
    }

    /**
     * Returns true if the point specified by x and y are contained in the lane specified by the
     * geometry in the node list.
     * 
     * @param point the point to check
     * @param lane the lane to check against
     * @return True if the point is contained in the lane.
     */
    public static boolean isPointInLane(IDistanceable point, Lane lane) {
        double x = point.getX();
        double y = point.getY();
        Iterator<LaneNode> iterator = lane.getLaneGeomList().iterator();

        LaneNode lastNode = null;
        while (iterator.hasNext()) {
            LaneNode currNode = iterator.next();

            if (lastNode != null) {
                Polygon poly = buildPolygonFromLaneNodes(lastNode, currNode);

                if (poly.contains(x, y)) {
                    return true;
                }
            }

            lastNode = currNode;
        }

        return false;
    }

    /**
     * Function for determining if a vehicle is in the logout zone of its lane. This function does
     * assume that your vehicle is in a lane. You will get a NullPointerException if the vehicle is
     * not in a lane.
     * 
     * @param lm manager for all lanes
     * @param v vehicle to check against
     * @param timeStepInterval the current time step interval
     * @return true/false - v is in the logout zone of its lane
     */
    public static boolean vehicleInLogoutZone(ILaneManager lm, IVehicle v, double timeStepInterval) {
        Lane l = lm.getLaneById(v.getLaneID());
        LaneNode last = l.getLaneGeomList().get(l.getLaneGeomList().size() - 1);
        LOGGER.debug("The lane number {} and the last node {}", l.getLaneId(), last);
        if (l.getType().equals(Lane.OUTBOUND)) {
            if (v.getSpeed() * timeStepInterval <= UtilsCalculations.getDistance(v, last)) {
                LOGGER.debug("In logout zone: {}", v);
                return true;
            }
        }

        return false;
    }

    /**
     * Returns the steering wheel angle (in degrees) of a vehicle in the current time step. The
     * method should not be invoked before the heading of the vehicle in the current time step has
     * been updated.
     * 
     * @param current the vehicle in the current time step
     * @param previous the vehicle in the previous time step
     * @return the steering wheel angle (in degrees) of the vehicle in the current time step
     */
    static double calculateSteeringAngle(Vehicle current, Vehicle previous) {

        // if no previous heading exists
        if (previous == null || !previous.isHeadingSet()) {

            // return that the steering angle is not available
            return Byte.MAX_VALUE;
        }

        // calculate the degrees of change in the steering wheel
        return (current.getHeading() - previous.getHeading()) * UtilsInterRep.STEERING_RATIO;
    }
}
