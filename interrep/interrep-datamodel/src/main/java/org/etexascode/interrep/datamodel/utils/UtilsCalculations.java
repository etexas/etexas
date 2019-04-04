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
package org.etexascode.interrep.datamodel.utils;

import java.awt.Polygon;
import java.awt.geom.Area;

import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * A utility class for performing common calculations on the model.
 * 
 * @author ablatt
 */
public class UtilsCalculations {

    /**
     * Determines if the specified area is over a detector
     * 
     * @param a The area of an object
     * @param d The detector
     * @return True/False the area is over the detector
     */
    public static boolean areaIsOverDetector(Area a, IDetector d) {
        Area dArea = new Area(d.getArea());
        dArea.intersect(a);
        return !dArea.isEmpty();
    }

    /**
     * Generates the area of a vehicle from the vehicle interface
     * 
     * @param v The vehicle interface to get area for
     * @param heading The heading from the vehicle
     * @return The area of the vehicle
     */
    public static Area genAreaFromVehicle(IVehicle v, double[] heading) {
        return new Area(genPolygonFromVehicle(v, heading));
    }

    /**
     * Generates the polygon to represent the vehicle
     * 
     * @param v The vehicle interface to get polygon for
     * @param heading The heading of the vehicle
     * @return The polygon representation for the vehicle
     */
    public static Polygon genPolygonFromVehicle(IVehicle v, double[] heading) {
        return genPolygonFromBox(v.getX(), v.getY(), v.getWidth(), v.getLength(), heading);
    }

    /**
     * Generates the polygon box of the car
     * 
     * @param x The x coordinate of the center of the front of the car
     * @param y The y coordinate of the center of the front of the car
     * @param width The width of the car
     * @param length The length of the car
     * @param heading The heading of the car
     * @return vPoly The polygon of the vehicle
     */
    public static Polygon genPolygonFromBox(double x, double y, double width, double length, double[] heading) {
        Polygon vPoly = new Polygon();

        double[] normal = genNormal(heading);

        double[] frontCarCenter = new double[] { x, y };
        double[] rearCarCenter = moveAlongAxis(frontCarCenter, heading[0], heading[1], -length);

        double[] tmp = moveAlongAxis(frontCarCenter, normal[0], normal[1], -width / 2); // front
                                                                                        // left
        vPoly.addPoint((int)Math.round(tmp[0]), (int)Math.round(tmp[1]));

        tmp = moveAlongAxis(frontCarCenter, normal[0], normal[1], width / 2); // front right
        vPoly.addPoint((int)Math.round(tmp[0]), (int)Math.round(tmp[1]));

        tmp = moveAlongAxis(rearCarCenter, normal[0], normal[1], width / 2); // rear right
        vPoly.addPoint((int)Math.round(tmp[0]), (int)Math.round(tmp[1]));

        tmp = moveAlongAxis(rearCarCenter, normal[0], normal[1], -width / 2); // rear left
        vPoly.addPoint((int)Math.round(tmp[0]), (int)Math.round(tmp[1]));

        return vPoly;
    }

    /**
     * Generate the normal of the heading returned by genHeading.
     * 
     * @param heading The heading.
     * @return The normal heading.
     */
    public static double[] genNormal(double[] heading) {
        return new double[] { -heading[1], heading[0] };
    }

    /**
     * Move 'origin' distance 'dist' along slope 'run'/'rise'
     * 
     * @param origin The point to move.
     * @param rise The y-component of the axis to move along.
     * @param run The x-component of the axis to move along.
     * @param dist The distance to move the point along the axis.
     * @return The translated point.
     */
    public static double[] moveAlongAxis(double[] origin, double rise, double run, double dist) {
        // TODO: ablatt - integrate this with moveToward
        if (run == 0 && rise == 0) {
            return new double[] { origin[0], origin[1] };
        }

        double moveByConst = Math.sqrt(dist * dist / (run * run + rise * rise));

        if (dist < 0.0) {
            run *= -1;
            rise *= -1;
        }

        double[] ret = new double[2];

        run *= moveByConst;
        rise *= moveByConst;

        ret[0] = origin[0] + run;
        ret[1] = origin[1] + rise;

        return ret;
    }

    /**
     * Represents movement from initial point along an axis towards another point a specified
     * distance
     * 
     * @param toMove The point to move
     * @param toward The point to move towards
     * @param dist The distance to move the point
     * @return The movement along the axis created
     */
    public static double[] moveToward(IDistanceable toMove, IDistanceable toward, double dist) {
        // TODO: ablatt - find a way to make this return IDistanceable
        double[] d = new double[] { toMove.getX(), toMove.getY() };
        return moveAlongAxis(d, toward.getY() - toMove.getY(), toward.getX() - toMove.getX(), dist);
    }

    /**
     * Returns true if the vehicle is over the detector. Returns false otherwise.
     * 
     * @param v The vehicle.
     * @param d The detector.
     * @return True/False
     */
    public static boolean vehicleIsOverDetector(IVehicle v, IDetector d) {
        double[] heading = new double[] { Math.cos((v.getHeading())), Math.sin((v.getHeading())) };
        return areaIsOverDetector(genAreaFromVehicle(v, heading), d);
    }

    /**
     * Calculates the distance of the vehicle to the stop line of the lane.
     * 
     * @param vehicle The vehicle.
     * @param lane The lane.
     * @return Distance in centimeters.
     */
    @SuppressWarnings("rawtypes")
    public static double calcDistToStopLine(IVehicle vehicle, ILane lane) {
        if (vehicle == null) {
            return -1.0;
        }
        else {
            return getDistance(vehicle, lane);
        }
    }

    /**
     * Returns the base node of the lane. Will throw an exception if the lane geometry list is not
     * properly formed.
     * 
     * @param lane The lane.
     * @return The lane node info.
     */
    public static ILaneNode getBaseNode(ILane lane) {
        if (lane.getLaneGeomList().size() < 1) {
            throw new RuntimeException("Lane " + lane.getLaneId() + " has no lane geometry");
        }
        else {
            return lane.getLaneGeomList().get(0);
        }
    }

    /**
     * Get the distance between id1 and id2
     * 
     * @param id1 first x, y pair
     * @param id2 second x, y pair
     * @return the distance between id1 and id2
     */
    public static double getDistance(IDistanceable id1, IDistanceable id2) {

        if ((id1 == null) || (id2 == null)) {

            return -1;
        }
        double x = id1.getX() - id2.getX();
        double y = id1.getY() - id2.getY();
        double z = id1.getZ() - id2.getZ();

        return Math.sqrt((x * x) + (y * y) + (z * z));
    }

    /**
     * Get the distance between (x1, y1) and (x2, y2)
     * 
     * @param x1 First x
     * @param y1 First y
     * @param x2 Second x
     * @param y2 Second y
     * @return The distance between (x1, y1) and (x2, y2)
     */
    public static double getDist(double x1, double y1, double x2, double y2) {
        double x = x1 - x2;
        double y = y1 - y2;
        return Math.sqrt((x * x) + (y * y));
    }

    /**
     * Calculates the speed of a vehicle from the current step information and the previous step
     * information
     * 
     * @param id1 The current time step distanceable object.
     * @param id2 The previous time step distanceable object.
     * @param timeStepInterval The time step interval between the current step and the previous
     *        step.
     * @return The speed of the vehicle.
     */
    public static double genSpeed(IDistanceable id1, IDistanceable id2, double timeStepInterval) {

        return getDistance(id1, id2) / timeStepInterval;
    }

    /**
     * Calculates the acceleration of a vehicle with a speed from the current time step and the
     * previous time step.
     * 
     * @param veh1 The current time step vehicle.
     * @param veh2 The previous time step vehicle.
     * @param timeStepInterval The time step interval between the current step and the previous
     *        step.
     * @return The acceleration of the vehicle.
     */
    public static double getAcceleration(IVehicle veh1, IVehicle veh2, double timeStepInterval) {

        return ((veh1.getSpeed() - veh2.getSpeed()) / timeStepInterval);
    }
}
