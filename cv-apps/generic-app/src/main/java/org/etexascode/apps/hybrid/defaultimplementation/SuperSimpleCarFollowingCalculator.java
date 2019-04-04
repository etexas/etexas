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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.apps.hybrid.interfaces.ICarFollowingCalculator;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;

/**
 * This is an implementation of the simpler version of the car velocity equation (equation 3) in
 * "120726 - RPA Model Enhancement - TRB2013". Note: Ablatt - cases not being handled: 1) Dealing
 * with the cars which are not in the current lane 2) Intersections which are not 4 way stop lights
 * 
 * @author ablatt
 */
public class SuperSimpleCarFollowingCalculator implements ICarFollowingCalculator {

    /** Constant for spacing at jam density (in meters) */
    double spacingAtJam = 8.26;

    /** Constant for spacing at capacity (in meters) */
    double spacingAtCapacity = spacingAtJam * 2.0;

    /** The previous time step's info model. */
    InterRepInfoModel prevModel = null;

    /** A generated vehicle id for use as a fake vehicle. */
    final static int genVehId = Integer.MAX_VALUE;

    @Override
    public Map<Integer, List<Vehicle>> performCarFollowing(Map<Integer, List<Vehicle>> vehiclesByLane, InterRepInfoModel interRep) {

        if (prevModel != null) {
            for (Entry<Integer, List<Vehicle>> entry : vehiclesByLane.entrySet()) {
                List<Vehicle> currVehs = entry.getValue();
                List<IVehicle> curr = (List<IVehicle>)interRep.vmi.getVehiclesInLane(entry.getKey());
                ILane li = interRep.lmi.getLaneById(entry.getKey());
                curr = placeVehForLight(curr, li, interRep.smi);
                List<Vehicle> vehsToDrop = new LinkedList<Vehicle>();

                for (Vehicle v : currVehs) {
                    IVehicle prev = prevModel.vmi.getVehicle(v.getProperId());
                    if (prev != null) {
                        moveVehicle(v, li, interRep.simTime - prevModel.simTime);
                        if (dropVeh(v, li)) {
                            vehsToDrop.add(v);
                            continue;
                        }
                        IVehicle vi = getInFront(v, curr);
                        double speedLimit = interRep.lmi.getLaneById(entry.getKey()).getSpeedLimitInMetersPerSecond();
                        if ((vi == null) || over50sAway(v, vi, speedLimit)) {
                            v.setSpeed(speedLimit);
                        }
                        else {
                            v.setSpeed(eq3(speedLimit - 3.0, speedLimit, spacingAtJam, spacingAtCapacity, UtilsUnitConversion.convertCentimetersToMeters(UtilsCalculations.getDistance(v, vi))));
                        }
                    }
                }

                for (Vehicle v : vehsToDrop) {
                    currVehs.remove(v);
                }

                vehiclesByLane.put(entry.getKey(), currVehs);
            }
        }

        prevModel = interRep;

        return vehiclesByLane;
    }

    /**
     * Determine if a Vehicle should be dropped from the lane.
     * 
     * @param v The vehicle to examine
     * @param li The lane to examine
     * @return (true/false) v is still in li
     */
    boolean dropVeh(Vehicle v, ILane li) {
        return UtilsCalculations.getDistance(v, li.getLaneGeomList().get(li.getLaneGeomList().size() - 1)) > UtilsCalculations.getDistance(li, li.getLaneGeomList()
                .get(li.getLaneGeomList().size() - 1));
    }

    /**
     * If necessary, place a vehicle at the optimal point to cause vehicles to act as though they
     * are approaching a red light.
     * 
     * @param curr The vehicles which are currently in the lane.
     * @param li The lane which we are currently examining.
     * @param smi The signal manager which will tell us if the light is red
     * @return The new list of vehicles (this might be immutable so be careful)
     */
    List<IVehicle> placeVehForLight(List<IVehicle> curr, ILane li, ISignalManager smi) {
        List<IVehicle> ret = curr;
        List<? extends ISignalIndication> si = smi.getSignalsByLaneId(li.getLaneId());
        if ((si != null) && (si.size() != 0)) {
            ISignalIndication sii = si.get(0);
            if (sii != null) {
                if (sii.getColorIndication() == SignalIndication.Color.RED) {
                    ret = new ArrayList<IVehicle>(curr.size() + 1);
                    ret.add(getVehForLight(li));
                    ret.addAll(curr);
                }
            }
        }

        return ret;
    }

    /**
     * Generate a vehicle to induce red light behavior in the specified lane.
     * 
     * @param li The lane we are generating the vehicle for.
     * @return The generated vehicle.
     */
    IVehicle getVehForLight(ILane li) {
        Vehicle v = new Vehicle(genVehId, 0.0, 0.0, li.getX(), li.getY(), 0.5);

        performMove(v, li.getLaneGeomList().get(1), -spacingAtJam + 2);
        v.setSpeed(0.0);

        return v;
    }

    /**
     * Determine if the vehicle has more than 50 seconds of headway (mentioned in the document as
     * the point at which the equation breaks down)
     * 
     * @param v The vehicle which our function is moving.
     * @param vi The vehicle in front of v
     * @param speedLimit The current speed limit
     * @return (ture/false) v has more than 50 seconds of headway
     */
    boolean over50sAway(Vehicle v, IVehicle vi, double speedLimit) {
        return UtilsCalculations.getDistance(v, vi) / UtilsUnitConversion.convertMetersToCentimeters(speedLimit) > 50.0;
    }

    /**
     * Get the vehicle in front of nP1.
     * 
     * @param nP1 The vehicle you need the vehicle in front of.
     * @param vehs The vehicles in the same lane as nP1
     * @return The vehicle in front of nP1.
     */
    IVehicle getInFront(Vehicle nP1, List<IVehicle> vehs) {
        IVehicle ret = null;

        for (IVehicle v : vehs) {
            if (v.getVehicleID() == nP1.getVehicleID()) {
                break;
            }
            else {
                ret = v;
            }
        }

        return ret;
    }

    /**
     * Move v
     * 
     * @param v The vehicle to be moved
     * @param li The lane to move the vehicle in
     * @param moveTime The amount of time over which the vehicle is moving
     */
    void moveVehicle(Vehicle v, ILane li, double moveTime) {
        double moveDist = UtilsUnitConversion.convertMetersToCentimeters(v.getSpeed() * moveTime);
        performMove(v, getMoveToward(v, li, moveDist), moveDist);
    }

    /**
     * Get the point to move toward.
     * 
     * @param v The origin we are moving from
     * @param li The lane we are moving in
     * @param moveDist The distance we are going to move (used as a restriction for how close a node
     *        needs to be before we will stop using it)
     * @return The point to move toward
     */
    IDistanceable getMoveToward(IDistanceable v, ILane li, double moveDist) {
        IDistanceable ret = getBefore(v, li);
        double d = UtilsCalculations.getDistance(v, ret);

        if (d < moveDist) {
            ret = getBefore(ret, li);
        }

        return ret;
    }

    /**
     * Get the point before v in the lane (returns the first point in the lane if none is found)
     * 
     * @param v The point to examine
     * @param li The lane to examine
     * @return The point before v in li
     */
    IDistanceable getBefore(IDistanceable v, ILane li) {
        double d = UtilsCalculations.getDistance(v, li);
        IDistanceable lni = li.getLaneGeomList().get(0);
        for (ILaneNode tmp : li.getLaneGeomList()) {
            if (d > UtilsCalculations.getDistance(tmp, li)) {
                lni = tmp;
            }
            else {
                break;
            }
        }

        return lni;
    }

    /**
     * Performs the actual movement of v
     * 
     * @param v The vehicle to move
     * @param target The target to move v toward
     * @param moveDistance The distance (in cm) to move v.
     */
    void performMove(Vehicle v, IDistanceable target, double moveDistance) {
        double[] d = UtilsCalculations.moveToward(v, target, moveDistance);
        v.setX(d[0]);
        v.setY(d[1]);
    }

    /**
     * The simple equation (equation 3) provided by the paper.
     * 
     * @param velocityAtCapacity The velocity at capacity (meters/second)
     * @param velocityDesiredByDriver The velocity desired by the driver (meters/second)
     * @param spacingAtJamDensity The spacing of the vehicles at the jam density (meters)
     * @param spacingAtCapacity The spacing of the vehicles when the intersection is at capacity
     *        (meters)
     * @param distBetweenVehicles The distance between the vehicle we are determining the speed of
     *        and the vehicle in front of it (front-to-front) (meters)
     * @return The velocity of the vehicle
     */
    double eq3(double velocityAtCapacity, double velocityDesiredByDriver, double spacingAtJamDensity, double spacingAtCapacity, double distBetweenVehicles) {
        double sub = subC1(velocityAtCapacity, velocityDesiredByDriver, spacingAtJamDensity);
        double c1t = c1(velocityAtCapacity, velocityDesiredByDriver, spacingAtJamDensity, sub);
        double c2t = c2(velocityAtCapacity, velocityDesiredByDriver, spacingAtJamDensity, sub);
        double c3t = c3(velocityAtCapacity, velocityDesiredByDriver, spacingAtJamDensity, spacingAtCapacity, sub);
        double p1 = (c3t * velocityDesiredByDriver) - c1t + distBetweenVehicles;
        double p2 = p1 * p1;
        double p3 = (distBetweenVehicles * velocityDesiredByDriver) - (c1t * velocityDesiredByDriver) - c2t;
        double p4 = 4 * c3t * p3;
        double p5 = Math.sqrt(p2 - p4);
        return (p1 - p5) / (2 * c3t);
    }

    /**
     * Sub-equation c1.
     * 
     * @param xc The velocity at capacity (meters/second)
     * @param xd The velocity desired by the driver (meters/second)
     * @param dxj The spacing of the vehicles at the jam density (meters)
     * @param sub The result of subC1
     * @return c1
     */
    double c1(double xc, double xd, double dxj, double sub) {
        return sub * ((2 * xc) - xd);
    }

    /**
     * Sub-equation c2
     * 
     * @param xc The velocity at capacity (meters/second)
     * @param xd The velocity desired by the driver (meters/second)
     * @param dxj The spacing of the vehicles at the jam density (meters)
     * @param sub The result of subC1
     * @return c2
     */
    double c2(double xc, double xd, double dxj, double sub) {
        return sub * (Math.pow((xd - xc), 2.0));
    }

    /**
     * Sub-equation c3
     * 
     * @param xc The velocity at capacity (meters/second)
     * @param xd The velocity desired by the driver (meters/second)
     * @param dxj The spacing of the vehicles at the jam density (meters)
     * @param dxc The spacing of the vehicles when the intersection is at capacity (meters)
     * @param sub The result of subC1
     * @return c3
     */
    double c3(double xc, double xd, double dxj, double dxc, double sub) {
        return (dxc / xc) - sub;
    }

    /**
     * An equation shared between c1, c2, c3
     * 
     * @param xc The velocity at capacity (meters/second)
     * @param xd The velocity desired by the driver (meters/second)
     * @param dxj The spacing of the vehicles at the jam density (meters)
     * @return subC1
     */
    double subC1(double xc, double xd, double dxj) {
        return (dxj * xd) / (xc * xc);
    }
}
