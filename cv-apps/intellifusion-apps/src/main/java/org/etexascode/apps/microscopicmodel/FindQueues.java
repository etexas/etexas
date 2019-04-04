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
package org.etexascode.apps.microscopicmodel;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsSpecialEquals;

/**
 * A module for finding the queue for each lane for a given time step. This module partially relies
 * upon the previous time step's queues to producer better results.
 * 
 * @author ablatt
 */
public class FindQueues {

    /**
     * The queues returned by the module last time step. Initialized to empty. This is used for
     * determining if cars are still in the queue. See the documentation on getQueue for more
     * details.
     */
    Map<Integer, List<IVehicle>> prevOut = new HashMap<Integer, List<IVehicle>>();

    /**
     * The speed below which a vehicle is considered to be stopped. Currently set to 5 mph.
     */
    private static final double stopSpeed = 2.2352; // 5 miles/hour = 2.2352 meters/second according
                                                    // to google unit translation

    /**
     * Parse the VehicleManager into queues. Note: maintains the return value as state, any changes
     * to the return value will throw off this module.
     * 
     * @param vehMan The vehicle manager which contains vehicles to put into queues.
     * @param lanMan A lane manager to define which lanes to get queues for
     * @return The queues of vehicles in vehMan
     */
    public Map<Integer, List<IVehicle>> getCarsInQueue(IVehicleManager vehMan, ILaneManager lanMan) {
        Map<Integer, List<IVehicle>> filtered = filterByLane(vehMan);
        Map<Integer, List<IVehicle>> ret = new HashMap<Integer, List<IVehicle>>();

        for (Integer i : lanMan.getLaneIds()) {
            ret.put(i, getQueue(filtered.get(i), prevOut.get(i), lanMan.getLaneById(i)));
        }

        prevOut = ret;
        return ret;
    }

    /**
     * Get the queue for a specific lane. Definitional note: Once a car is reported to have joined a
     * queue, that car is considered to be in the queue for as long as it is in the lane. This note
     * is why we must have access to the previous time step's queue.
     * 
     * @param listThisStep A list containing all the vehicles in the specified lane this time step.
     * @param queueLastStep A list containing all the vehicles which were in the queue last time
     *        step.
     * @param lane The lane the vehicles are in.
     * @return The queue for the specified lane for this time step.
     */
    public static List<IVehicle> getQueue(List<IVehicle> listThisStep, List<IVehicle> queueLastStep, ILane lane) {
        if (listThisStep == null) {
            return new ArrayList<IVehicle>(0);
        }

        if (queueLastStep == null) {
            queueLastStep = new ArrayList<IVehicle>(0);
        }

        IDistanceable currPoint = lane;
        List<IVehicle> ret = listThisStep;

        Collections.sort(ret, new VehicleComparator(lane.getX(), lane.getY()));
        for (int i = 0; i < listThisStep.size(); i++) {
            IVehicle vi = listThisStep.get(i);
            if (UtilsSpecialEquals.idableConatins(queueLastStep, vi)) {
                // keep the car in the queue regardless
            }
            else if ((vi.getSpeed() < stopSpeed) && Math.abs(UtilsCalculations.getDistance(currPoint, vi)) < 862) { // this
                                                                                                                    // is
                                                                                                                    // the
                                                                                                                    // average
                                                                                                                    // jam
                                                                                                                    // density,
                                                                                                                    // should
                                                                                                                    // we
                                                                                                                    // increase
                                                                                                                    // this
                                                                                                                    // number?

            }
            else {
                ret = chopList(listThisStep, i);
                break;
            }
            currPoint = vi;
        }

        for (IVehicle vi : queueLastStep) {
            if ((!UtilsSpecialEquals.idableConatins(ret, vi) && UtilsSpecialEquals.idableConatins(listThisStep, vi))) {
                ret.add(vi);
            }
        }

        return ret;
    }

    /**
     * Filter the vehicles in vmi by their lane id.
     * 
     * @param vmi The vehicle manager which contains vehicles to filter.
     * @return Key: Lane Id, Value: List of vehicles in that lane.
     */
    public static Map<Integer, List<IVehicle>> filterByLane(IVehicleManager vmi) {
        Map<Integer, List<IVehicle>> ret = new HashMap<Integer, List<IVehicle>>();

        for (String i : vmi.getAllVehicleIds()) {
            if (ret.containsKey(vmi.getVehicle(i).getLaneID())) {
                ret.get(vmi.getVehicle(i).getLaneID()).add(vmi.getVehicle(i));
            }
            else {
                List<IVehicle> lv = new ArrayList<IVehicle>();
                lv.add(vmi.getVehicle(i));
                ret.put(vmi.getVehicle(i).getLaneID(), lv);
            }
        }

        return ret;
    }

    /**
     * Create a list of the first firstRemoved emelments in it.
     * 
     * @param orig The list to tak the elements for the new list from.
     * @param firstRemoved The index of the first element in orig to remove.
     * @return A list comprised of the firstRemoved items from orig in it.
     */
    static List<IVehicle> chopList(List<IVehicle> orig, int firstRemoved) {
        List<IVehicle> ret = new ArrayList<IVehicle>(orig.size() - firstRemoved);

        for (int i = 0; i < firstRemoved; i++) {
            ret.add(orig.get(i));
        }

        return ret;
    }

    /**
     * Utility class for sorting vehicles based on distance to the stop line. This comparator will
     * help sort vehicles from closest to the stop line to farthest away.
     * 
     * @author ablatt
     */
    private static class VehicleComparator implements Comparator<IVehicle>, Serializable {

        /**
         * x coordinate of the stop line.
         */
        double x;

        /**
         * y coordinate of the stop line.
         */
        double y;

        /**
         * Constructor.
         * 
         * @param x x coordinate of the stop line.
         * @param y y coordinate of the stop line.
         */
        VehicleComparator(double x, double y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int compare(IVehicle v1, IVehicle v2) {
            return Double.compare(UtilsCalculations.getDist(v1.getX(), v1.getY(), x, y), UtilsCalculations.getDist(v2.getX(), v2.getY(), x, y));
        }
    }
}