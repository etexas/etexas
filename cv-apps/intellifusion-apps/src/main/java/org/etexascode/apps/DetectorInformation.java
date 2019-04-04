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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;

import org.etexascode.CoberturaIgnore;

/**
 * A structure to contain the latest known information from detectors. TODO ttevendale 1/4/2018 look
 * into this class and decide if it's worth keeping, also we should probably wait until we are
 * working on the intellifusion project again.
 * 
 * @author bbadillo
 */
class DetectorInformation {

    /**
     * The lane this detector is in.
     */
    private int laneId;

    /**
     * A window of time within which to calculate a moving average of flow.
     */
    private double timeWindow;

    /**
     * A map of vehicle counts given at specific periods of time.
     */
    private Queue<VehicleCount> vehicleCounts = new LinkedList<VehicleCount>();

    /**
     * The current tally of vehicles.
     */
    private int currCount;

    /**
     * Constructor
     * 
     * @param laneId The id of the lane the detector is in.
     */
    public DetectorInformation(int laneId, double timeWindow) {
        this.laneId = laneId;
        this.timeWindow = timeWindow;
    }

    /**
     * Getter for the lane this detector is in.
     * 
     * @return The lane this detector is in.
     */
    public int getLaneId() {
        return laneId;
    }

    /**
     * Add a specific amount to the vehicle count.
     * 
     * @param increment The amount to add to the vehicle count.
     */
    @CoberturaIgnore
    public void add(int increment) {
        currCount += increment;
    }

    /**
     * Set the time and reset the current count.
     * 
     * @param time The time at which a vehicle count was made.
     */
    public void setTime(double time) {
        VehicleCount vehicleCount = new VehicleCount(time);
        vehicleCount.add(currCount);
        currCount = 0;
        vehicleCounts.offer(vehicleCount);
        Iterator<VehicleCount> iterator = vehicleCounts.iterator();
        while (iterator.hasNext()) {
            VehicleCount countObj = iterator.next();
            if ((time - timeWindow) > countObj.getTime()) {
                iterator.remove();
            }
        }
    }

    /**
     * Get the current flow calculation based on vehicles detected over the current time window.
     * 
     * @return The current flow calculation. Units are Vehicles / second.
     */
    public double getFlow() {
        Iterator<VehicleCount> iterator = vehicleCounts.iterator();
        if (iterator.hasNext()) {
            VehicleCount firstCount = iterator.next();
            int totalCount = firstCount.getCount();
            VehicleCount countObj = null;
            while (iterator.hasNext()) {
                countObj = iterator.next();
                totalCount += countObj.getCount();
            }
            if (countObj != null) {
                double timeDiff = countObj.getTime() - firstCount.getTime();
                if (timeDiff > 0.0) {
                    return (totalCount / timeDiff);
                }
            }
        }
        return 0.0;
    }

    /**
     * A method used to get the detected time-mean speed.
     * 
     * @return The arithmetic average of vehicle velocities over the detector. Units are feet /
     *         second.
     */
    public double getTimeMeanSpeed() {
        // TODO: bbadillo - Add velocity capabilities to detectors for reporting time mean speed.
        return 44.0; // 30 mph in ft/sec
    }

    /**
     * A private inner class to help track vehicle counts.
     */
    static private class VehicleCount {

        /**
         * The time for the vehicle count.
         */
        private double time;

        /**
         * The vehicle count.
         */
        private int count;

        /**
         * Constructor
         * 
         * @param time The default time to set.
         */
        private VehicleCount(double time) {
            this.time = time;
            this.count = 0;
        }

        /**
         * @return The time for the vehicle count.
         */
        private double getTime() {
            return time;
        }

        /**
         * @return The vehicle count.
         */
        private int getCount() {
            return count;
        }

        /**
         * Add a specific amount to the vehicle count.
         * 
         * @param increment The amount to add to the vehicle count.
         */
        private void add(int increment) {
            count += increment;
        }
    }
}
