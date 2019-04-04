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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;

/**
 * Module for calculating the travel time to the stop line.
 * 
 * @author jconnelly
 */
public class TravelTimeMOE {

    /** The length from the intersection, should be configurable. */
    double lengthFromIntersection = 27000.0;

    /** Map for the vehicle lanes. Key: Vehicle Proper Id, Value: Lane Proper Id */
    Map<String, String> vehStartLane = new HashMap<String, String>();

    /**
     * Map of the time that the vehicle was within the specified length of the intersection. Key:
     * Vehicle Proper Id, Value: time in length of the intersection
     */
    Map<String, Double> vehicleStart = new HashMap<String, Double>();

    public TravelTimeMOE() {
        // Constructor intentionally left blank to satisfy JAXB
    }

    /**
     * Calculate the travel time to the stop line.
     * 
     * @param currMan The current Vehicle Manager.
     * @param lanes The Lane Manager.
     * @param simTime The current simulation time.
     * @param logger The logger.
     */
    public void update(IVehicleManager currMan, ILaneManager lanes, double simTime, AppLogger logger) {
        Lane lane = null;

        for (IVehicle v : currMan) {
            String vid = v.getProperId();
            lane = lanes.getLaneById(v.getLaneID());
            double dist = UtilsCalculations.getDistance(v, lane);
            double speed = v.getSpeed();

            if (lane.getType().equals(Lane.OUTBOUND)) {
                lane = null;
            }

            Double startTime = vehicleStart.get(vid);
            if (startTime != null) { // vehicle has been seen
                if (lane == null) {
                    writeLogs(vid, vehStartLane.get(vid), startTime, simTime, logger);
                    vehStartLane.remove(vid);
                    vehicleStart.remove(vid);
                }
            }
            else if (lane != null && dist <= lengthFromIntersection) { // vehicle has not been seen
                vehStartLane.put(vid, lane.getProperId());
                double tmp = simTime - ((lengthFromIntersection - dist) / speed);
                vehicleStart.put(vid, tmp);
            }
        }

        // Since it is possible for vehicles to randomly disappear from the model this is
        // to check to see if a vehicle is in the vehicle time map, but not in the vehicle
        // manager, if so then the vehicle is removed from the vehicle lane map and vehicle
        // time map and writes to logs
        Iterator<Entry<String, Double>> it = vehicleStart.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, Double> pairs = (Map.Entry<String, Double>)it.next();
            String vehId = pairs.getKey();
            if (currMan.getVehicle(vehId) == null) {
                writeLogs(vehId, vehStartLane.get(vehId), vehicleStart.get(vehId), simTime, logger);
                vehStartLane.remove(vehId);
                it.remove();
            }
        }
    }

    /**
     * Formats data and write logs.
     * 
     * @param vehId The vehicle id to use.
     * @param laneId The lane id to use.
     * @param vehStart The sim time the vehicle started being tracked.
     * @param simTime The current sim time (the time the vehicle stopped being tracked.
     * @param logger The logger to write the data out to.
     */
    public void writeLogs(String vehId, String laneId, double vehStart, double simTime, AppLogger logger) {
        StringBuilder sb1 = new StringBuilder("Travel time for ");
        sb1.append(vehId);
        sb1.append(", starting lane ");
        sb1.append(laneId);

        StringBuilder sb2 = new StringBuilder();
        sb2.append(simTime - vehStart);

        logger.log(sb1.toString(), sb2.toString());
    }

    /**
     * Gets the length from the intersection for configuration.
     * 
     * @return The length from the intersection.
     */
    public double getLengthFromIntersection() {
        return lengthFromIntersection;
    }

    /**
     * Sets the length from the intersection for configuration.
     * 
     * @param lfi The length from the intersection to set.
     */
    public void setLengthFromIntersection(double lfi) {
        this.lengthFromIntersection = lfi;
    }
}
