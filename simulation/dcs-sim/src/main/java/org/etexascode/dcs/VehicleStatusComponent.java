/**********************************************************************
 *** *                                                            * ***
 *** *  Copyright (c) 2012 Harmonia Holdings Group LLC            * ***
 *** *                                                            * ***
 *** * Permission is hereby granted to use, modify, copy, and     * ***
 *** * distribute this software and its documentation for any     * ***
 *** * purpose only without profit, provided that the above       * ***
 *** * Copyright Notice appears in all copies and that both the   * ***
 *** * Copyright Notice and this Permission Notice appears in     * ***
 *** * every copy of supporting documentation.  No title to nor   * ***
 *** * ownership of the software is transferred hereby.  The name * ***
 *** * of Harmonia Holdings Group LLC shall not be used in        * ***
 *** * advertising or publicity related to the distribution of    * ***
 *** * the software without specific, written, prior permission.  * ***
 *** * This software is provided as-delivered without expressed   * ***
 *** * or implied warranty.  Harmonia Holdings Group LLC          * ***
 *** * makes no representation about the suitability of this      * ***
 *** * software for any purpose and accepts no responsibility for * ***
 *** * its use.                                                   * ***
 *** *                                                            * ***
 *** ************************************************************** ***
 *** *                                                            * ***
 *** * This program is free software; you can redistribute it     * ***
 *** * and/or modify it under the terms of the GNU General Public * ***
 *** * License as published by the Free Software Foundation;      * ***
 *** * either version 2 of the License, or (at your option) any   * ***
 *** * later version.                                             * ***
 *** *                                                            * ***
 *** * This program is distributed in the hope that it will be    * ***
 *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
 *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
 *** * PURPOSE.  See the GNU General Public License for more      * ***
 *** * details.                                                   * ***
 *** *                                                            * ***
 *** * You should have received a copy of the GNU General Public  * ***
 *** * License along with this program; if not, write to the Free * ***
 *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
 *** * Floor, Boston, MA 02110-1301, USA.                         * ***
 *** *                                                            * ***
 *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
 *** *                                                            * ***
 **********************************************************************/
package org.etexascode.dcs;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.Vehicle;

/**
 * A component used to check the status of vehicles and provide dilemma
 * zone information.
 *
 * @author bbadillo
 */
public class VehicleStatusComponent {

    /**
     * The estimated time of arrival to to the stop bar for a vehicle (in seconds)
     * at which the dilemma zone begins.
     */
    private static final double dilemmaZoneBeginTime = 5.5;
    /**
     * A collection of dilemma zone data for each vehicle detected by the
     * vehicle detectors. The map is keyed by unique vehicle identifiers.
     */
    private Map<Integer, VehicleDilemmaZoneData> dilemmaZoneMatrix = new HashMap<Integer, VehicleDilemmaZoneData>();
    /**
     * Reference to currently active InterRep instance.
     */
    private InterRep intersection;

    /**
     * Constructor
     *
     * @param detectors An array of preconfigured vehicle detectors. One for each lane.
     */
    public VehicleStatusComponent(InterRep interRep) {
//        this.detectors = detectors;
        this.intersection = interRep;
    }

    /**
     * Obtain the current collection of dilemma zone data.
     *
     * @return The currrent collection of dilemma zone data.
     */
    public Collection<VehicleDilemmaZoneData> getDilemmaZoneMatrix() {
        return dilemmaZoneMatrix.values();
    }

    /**
     * Detect vehicles and calculate a list of dilemma zone information.
     *
     * @param currentTime The current time (in seconds) since the start of the algorithm loop. 
     */
    public void performDetection(double currentTime) {
        Iterator<Integer> laneIterator = intersection.getIntersectionManager().getLanes().keySet().iterator();
        while (laneIterator.hasNext()) {
            Integer next = laneIterator.next();
            List<Vehicle> detectedVehicles = intersection.getVehicleManager().getVehiclesInLane(next);
            if (!detectedVehicles.isEmpty()) {
                Iterator<Vehicle> iterator = detectedVehicles.iterator();
                while (iterator.hasNext()) {
                    Vehicle vehicle = iterator.next();
                    if (!dilemmaZoneMatrix.keySet().contains(vehicle.getVehicleID())) {
                        double vehicleSpeed = vehicle.getSpeed();
                        if (vehicleSpeed != 0.0) {
                            VehicleDilemmaZoneData newData = new VehicleDilemmaZoneData();
                            newData.setLaneId(next);
                            newData.setVehicleSpeed(vehicleSpeed);
                            if (vehicle.getSpeed() != 0) {
                                double timeToZone = computeTimeOfArrivalToDilemmaZone(currentTime, 
                                		UtilsInterRep.getDistToStopLine(vehicle, intersection.getIntersectionManager().getLaneById(vehicle.getLaneID())), 
                                		vehicleSpeed, detectedVehicles);
                                newData.setTimeOfArrivalToDilemmaZone(timeToZone);
                            }
                            newData.setVehicleLength(vehicle.getLength());
                            dilemmaZoneMatrix.put(vehicle.getVehicleID(), newData);
                        }
                    }
                }
            }
        }
    }

    /**
     * Calculate the start time of the dilemma zone based on where a
     * a vehicle was detected, the vehicles current speed, and the current time into
     * the algorithm. Also takes slow leading vehicles into account
     *
     * @param currentTime The current time (in seconds) since the start of the algorithm loop.
     * @param feetFromStopLine The distance (in feet) between the vehicle and the stop bar. 
     * @param vehicleSpeed The velocity (in feet per second) of the vehicle. 
     * @return The start time of the dilemma zone in relation to the current time.
     */
    private double computeTimeOfArrivalToDilemmaZone(double currentTime, double feetFromStopLine, double vehicleSpeed, List<Vehicle> otherVehiclesInLane) {
        if (vehicleSpeed == 0.0) {
            throw new IllegalStateException("Cannot compute a dilemma zone with a vehicle speed of 0.0");
        }
        double secondsFromStopLine = feetFromStopLine / vehicleSpeed;
        double secondsFromBeginDilemmaZone = secondsFromStopLine - dilemmaZoneBeginTime;
        Vehicle closestVehicle = null;
        //Finds the closest vehicle in frot of the one being checked
        for (int i = 0; i < otherVehiclesInLane.size(); i++) {
            Vehicle currentVehicle = otherVehiclesInLane.get(i);
            if (UtilsInterRep.getDistToStopLine(currentVehicle, 
            		intersection.getIntersectionManager().getLaneById(currentVehicle.getLaneID())) < feetFromStopLine) {
                if (closestVehicle == null || 
                	UtilsInterRep.getDistToStopLine(currentVehicle, intersection.getIntersectionManager().getLaneById(currentVehicle.getLaneID())) > 
                		UtilsInterRep.getDistToStopLine(closestVehicle, intersection.getIntersectionManager().getLaneById(closestVehicle.getLaneID()))) {
                    closestVehicle = currentVehicle;
                }
            }
        }
        double timeOfArrival = currentTime + secondsFromBeginDilemmaZone;
        if (closestVehicle != null) {
            double closestTimeOfArrival = currentTime + 
            		((UtilsInterRep.getDistToStopLine(closestVehicle, intersection.getIntersectionManager().getLaneById(closestVehicle.getLaneID())) / 
            		closestVehicle.getSpeed()) - dilemmaZoneBeginTime);
            if (timeOfArrival < closestTimeOfArrival) {
                timeOfArrival = currentTime + 
                	(((UtilsInterRep.getDistToStopLine(closestVehicle, intersection.getIntersectionManager().getLaneById(closestVehicle.getLaneID())) + 
                	closestVehicle.getLength()) / closestVehicle.getSpeed()) - dilemmaZoneBeginTime);
            }
        }

        //Vehicle will overtake closest vehicle and needs its time adjusted, changes its arrival time to just after the closest vehicle's
        return timeOfArrival;
    }
}
