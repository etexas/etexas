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
package org.etexascode.dcs.bindings;

import java.util.HashMap;
import org.etexascode.dcs.VehicleDetector;
import org.etexascode.interrep.datamodel.Vehicle;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * A class used to perform detection of vehicles for the DCS Algorithm
 *
 * @author bbadillo
 */
public class VehicleDetectorForConnectedVehicles implements VehicleDetector {

    /**
     * A collection of vehicles that have been detected.
     */
    private Map<Integer, Vehicle> newVehicles = new HashMap<Integer, Vehicle>();
    /**
     * A unique identifier for the lane which this vehicle detector monitors.
     */
    private int laneId;

    /**
     * Constructor
     *
     * @param laneId A unique identifier for the lane which this vehicle detector monitors.
     */
    public VehicleDetectorForConnectedVehicles(int laneId) {
        this.laneId = laneId;
    }

    /**
     * Get the vehicle data for use in creating a dilemma zone matrix.
     *
     * @return A list of vehicle data that contains speed and length information.
     */
    public List<Vehicle> getDetectedVehicles() {

        //Copy the list to return
        List<Vehicle> retList = new LinkedList<Vehicle>(newVehicles.values());

        //Clear the original list
        newVehicles.clear();

        //Return the new vehicles
        return retList;
    }

    /**
     * Get the unique lane identifier of the lane that this detector monitors.
     *
     * @return The unique lane identifier of the lane that this detector monitors.
     */
    public int getLaneId() {
        return laneId;
    }

    /**
     * Notifies subscribing objects that an intellidrive object has recieved new
     * information.
     *
     * @param updatedIntellidrive The intellidrive object that has been updated.
     */
    public void intellidriveUpdated() {
//            if (updatedIntellidrive.getLaneId() == laneId) {
//                Vehicle vehicle = updatedIntellidrive.getVehicle();
////                    Vehicle dcsVehicle = new Vehicle(vehicle.getId(), vehicle.getSpeed(), vehicle.getLength(), updatedIntellidrive.getDistanceToIntersection());
//                    Vehicle dcsVehicle = new Vehicle();
//                    dcsVehicle.setId(vehicle.getId());
//                    dcsVehicle.setSpeedMPH(vehicle.getSpeedMPH());
//                    dcsVehicle.setLength(vehicle.getLength());
//                    dcsVehicle.setDistToStopLine(vehicle.getDistToStopLine());
//                    newVehicles.put(dcsVehicle.getId(), dcsVehicle);
//            }
            
    }
}
