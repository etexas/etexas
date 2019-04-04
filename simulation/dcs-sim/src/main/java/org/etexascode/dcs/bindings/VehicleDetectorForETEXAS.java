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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.Vehicle;

/**
 * A class used to perform detection of vehicles for the DCS Algorithm
 *
 * @author bbadillo
 */
public class VehicleDetectorForETEXAS {

    private Detector vehicleDetector;

    /**
     * Constructor
     *
     * @param feetFromStopLine The distance (in feet) between the detector and the stop bar. 
     * @param vehicleManager An object that notifies the detector of vehicle movement. 
     * @param mapData Lane geometry information to determine distance from the lane stop bars.
     * @param laneId A unique identifier for the lane which this vehicle detector monitors. 
     */
    public VehicleDetectorForETEXAS(Detector inputDetector) {
        Iterator<Integer> laneIDs = inputDetector.getLaneIDs().iterator();
            vehicleDetector.setLaneIDs(inputDetector.getLaneIDs());
            
        // TODO jrutherford this needs to be set or calculated elsewhere.
        //vehicleDetector.setDistToStopLine(inputDetector.getDistToStopLine());

    }

    /**
     * Get the vehicle data for use in creating a dilemma zone matrix.
     *
     * @return A list of vehicle data that contains speed and length information.
     */
    public List<Vehicle> getDetectedVehicles() {

        //Copy the list to return
        List<Vehicle> retList = new LinkedList<Vehicle>();

        //Clear the original list
//        newVehicles.clear();

        //Return the new vehicles
        return retList;
    }

    /**
     * Get the unique lane identifier of the lane that this detector monitors.
     *
     * @return The unique lane identifier of the lane that this detector monitors. 
     */
    public List<Integer> getLaneId() {
        return vehicleDetector.getLaneIDs();
    }
//    /**
//     * Notifies subscribing objects that a new vehicle is being managed.
//     *
//     * @param vehicle The new vehicle that is being managed. 
//     */
//    public void vehicleLogin(Vehicle vehicle) {
//        // Create a new intellidrive handler.
//        IntellidriveHandler intellidrive = new IntellidriveHandler();
//        intellidrive.update(mapData);
//
//        // The Intellidrive listener must be registered before the vehicle listener
//        // so that an intellidrive update occurs immediately.
//        intellidrive.registerListener(this);
//
//        // Hook the intellidrive object to its vehicle.
//        vehicle.registerListener(intellidrive);
//
//        // Add the vehicle to the map.
//        intellidriveMap.put(vehicle.getId(), intellidrive);
//    }
//
//    /**
//     * Notifies subscribing objects that a vehicle is leaving the system.
//     *
//     * @param vehicle The vehicle that is leaving the system.
//     */
//    public void vehicleLogout(Vehicle vehicle) {
//        IntellidriveHandler intellidrive = intellidriveMap.remove(vehicle.getId());
//
//        if (intellidrive != null) {
//            //Remove listeners
//            intellidrive.removeListener(this);
//            vehicle.removeListener(intellidrive);
//        }
//    }
    /**
     * Notifies subscribing objects that an intellidrive object has recieved new
     * information. 
     *
     * @param updatedIntellidrive The intellidrive object that has been updated. 
     */
//    public void intellidriveUpdated(IntellidriveHandler updatedIntellidrive) {
//
//        if (updatedIntellidrive.getLaneId() != laneId) {
//            updatedIntellidrive.removeListener(this);
//        } else if (updatedIntellidrive.getDistanceToIntersection() <= feetFromStopLine) {
//            updatedIntellidrive.removeListener(this);
//
//            Vehicle vehicle = updatedIntellidrive.getVehicle();
////            newVehicles.add(new Vehicle(vehicle.getId(), vehicle.getSpeed(), vehicle.getLength(), feetFromStopLine));
//            newVehicles.add(vehicle);
//        }
//
//    }
}
