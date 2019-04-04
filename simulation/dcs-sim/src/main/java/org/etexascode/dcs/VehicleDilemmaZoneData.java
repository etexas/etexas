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

/**
 * A data structure used to hold information pertaining to the dilemma zone for a 
 * vehicle. 
 *
 * @author bbadillo
 */
public class VehicleDilemmaZoneData {

    /**
     * The duration (in seconds) of the dilemma zone. 
     */
    private final double lengthOfDilemmaZoneInSeconds = 3.0;

    /**
     * The unique identifier for the lane in which the vehicle represented by this
     * data resides. 
     */
    private int laneId;

    /**
     * The velocity (in feet per second) at which the vehicle represented by this data
     * is travelling. 
     */
    private double vehicleSpeed;

    /**
     * The time (in seconds) at which the vehicle represented by this data will enter
     * the dilemma zone.
     */
    private double timeOfArrivalToDilemmaZone;

    /**
     * The length (in feet) of the vehicle represented by this data. 
     */
    private double vehicleLength;

    /**
     * Get the unique identifier for the lane in which the vehicle represented by this
     * data resides.
     *
     * @return The unique identifier for the lane in which the vehicle represented by this
     * data resides.
     */
    public int getLaneId() {
        return laneId;
    }

    /**
     * Set the unique identifier for the lane in which the vehicle represented by this
     * data resides.
     *
     * @param laneId The unique identifier for the lane in which the vehicle represented by this
     * data resides.
     */
    public void setLaneId(int laneId) {
        this.laneId = laneId;
    }

    /**
     * Get the velocity (in feet per second) at which the vehicle represented by this data
     * is travelling.
     *
     * @return The velocity (in feet per second) at which the vehicle represented by this data
     * is travelling.
     */
    public double getVehicleSpeed() {
        return vehicleSpeed;
    }

    /**
     * Set the velocity (in feet per second) at which the vehicle represented by this data
     * is travelling.
     *
     * @param speed The velocity (in feet per second) at which the vehicle represented by this data
     * is travelling.
     */
    public void setVehicleSpeed(double speed) {
        this.vehicleSpeed = speed;
    }

    /**
     * Get the time (in seconds) at which the vehicle represented by this data will enter
     * the dilemma zone.
     *
     * @return The time (in seconds) at which the vehicle represented by this data will enter
     * the dilemma zone.
     */
    public double getTimeOfArrivalToDilemmaZone() {
        return timeOfArrivalToDilemmaZone;
    }

    /**
     * Set the time (in seconds) at which the vehicle represented by this data will enter
     * the dilemma zone.
     *
     * @param timeOfArrivalToDilemmaZone The time (in seconds) at which the vehicle represented by this data will enter
     * the dilemma zone.
     */
    public void setTimeOfArrivalToDilemmaZone(double timeOfArrivalToDilemmaZone) {
        this.timeOfArrivalToDilemmaZone = timeOfArrivalToDilemmaZone;
    }

    /**
     * Get the length (in feet) of the vehicle represented by this data.
     *
     * @return The length (in feet) of the vehicle represented by this data.
     */
    public double getVehicleLength() {
        return vehicleLength;
    }

    /**
     * Set the length (in feet) of the vehicle represented by this data.
     *
     * @param length The length (in feet) of the vehicle represented by this data.
     */
    public void setVehicleLength(double length) {
        this.vehicleLength = length;
    }

    /**
     * Get the time (in seconds) at which the vehicle represented by this data will exit
     * the dilemma zone.
     *
     * @return The time (in seconds) at which the vehicle represented by this data will exit
     * the dilemma zone.
     */
    public double getTimeOfDepartureFromDilemmaZone() {
        return this.timeOfArrivalToDilemmaZone+lengthOfDilemmaZoneInSeconds;
    }
}
