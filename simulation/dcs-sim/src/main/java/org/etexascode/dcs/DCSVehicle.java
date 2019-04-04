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
 * A data structure to contain vehicle information necessary for the D-CS Algorithm. 
 *
 * @author bbadillo
 */
public class DCSVehicle {

    /**
     * The unique identifier of the vehicle.
     */
    protected int id;
    /**
     * Distance from the lane stop line (in feet) at which this vehicle was detected.
     */
    protected double detectionDistance;
    /**
     * The speed (in feet per second) of this vehicle.
     */
    protected double speed;
    /**
     * The length (in feet) of this vehicle. 
     */
    protected double length;

    /**
     * Constructor
     *
     * @param id The unique identifier of the vehicle.
     * @param speed The speed (in feet per second) of this vehicle.
     * @param length The length (in feet) of this vehicle.
     * @param detectionDistance Distance from the lane stop line (in feet) at which this vehicle was detected.
     */
    public DCSVehicle(int id, double speed, double length, double detectionDistance) {
        this.id = id;
        this.speed = speed;
        this.length = length;
        this.detectionDistance = detectionDistance;
    }

    /**
     * Get the unique identifier of the vehicle.
     *
     * @return The unique identifier of the vehicle.
     */
    public int getId() {
        return id;
    }

    /**
     * Get the speed (in feet per second) of this vehicle.
     *
     * @return The speed (in feet per second) of this vehicle.
     */
    public double getSpeed() {
        return speed;
    }

    /**
     * Get the length (in feet) of this vehicle.
     *
     * @return The length (in feet) of this vehicle.
     */
    public double getLength() {
        return length;
    }

    /**
     * Get the distance from the lane stop line (in feet) at which this vehicle was detected.
     *
     * @return The distance from the lane stop line (in feet) at which this vehicle was detected.
     */
    public double getDetectionDistance() {
        return detectionDistance;
    }
    
}
