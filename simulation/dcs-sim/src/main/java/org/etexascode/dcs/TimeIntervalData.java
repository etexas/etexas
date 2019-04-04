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
 * A structure used to count cars and trucks in a specific time interval. 
 *
 * @author bbadillo
 */
public class TimeIntervalData {
    /**
     * The number of cars in a specific time interval.
     */
    private int numberOfCars = 0;
    /**
     * The number of trucks in a specific time interval.
     */
    private int numberOfTrucks = 0;

    /**
     * Add a car to the car count.
     */
    public void addCar() {
        numberOfCars++;
    }

    /**
     * Add a truck to the truck count. 
     */
    public void addTruck() {
        numberOfTrucks++;
    }

    /**
     * Get the number of cars in the time interval.
     *
     * @return The number of cars in the time interval.
     */
    public int getNumberOfCars() {
        return numberOfCars;
    }

    /**
     * Get the number of trucks in the time interval.
     *
     * @return The number of trucks in the time interval.
     */
    public int getNumberOfTrucks() {
        return numberOfTrucks;
    }
}
