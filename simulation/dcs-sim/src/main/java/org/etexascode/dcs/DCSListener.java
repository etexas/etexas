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

import java.util.Map;

/**
 * An interface that allows objects to subscribe to D-CS events such as when
 * a "best time to end" computations have been complete, or the D-CS Algorithm
 * state has changed.
 *
 * @author bbadillo
 */
public interface DCSListener {

    /**
     * This method is called when a new time interval matrix is available.
     *
     * @param timeIntervalDataMap A map containing arrays of car and truck counts 
     * at certain time intervals. The map is keyed by the lane ID of the lane for
     * which it is computed.
     */
    public void matrixComputed(Map<Integer, TimeIntervalData[]> timeIntervalDataMap);

    /**
     * This method is called when the "best-time-to-end" has been calculated. 
     *
     * @param BTTE The index of the (best-time-to-end) for a time interval matrix.
     */
    public void btteComputed(Integer BTTE);

    /**
     * This method is called when the state of the algorithm has changed. 
     *
     * @param currentState The current state of the algorithm. 
     */
    public void stateChanged(int currentState);
}
