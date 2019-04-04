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
import org.etexascode.interrep.InterRep;

/**
 * A Java implementation of Bonneson's D-CS algorithm
 *
 * @author bbadillo
 */
public class DCSAlgorithm {

    /**
     * A component used to check the status of vehicles and provide dilemma
     * zone information.
     */
    private VehicleStatusComponent vehicleStatusComponent;
    /**
     * A component used to find the best time to change the signal and actually
     * change the signal. 
     */
    private PhaseStatusComponent phaseStatusComponent;

    /**
     * The current time into the algorithm
     */
    private Double prevTime = null;

    /**
     * Constructor
     *
     * @param detectors An array of vehicle detectors on lanes that provide input
     * to the VehicleStatusComponent. 
     * @param controller An object used to change/hold signals or get signal information.
     */
    public DCSAlgorithm(SignalController controller, InterRep input) {
        this.vehicleStatusComponent = new VehicleStatusComponent(input);
        this.phaseStatusComponent = new PhaseStatusComponent(controller, input);
    }

    /**
     * Configure the D-CS Algorithm with parameters. This method should be called
     * before the method to update the algorithm is called. 
     *
     * @param stage1Timeout Number of seconds to wait before moving algorithm to stage two.
     * @param stage2Timeout Number of seconds to wait before moving algorithm to max out stage.
     */
    public void configureTimeouts(double stage1Timeout, double stage2Timeout) {
        phaseStatusComponent.setStage1Timeout(stage1Timeout);
        phaseStatusComponent.setStage2Timeout(stage2Timeout);
    }

    /**
     * Get the length (in seconds) of each time interval in the time interval matrix. 
     * 
     * @return The length (in seconds) of each time interval in the time interval matrix. 
     */
    public double getTimeInterval() {
        return phaseStatusComponent.getTimeInterval();
    }

    /**
     * Get the current state of the phase status component.
     *
     * @return The current state of the phase status component.
     */
    public int getState() {
        return phaseStatusComponent.getState();
    }

    /**
     * Update the algorithm with current information and perform vehicle detection
     * and signal control. 
     *
     * @param currentTime The current time in seconds. 
     */
    public void performDetectionControlLoop(double currentTime) {
        if(prevTime != null) {
            double dt = currentTime-prevTime;
            if(dt > 0.75) {
                phaseStatusComponent.setChangeThreshold(1);
            }
        }
        prevTime = currentTime;

        //Detect vehicles and calculate a list of dilemma zone information.
        vehicleStatusComponent.performDetection(currentTime);
        Collection<VehicleDilemmaZoneData> dilemmaZoneMatrix = vehicleStatusComponent.getDilemmaZoneMatrix();

        //Calculate the best time to end signal phases and change signals as needed. 
        phaseStatusComponent.performControl(currentTime, dilemmaZoneMatrix);
    }

    /**
     * Register a subscribing object interested in D-CS updates
     *
     * @param listener The subscribing object to register
     */
    public void registerListener(DCSListener listener) {
        phaseStatusComponent.registerListener(listener);
    }

    /**
     * Remove a subscribing object from the collection of listeners
     *
     * @param listener The subscribing object to remove
     */
    public void removeListener(DCSListener listener) {
        phaseStatusComponent.removeListener(listener);
    }
}
