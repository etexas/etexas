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
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;

/**
 * A component used to find the best time to change the signal and actually 
 * change the signal.
 *
 * @author bbadillo
 */
public class PhaseStatusComponent {

    /**
     * The initialization state of this component.
     */
    public static final int INIT = -1;
    /**
     * The wait state of this component.
     */
    public static final int NO_STAGE = 0;
    /**
     * The first signal hold state of this component.
     */
    public static final int STAGE_1 = 1;
    /**
     * The second signal hold state of this component.
     */
    public static final int STAGE_2 = 2;
    /**
     * The maximum signal hold state of this component.
     */
    public static final int MAX_OUT = 3;
    /**
     * Number of seconds to wait before moving algorithm to stage two.
     */
    private double stage1Timeout = 10.0;
    /**
     * Number of seconds to wait before moving algorithm to max out stage.
     */
    private double stage2Timeout = 5.0;
    /**
     * The length in feet at which a vehicle is deemed a truck. 
     */
    private double truckLengthThreshold = 25;
    /**
     * The current state of the phase status component
     */
    private int stage = INIT;
    /**
     * An object used to change/hold signals or get signal information.
     */
    private SignalController signalController;
    /**
     * The time at which the signal started the green phase.
     */
    private double greenHoldStartTime = 0.0;
    /**
     * The length of time in seconds of time intervals for computing BTTEs (best-time-to-end). 
     */
    private final double lengthOfTimeIntervalInSeconds = 0.5;
    /**
     * The number of time intervals to use for computing BTTEs (best-time-to-end).
     */
    private final int sizeOfTimeIntervalMatrix = 6;
    /**
     * Length of cars defined in Bonneson paper
     */
    private final int carLength = 18;
    /**
     * Weight factor for trucks defined in Bonneson paper
     */
    private final double weightFactor = 1.2;
    /**
     * A number indicating the time interval matrix rows to consider when determining when to change phases.
     * BTTEs under the threshold translate to a phase change
     */
    private int changeThreshold = 0;
    /**
     * InterRep object representing the intersection
     */
    private InterRep intersection;

    /**
     * Constructor
     *
     * @param signalController An object used to change/hold signals or get signal information.
     */
    public PhaseStatusComponent(SignalController signalController, InterRep input) {
        this.signalController = signalController;
        this.intersection = input;
    }

    /**
     * Set the changeThreshold value.
     *
     * @param changeThreshold A number indication the time interval matrix rows to
     * consider when determining when to change phases
     */
    public void setChangeThreshold(int changeThreshold) {
        this.changeThreshold = changeThreshold;
    }

    /**
     * Calculate the BTTE (best-time-to-end) signal phases and change signals as needed.
     *
     * @param currentTime The current time (in seconds) into the algorithm. 
     * @param dilemmaZoneMatrix A collection of dilemma zone data. 
     */
    public void performControl(double currentTime, Collection<VehicleDilemmaZoneData> dilemmaZoneMatrix) {

        // Construct a histogram of vehicle dilemma zones over a range of time.
        Map<Integer, TimeIntervalData[]> timeIntervalDataMap = constructTimeIntervalMatrixFromDilemmaZoneMatrix(currentTime, dilemmaZoneMatrix);
        // Notify listeners that the matrix has been computed.
        fireMatrixComputedEvent(timeIntervalDataMap);

        // Enter into a waiting stage if this is the first time through after initialization
        // or max out. 
        if (stage == INIT || stage == MAX_OUT) {
            // Notify listeners if a waiting stage is entered.
            changeState(NO_STAGE);
        }

        // Check to see if the signal controller is holding a green phase.
        boolean holdingGreen = signalController.isHoldingGreen();

        if (holdingGreen) {
            // If holding a green phase, then the algorithm is in effect.
            if (stage == NO_STAGE) {
                // Note the time at which the signal is held and change the state to stage 1. 
                greenHoldStartTime = currentTime;
                changeState(STAGE_1);
            }

            // Determine if the stage 2 or maxout should be entered base on the total
            // green phase hold time. 
            double totalGreenTime = currentTime - greenHoldStartTime;

            if ((stage == STAGE_1) && (totalGreenTime >= stage1Timeout)) {
                changeState(STAGE_2);
            }
            if ((stage == STAGE_2) && (totalGreenTime >= (stage1Timeout + stage2Timeout))) {
                changeState(MAX_OUT);
            }
        } else {
            // If not holding green, then the algorithm is not active. 
            // Notify listeners if a waiting stage is entered.
            changeState(NO_STAGE);
        }

        if (stage == MAX_OUT) {
            // If the max out has been reached, then change the phase immediately. 
            signalController.changePhase();
        } else if (stage != NO_STAGE && stage != INIT) {
            // If the algorithm is active, then compute the BTTEs
            Set<Entry<Integer, TimeIntervalData[]>> entrySet = timeIntervalDataMap.entrySet();

            int lowestBTTEIndex = -1;
            int lowestBTTETotalCarValue = Integer.MAX_VALUE;
            int endGreenWeight = Integer.MAX_VALUE;

            for (int i = 0; i < sizeOfTimeIntervalMatrix; i++) {
                boolean candidate = true;
                int totalCarValue = 0;

                double possibleEGW = Integer.MAX_VALUE;

                Iterator<Entry<Integer, TimeIntervalData[]>> iterator = entrySet.iterator();
                //Iterate over all lanes governed by the algorithm and determine the BTTE.
                while (iterator.hasNext()) {
                    Entry<Integer, TimeIntervalData[]> nextEntry = iterator.next();
                    TimeIntervalData[] timeIntervalData = nextEntry.getValue();
                    if (isCandidateBTTE(timeIntervalData[i])) {
                        double tempEGW = calculateEGW(timeIntervalData[i], nextEntry.getKey());
                        if (tempEGW < possibleEGW) {
                            possibleEGW = tempEGW;
                        }
                        totalCarValue = totalCarValue + timeIntervalData[i].getNumberOfCars() + timeIntervalData[i].getNumberOfTrucks();
                    } else {
                        candidate = false;
                        break;
                    }
                }
                if (candidate && (totalCarValue < lowestBTTETotalCarValue) && (possibleEGW < endGreenWeight)) {
                    lowestBTTEIndex = i;
                    lowestBTTETotalCarValue = totalCarValue;
                }
            }

            // Notify listeners that BTTEs have been computed.
            if (lowestBTTEIndex >= 0) {
                // Notify listeners that BTTEs have been computed.
                fireBtteComputedEvent(lowestBTTEIndex);
            } else {
                // Notify listeners that BTTEs have been computed.
                fireBtteComputedEvent(null);
            }



            // Determine whether or not to end the green phase now or continue to hold. 
            if (0 <= lowestBTTEIndex && lowestBTTEIndex <= changeThreshold) {
                signalController.changePhase();
            } else {
                signalController.holdPhase();
            }
        }
    }

    /**
     * Create a time interval matrix (or histogram of the distribution of dilemma zones over time).
     *
     * @param currentTime The current time (in seconds) into the algorithm. 
     * @param dilemmaZoneMatrix A collection of dilemma zone data.
     * @return A time interval matrix containing a count of vehicle dilemma zones over time intervals. 
     */
    private Map<Integer, TimeIntervalData[]> constructTimeIntervalMatrixFromDilemmaZoneMatrix(double currentTime, Collection<VehicleDilemmaZoneData> dilemmaZoneMatrix) {

        Map<Integer, TimeIntervalData[]> timeIntervalDataMap = new HashMap<Integer, TimeIntervalData[]>();

        Iterator<VehicleDilemmaZoneData> iterator = dilemmaZoneMatrix.iterator();
        while (iterator.hasNext()) {
            VehicleDilemmaZoneData vehicleDilemmaZoneData = iterator.next();

            double timeIntervalIndexStart = Math.round(Math.floor((vehicleDilemmaZoneData.getTimeOfArrivalToDilemmaZone() - currentTime) / lengthOfTimeIntervalInSeconds));
            long timeIntervalIndexEnd = Math.round(Math.floor((vehicleDilemmaZoneData.getTimeOfDepartureFromDilemmaZone() - currentTime) / lengthOfTimeIntervalInSeconds));
            for (int i = (int) timeIntervalIndexStart; i <= timeIntervalIndexEnd; i++) {
                if (0 <= i && i < sizeOfTimeIntervalMatrix) {
                    //If the vehicle is found to be in the look ahead interval,
                    //then we need to put it in the correct lane matrix.
                    int laneId = vehicleDilemmaZoneData.getLaneId();

                    TimeIntervalData[] timeIntervalData = timeIntervalDataMap.get(laneId);

                    if (timeIntervalData == null) {
                        //If the map doesn't have a lane matrix, initialize one
                        timeIntervalData = new TimeIntervalData[sizeOfTimeIntervalMatrix];
                        for (int j = 0; j < timeIntervalData.length; j++) {
                            timeIntervalData[j] = new TimeIntervalData();
                        }
                        //...and add it to the map
                        timeIntervalDataMap.put(laneId, timeIntervalData);
                    }

                    //Now add the correct type of vehicle to the lane matrix
                    //in the correct time slot
                    if (vehicleDilemmaZoneData.getVehicleLength() < truckLengthThreshold) {
                        timeIntervalData[i].addCar();
                    } else {
                        timeIntervalData[i].addTruck();
                    }
                }
            }
        }

        return timeIntervalDataMap;
    }

    /**
     * Finds out if the time interval is a candidate BTTEs (best-time-to-end)
     * given the current stage in the algorithm.
     *
     * @param timeIntervalData A time interval containing a count of vehicle dilemma zones over time intervals.
     * @return True if the interval data is a candidate BTTE.
     */
    private boolean isCandidateBTTE(TimeIntervalData timeIntervalData) {

        if (timeIntervalData.getNumberOfTrucks() == 0) {
            if (stage == STAGE_1) {
                if (timeIntervalData.getNumberOfCars() <= 0) {
                    return true;
                }
            } else if (stage == STAGE_2) {
                if (timeIntervalData.getNumberOfCars() <= 1) {
                    return true;
                }
            }
        }

        return false;
    }
    /**
     * A collection of subscribing objects interested in the D-CS Algorithm.
     */
    protected Collection<DCSListener> listeners = new LinkedList<DCSListener>();

    /**
     * Register a subscribing object interested in  D-CS updates.
     *
     * @param listener The subscribing object to register.
     */
    public void registerListener(DCSListener listener) {
        listeners.add(listener);
    }

    /**
     * Remove a subscribing object from the collection of listeners. 
     *
     * @param listener The subscribing object to remove.
     */
    public void removeListener(DCSListener listener) {
        listeners.remove(listener);
    }

    /**
     * Notify listeners that a new time interval matrix has been computed.
     *
     * @param timeIntervalDataMap A time interval matrix containing a count of vehicle dilemma zones over time intervals.
     */
    protected void fireMatrixComputedEvent(Map<Integer, TimeIntervalData[]> timeIntervalDataMap) {
        Iterator<DCSListener> iterator = listeners.iterator();
        while (iterator.hasNext()) {
            DCSListener listener = iterator.next();
            listener.matrixComputed(timeIntervalDataMap);
        }
    }

    /**
     * Notify listeners that a new BTTE (best-time-to-end) has been computed.
     *
     * @param BTTE The index of the (best-time-to-end) for a time interval matrix.
     * A BTTE value of 0 indicates that the BTTE is now.
     */
    protected void fireBtteComputedEvent(Integer BTTE) {
        Iterator<DCSListener> iterator = listeners.iterator();
        while (iterator.hasNext()) {
            DCSListener listener = iterator.next();
            listener.btteComputed(BTTE);
        }
    }

    /**
     * Change the state of the algorithm and notify all listeners of change. 
     *
     * @param newStage The new state to change to. 
     */
    protected void changeState(int newStage) {
        stage = newStage;

        Iterator<DCSListener> iterator = listeners.iterator();
        while (iterator.hasNext()) {
            DCSListener listener = iterator.next();
            listener.stateChanged(stage);
        }
    }

    /**
     * Set the number of seconds to wait before moving algorithm to stage two.
     *
     * @param stage1Timeout Number of seconds to wait before moving algorithm to stage two.
     */
    protected void setStage1Timeout(double stage1Timeout) {
        this.stage1Timeout = stage1Timeout;
    }

    /**
     * Set the number of seconds to wait before moving algorithm to max out stage.
     *
     * @param stage2Timeout Number of seconds to wait before moving algorithm to max out stage.
     */
    protected void setStage2Timeout(double stage2Timeout) {
        this.stage2Timeout = stage2Timeout;
    }

    /**
     * Get the length (in seconds) of each time interval in the time interval matrix. 
     * 
     * @return The length (in seconds) of each time interval in the time interval matrix. 
     */
    double getTimeInterval() {
        return lengthOfTimeIntervalInSeconds;
    }

    /**
     * Get the current state of the phase status component.
     *
     * @return The current state of the phase status component.
     */
    int getState() {
        return stage;
    }

    /**
     * Calculates the EGW for the specified lane
     * @param timeIntervalData The current Time Interval Data
     * @param laneID ID of the lane
     * @return The EGW of this lane as a double
     */
    private double calculateEGW(TimeIntervalData timeIntervalData, int laneID) {
        int approachId = intersection.getIntersectionManager().getLaneById(laneID).getApproachId();
        DetectorManager detectorManager = intersection.getDetectorManager();
        int conflicting = 0;
        //Loops through detectors to find calls for service (other than current approach)
        Iterator<Detector> detectors = detectorManager.getDetectorCollection().iterator();
        while (detectors.hasNext()) {
            Detector nextDetector = detectors.next();
            boolean conflictingPhase = true;
            if (UtilsInterRep.getDistToStopLine(nextDetector, intersection.getIntersectionManager().getLaneById(laneID)) < 25 && 
            		nextDetector.isPresenceDetectCap()) {
                Iterator<Integer> detectorLanes = nextDetector.getLaneIDs().iterator();
                Integer next = detectorLanes.next();
                if (intersection.getIntersectionManager().getLaneById(next).getApproachId() == approachId) {
                    conflictingPhase = false;
                }
            }
            if (conflictingPhase) {
                conflicting++;
            }
        }
        VehicleManager vehicleManager = intersection.getVehicleManager();
        int waitingVehicles = 0;
        //Finds out how many vehicles are waiting for service
        Iterator<Integer> vehicleIDs = vehicleManager.getAllVehicleIds().iterator();
        while (vehicleIDs.hasNext()) {
            Vehicle next = vehicleManager.getVehicle(vehicleIDs.next());
            if (next.getSpeed() == 0.0) {
                if (intersection.getIntersectionManager().getLaneById(next.getLaneID()).getApproachId() != approachId) {
                    Iterator<Integer> signals = intersection.getSignalByLaneID(laneID).getKeys().iterator();
                    while (signals.hasNext()) {
                        SignalIndication signalIndicationById = intersection.getSignalByLaneID(laneID).getSignalIndicationById(signals.next());
                        if (signalIndicationById.getColorIndication() != SignalIndication.Color.GREEN) {
                            waitingVehicles++;
                            break;
                        }
                    }

                }
            }
        }

        //Calculates and returns EGW based on formula in Bonneson paper
        double output = 0.0;
        output += timeIntervalData.getNumberOfCars() / carLength;
        output = Math.pow(output, weightFactor);
        output += conflicting * (waitingVehicles * 0.1);

        return output;
    }
}