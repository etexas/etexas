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

package org.etexascode.apps.dcs;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.etexascode.apps.dcs.model.SignalController;
import org.etexascode.apps.dcs.model.TimeIntervalData;
import org.etexascode.apps.dcs.model.VehicleDilemmaZoneData;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A component used to find the best time to change the signal and actually change the signal.
 * 
 * @author jrutherford
 * @author janway
 */
public class PhaseStatusComponent {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(PhaseStatusComponent.class);

    /** The initialization state of this component. */
    public static final int INIT = -1;

    /** The wait state of this component. */
    public static final int NO_STAGE = 0;

    /** The first signal hold state of this component. */
    public static final int STAGE_1 = 1;

    /** The second signal hold state of this component. */
    public static final int STAGE_2 = 2;

    /** The maximum signal hold state of this component. */
    public static final int MAX_OUT = 3;

    /** Number of seconds to wait before moving algorithm to stage two. */
    private double stage1Timeout = 10.0;

    /** Number of seconds to wait before moving algorithm to max out stage. */
    private double stage2Timeout = 5.0;

    /** The length in feet at which a vehicle is deemed a truck. */
    private double truckLengthThreshold = 25;

    /** The current state of the phase status component. */
    private int stage;

    /** An object used to change/hold signals or get signal information. */
    private SignalController signalController;

    /** The time at which the signal started the green phase. */
    private double greenHoldStartTime;

    /**
     * The length of time in seconds of time intervals for computing BTTEs (best-time-to-end).
     */
    private final double lengthOfTimeIntervalInSeconds = 0.5;

    /**
     * The number of time intervals to use for computing BTTEs (best-time-to-end).
     */
    private final int sizeOfTimeIntervalMatrix = 6;

    // This number seems to only be used in the EGW formula, where it is divided by itself.
    // So we effectively only need the number of cars, and can ignore their lengths.
    // /** Length of cars defined in Bonneson paper. */
    // private final int carLength = 18;

    /** Weight factor for trucks defined in Bonneson paper. */
    private final double weightFactor = 1.2;

    /** Reference to the lane manager. */
    private ILaneManager laneManager;

    /** Reference to the signal manager. */
    private ISignalManager signalManager;

    /** Reference to the vehicle manager. */
    private IVehicleManager vehicleManager;

    /** Reference to the detector manager. */
    private IDetectorManager detectorManager;

    /** The list of lanes on this DCS app. */
    private int laneIDs[];

    /**
     * Constructor.
     * 
     * @param signalController The signal controller.
     * @param laneManager The lane manager.
     * @param signalManager The signal manager.
     * @param vehicleManager The vehicle manager.
     * @param detectorManager The detector manager.
     * @param laneIDs The lane IDs.
     */
    public PhaseStatusComponent(SignalController signalController, ILaneManager laneManager, ISignalManager signalManager, IVehicleManager vehicleManager, IDetectorManager detectorManager,
            int laneIDs[]) {
        this.signalController = signalController;
        this.laneManager = laneManager;
        this.signalManager = signalManager;
        this.vehicleManager = vehicleManager;
        this.detectorManager = detectorManager;
        this.stage = INIT;
        this.greenHoldStartTime = 0.0;
        this.laneIDs = Arrays.copyOf(laneIDs, laneIDs.length);
    }

    /**
     * Update the managers.
     * 
     * @param interrep The interrep model.
     */
    public void updateManagers(InterRepInfoModel interrep) {
        this.laneManager = interrep.lmi;
        this.signalManager = interrep.smi;
        this.vehicleManager = interrep.vmi;
        this.detectorManager = interrep.dmi;
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

        // Enter into a waiting stage if this is the first time through after initialization or max
        // out.
        if (stage == INIT || stage == MAX_OUT) {
            stage = NO_STAGE;
        }

        // Get the time remaining before the signal will change without commands from this
        // algorithm.
        double ttc = signalManager.getSignalsByLaneId(laneIDs[0]).get(0).getTimeToChange();

        // Check to see if there are vehicles in the lane or if the signal controller is holding a
        // green phase.
        boolean vehiclesInLane = false;
        for (int lane : laneIDs) {
            if (vehicleManager.getVehiclesInLane(lane).size() > 0) {
                vehiclesInLane = true;
            }
        }
        if (!vehiclesInLane) {
            return;
        }
        boolean holdingGreen = signalController.isHoldingGreen(laneIDs);

        // If holding a green phase, then the algorithm is in effect.
        if (holdingGreen) {
            if (stage == NO_STAGE) {
                // Note the time at which the signal is held and change the state to stage 1.
                greenHoldStartTime = currentTime;

                // The DCS algorithm should take over once the signal's usual duration has expired.
                // This is complicated by the fact that we're using the microscopic model (subject
                // to message-lag),
                // and the hold command needs to reach TEXAS before it actually changes to yellow.
                // A 1-second buffer is probably enough, so 1.5 to be safe.
                // The only impact is our algorithm could change a signal slightly earlier than the
                // standard one.
                if (ttc <= 1.5) {
                    // Check if there is demand for a conflicting phase. If not, reset the timer and
                    // hold the signal.
                    if (countWaitingPhases() == 0) {
                        greenHoldStartTime = currentTime;
                        LOGGER.debug("Holding phase, no demand.");
                        signalController.holdPhase();
                        return;
                    }
                    else {
                        stage = STAGE_1;
                    }
                }
            }

            // Determine if the stage 2 or maxout should be entered based on the total
            // green phase hold time.
            double totalGreenTime = currentTime - greenHoldStartTime;
            LOGGER.debug("Total Green Time: " + totalGreenTime);
            if ((stage == STAGE_1) && (totalGreenTime >= stage1Timeout)) {
                stage = STAGE_2;
            }
            if ((stage == STAGE_2) && (totalGreenTime >= (stage1Timeout + stage2Timeout))) {
                stage = MAX_OUT;
            }
        }
        else {
            stage = NO_STAGE;
        }

        LOGGER.debug("Hold Green: " + holdingGreen);
        LOGGER.debug("Stage: " + stage);

        // If the max out has been reached, then change the phase immediately.
        if (stage == MAX_OUT) {
            signalController.changePhase();
            return;
        }

        // If the algorithm is active, then compute the EGWs.
        if (stage != NO_STAGE && stage != INIT) {

            // Calculate the EGW for each time interval
            double[] EGWs = new double[sizeOfTimeIntervalMatrix];
            for (int timeIndex = 0; timeIndex < EGWs.length; ++timeIndex) {
                EGWs[timeIndex] = calculateEGW(timeIntervalDataMap, timeIndex, stage);
            }

            // Find the lowest EGW, or determine that no time was a valid candidate
            double minEGW = -1;
            int timeIndexOfMinEGW = -1;
            for (int k = 0; k < EGWs.length; ++k) {
                if (EGWs[k] >= 0.0 && ((EGWs[k] < minEGW) || (minEGW == -1))) {
                    minEGW = EGWs[k];
                    timeIndexOfMinEGW = k;
                }
            }

            // Determine whether or not to end the green phase now or continue to hold.
            LOGGER.debug("EGW Index: " + timeIndexOfMinEGW);
            LOGGER.debug("Min EGW: " + minEGW);
            LOGGER.debug("TTC: " + ttc);
            if (timeIndexOfMinEGW == 0) {
                LOGGER.info("Change Phase Entered");
                signalController.changePhase();
                return;
            }
            else if (ttc <= 1.5) {
                LOGGER.debug("Hold Phase Entered");
                signalController.holdPhase();
                return;
            }
        }
    }

    /**
     * Create a time interval matrix (or histogram of the distribution of dilemma zones over time).
     * 
     * @param currentTime The current time (in seconds) into the algorithm.
     * @param dilemmaZoneMatrix A collection of dilemma zone data.
     * @return A time interval matrix containing a count of vehicle dilemma zones over time
     * intervals.
     */
    private Map<Integer, TimeIntervalData[]> constructTimeIntervalMatrixFromDilemmaZoneMatrix(double currentTime, Collection<VehicleDilemmaZoneData> dilemmaZoneMatrix) {

        Map<Integer, TimeIntervalData[]> timeIntervalDataMap = new HashMap<Integer, TimeIntervalData[]>();
        for (VehicleDilemmaZoneData vehicleDilemmaZoneData : dilemmaZoneMatrix) {
            double timeIntervalIndexStart = Math.round(Math.floor((vehicleDilemmaZoneData.getTimeOfArrivalToDilemmaZone() - currentTime) / lengthOfTimeIntervalInSeconds));
            long timeIntervalIndexEnd = Math.round(Math.floor((vehicleDilemmaZoneData.getTimeOfDepartureFromDilemmaZone() - currentTime) / lengthOfTimeIntervalInSeconds));

            long start = Math.max(0, (long)timeIntervalIndexStart);
            long end = Math.min(sizeOfTimeIntervalMatrix - 1, timeIntervalIndexEnd);

            int laneId = vehicleDilemmaZoneData.getLaneId();

            TimeIntervalData[] timeIntervalData = timeIntervalDataMap.get(laneId);

            if (timeIntervalData == null) {
                // If the map doesn't have a lane matrix, initialize one
                timeIntervalData = new TimeIntervalData[sizeOfTimeIntervalMatrix];
                for (int j = 0; j < timeIntervalData.length; j++) {
                    timeIntervalData[j] = new TimeIntervalData();
                }
                // ...and add it to the map
                timeIntervalDataMap.put(laneId, timeIntervalData);
            }

            for (long i = start; i <= end; i++) {
                // Now add the correct type of vehicle to the lane matrix
                // in the correct time slot
                if (vehicleDilemmaZoneData.getVehicleLength() < truckLengthThreshold) {
                    timeIntervalData[(int)i].addCar(); // cast is safe because 0 <= i <=
                                                       // sizeOfTimeIntervalMatrix (an int)
                }
                else {
                    timeIntervalData[(int)i].addTruck();
                }
            }
        }
        return timeIntervalDataMap;
    }

    /**
     * Finds out if the time interval is a candidate BTTEs (best-time-to-end) given the current
     * stage in the algorithm.
     * 
     * @param timeIntervalData A time interval containing a count of vehicle dilemma zones over time
     * intervals.
     * @param stage The current stage in the algorithm.
     * @return True if the interval data is a candidate BTTE.
     */
    private boolean isCandidateBTTE(TimeIntervalData timeIntervalData, int stage) {

        if (timeIntervalData.getNumberOfTrucks() == 0) {
            if (stage == STAGE_1) {
                if (timeIntervalData.getNumberOfCars() <= 0) {
                    return true;
                }
            }
            else if (stage == STAGE_2) {
                if (timeIntervalData.getNumberOfCars() <= 1) {
                    return true;
                }
            }
        }

        return false;
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
     * Calculates the EGW for a given time interval.
     * 
     * @param tidMap The set of time interval data to use.
     * @param timeIndex The time to calculate the EGW for.
     * @param phase The phase, for determining if a time is a valid candidate.
     * @return The EGW for the given time interval.
     */
    private double calculateEGW(Map<Integer, TimeIntervalData[]> tidMap, int timeIndex, int phase) {
        double result = 0.0;

        for (TimeIntervalData[] tidArray : tidMap.values()) {
            if (isCandidateBTTE(tidArray[timeIndex], phase)) {
                result += calculateEGWForLane(tidArray[timeIndex], timeIndex * lengthOfTimeIntervalInSeconds);
            }
            else {
                return -1;
            }
        }

        return result;
    }

    /**
     * Calculates the EGW for the specified lane
     * 
     * @param timeIntervalData The current Time Interval Data
     * @param t Seconds between present and this time interval.
     * @return The EGW of this lane as a double
     */
    private double calculateEGWForLane(TimeIntervalData timeIntervalData, double t) {
        int conflicting = countWaitingPhases();

        // Calculates and returns EGW based on formula in Bonneson paper
        double output = 0.0;
        output += timeIntervalData.getNumberOfCars();
        output = Math.pow(output, weightFactor);
        output += t * conflicting * 0.1;
        return output;
    }

    /**
     * Counts the number of conflicting phases with at least one vehicle waiting for service.
     * 
     * @return The number of conflicting phases waiting for service.
     */
    private int countWaitingPhases() {
        Set<Integer> conflictingPhases = new HashSet<Integer>();

        // taking phase to mean approach, count the approaches with at least one stopped vehicle
        for (IVehicle veh : vehicleManager) {
            if (veh.getSpeed() <= 0.001) {
                // check if the vehicle is in the intersection (over the stop line)
                if (veh.getLaneID() == 0) {
                    // find the lane it's actually in
                    for (ILane lane : laneManager) {
                        if (UtilsCalculations.getDistance(veh, lane) <= 100.0) {
                            conflictingPhases.add(lane.getApproachId());
                        }
                    }
                }
                else {
                    conflictingPhases.add(laneManager.getLaneById(veh.getLaneID()).getApproachId());
                }
            }

        }

        return conflictingPhases.size();
    }
}