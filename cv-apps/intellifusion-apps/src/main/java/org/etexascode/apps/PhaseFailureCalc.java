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
package org.etexascode.apps;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class for calculating phase failures. Expected Usage: The constructor requires the real
 * LaneManager from the underlying model because the class needs the real speed limit which is not
 * in the MapData message. The update method expects only generated data. The processQueue function
 * may be used to estimate the number of vehicles which will fail in a queue before the light
 * actually changes. processQueue is expected to be used to alter light timings to decrease the
 * number of phase failures. Other methods are provided for the event in which the user wishes to
 * test the reasoning behind our processQueue function.
 * 
 * @author ablatt
 */
public class PhaseFailureCalc {

    /**
     * Static logger
     */
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(PhaseFailureCalc.class);

    /*
     * Units to check: 1) Units of the queue lengths coming in 2) Units of the speed limits 3)
     * Vehicle lengths
     */
    /**
     * enum for keeping track of light colors.
     * 
     * @author ablatt
     */
    public enum Color {

        GREEN,
        RED,
        YELLOW
    };

    /**
     * Key: Lane Id Value: Speed limit for that lane
     */
    Map<Integer, Double> speedLimits;

    /**
     * Key: Lane Id Value: Queue Length at the last green light for that lane
     */
    Map<Integer, Double> greenLightQueueLens;

    /**
     * Key: Lane Id Value: Sim time at the last green light for that lane
     */
    Map<Integer, Double> greenLightSimTimes;

    /**
     * The sim time of the last step.
     */
    double prevSimTime;

    /**
     * The number of light cycles in which no SPAT was received on a red light (meaning we do not
     * know when the green started).
     */
    int numLightCycleFailures = 0; // This contains the number of light cycles in which no SPAT was
                                   // received on a red light (meaning we do not know when the green
                                   // started).

    /**
     * A running count of the total number of phase failures which have occurred.
     */
    int numPhaseFailure = 0;

    Map<Integer, Boolean> hasBeenGreen = new HashMap<Integer, Boolean>();

    // http://www.ops.fhwa.dot.gov/publications/weatherempirical/sect3.htm -- puts jam density at
    // about 1 vehicle every 8.6 meters.
    /**
     * Constructor. Pass in the real lane manager so we can get a hold of the speed limits for each
     * lane.
     * 
     * @param realManager The real lane manager.
     */
    public PhaseFailureCalc(ILaneManager realManager) {
        speedLimits = new HashMap<Integer, Double>();
        greenLightQueueLens = new HashMap<Integer, Double>();
        greenLightSimTimes = new HashMap<Integer, Double>();

        for (Integer i : realManager.getLaneIds()) {
            speedLimits.put(i, realManager.getLaneById(i).getSpeedLimitInMetersPerSecond());
            hasBeenGreen.put(i, false);
        }
    }

    /**
     * Constructor.
     * 
     * @param speedLims A map of the speed limits keyed by laneId.
     */
    public PhaseFailureCalc(Map<Integer, Double> speedLims) {
        this.speedLimits = speedLims;
        greenLightQueueLens = new HashMap<Integer, Double>();
        greenLightSimTimes = new HashMap<Integer, Double>();

        for (Integer i : speedLimits.keySet()) {
            // speedLimits.put(i, realManager.getLaneInfoById(i).getSpeedLimitInMetersPerSecond());
            hasBeenGreen.put(i, false);
        }
    }

    /**
     * Call this method to get the final measure of phase failures.
     * 
     * @param logger The logger.
     */
    public void onDestroy(AppLogger logger) {
        logger.log("Total Phase Failures", numPhaseFailure + "");
    }

    public int getNumPhaseFailure() {
        return this.numPhaseFailure;
    }

    public Map<Integer, Integer> update(Map<Integer, Double> queueLens, Map<String, Set<Integer>> lightChangeInfo, Map<Integer, Double> timeToChange, IVehicleManager vehMan, double simTime) {
        Map<Integer, Integer> ret = new HashMap<Integer, Integer>();

        for (Integer laneId : lightChangeInfo.get("GREEN-GREEN")) {
            // TODO ttevendale - 7/3/2017 - figure out what the intent of this call was.
            // onGreen(timeToChange, laneId, simTime);
        }

        for (Integer laneId : lightChangeInfo.get("GREEN-YELLOW")) {
            fromGreen(laneId, simTime);
        }

        for (Integer laneId : lightChangeInfo.get("GREEN-RED")) {
            fromGreen(laneId, simTime);
        }

        for (Integer laneId : lightChangeInfo.get("RED-GREEN")) {
            toGreen(laneId, queueLens, simTime);
        }

        for (Integer laneId : lightChangeInfo.get("YELLOW-GREEN")) {
            toGreen(laneId, queueLens, simTime);
        }

        return ret;
    }

    int onGreen(Map<Integer, Double> timeToChange, Integer laneId, double simTime) {

        if (timeToChange.get(laneId) == null) {
            return 0;
        }
        else if (timeToChange.get(laneId) == 0.0) {
            return 0;
        }
        else if (greenLightQueueLens.get(laneId) == null || greenLightSimTimes.get(laneId) == null) {
            return 0;
        }
        else {
            return processQueue(greenLightQueueLens.get(laneId), greenLightSimTimes.get(laneId), simTime + timeToChange.get(laneId), speedLimits.get(laneId));
        }
    }

    void toGreen(Integer laneId, Map<Integer, Double> queueLens, double simTime) {
        greenLightQueueLens.put(laneId, queueLens.get(laneId));
        greenLightSimTimes.put(laneId, simTime);
        hasBeenGreen.put(laneId, true);
    }

    void fromGreen(Integer laneId, double simTime) {
        if (hasBeenGreen.get(laneId)) {
            Double greenLightQueueLen = greenLightQueueLens.get(laneId);
            double greenSimTime = greenLightSimTimes.get(laneId);
            double speedLimit = speedLimits.get(laneId);
            int numSlipped = processQueue(greenLightQueueLen, greenSimTime, simTime, speedLimit);// ,
                                                                                                 // aveVehicleLen);

            if (numSlipped > 0) {
                numPhaseFailure++;
            }

            hasBeenGreen.put(laneId, false);
        }
    }

    /**
     * Estimate how many cars in the queue of the specified queue length slipped the light. Returns
     * 0 if there was no phase failure.
     * 
     * @param queueLength The length of the queue (cm).
     * @param simTimeGreen The sim time of the green light for this light cycle (s).
     * @param simTimeYellow The sim time of the yellow light for this light cycle (s).
     * @param speedLimit The speed limit for this lane (m/s).
     * @return The number of cars expected to slip.
     */
    // public int processQueue(double queueLength, double simTimeGreen, double simTimeYellow, double
    // speedLimit) {
    public int processQueue(double queueLength, double simTimeGreen, double simTimeYellow, double speedLimit) {// ,
                                                                                                               // double
                                                                                                               // aveVehLane)
                                                                                                               // {
        List<Double> carDists = genVehicleDistEstimates(queueLength, 862); // 862 pulled from the
                                                                           // website listed at the
                                                                           // top
        // List<Double> carDists = genVehicleDistEstimates(queueLength, 0, aveVehLane, 61);

        int i = 1;
        int ret = carDists.size();
        for (Double car : carDists) {
            if (calcVehMakesIt(car, speedLimit, simTimeGreen, simTimeYellow, i)) {}
            else {
                return ret - i + 1;
            }

            i++;
        }

        return 0;
    }

    /**
     * Informs you if the car, represented by its distance to the stop line is going to make it over
     * the stop line it is that distance from.
     * 
     * @param distToStopLine Distance to the Stop Line (cm).
     * @param speedLimit The speed limit of the lane (m/s).
     * @param simTimeGreen The sim time of the green light in question.
     * @param simTimeYellow The sim time of the yellow light in question.
     * @param vehicleNum The count of vehicles which were in the queue before and including this one
     * @return True/False
     */
    private boolean calcVehMakesIt(double distToStopLine, double speedLimit, double simTimeGreen, double simTimeYellow, int vehicleNum) {
        return calcOverStopLine(distTraveled(speedLimit, simTimeGreen, simTimeYellow, vehicleNum), distToStopLine);
    }

    /**
     * Comparator which determines if a vehicle would cross the stop line.
     * 
     * @param distTraveledByVehicle
     * @param distToStopLine
     * @return True/False
     */
    private boolean calcOverStopLine(double distTraveledByVehicle, double distToStopLine) {
        return distTraveledByVehicle > distToStopLine;
    }

    /**
     * Estimates the distance a vehicle will travel.
     * 
     * @param speedLimit The speed a vehicle will travel (m/s).
     * @param simTimeGreen The start time of the light cycle (s).
     * @param simTimeYellow The end time of the light cycle (s).
     * @param vehicleNum The place of the vehicle in the queue.
     * @return The distance traveled (cm).
     */
    public double distTraveled(double speedLimit, double simTimeGreen, double simTimeYellow, int vehicleNum) {
        double timeRemaining = simTimeYellow - (simTimeGreen + (vehicleNum * 2.0)) + 3.0; // Note:
                                                                                          // ablatt
                                                                                          // - Info
                                                                                          // taken
                                                                                          // from
                                                                                          // TR-B
                                                                                          // 396 -
                                                                                          // Delay
                                                                                          // Paper.pdf
                                                                                          // from
                                                                                          // Dr.
                                                                                          // Rakah.
        // According to the paper, there is a theoretical 2 second reaction time for each car
        // (including the first car) -- hence the vehicleNum * 2
        // Furthermore, drivers tend to use about 3 seconds into the yellow light before stopping
        // Therefore our typical equation of END - START becomes (yellow start + 3.0) - (green start
        // + vehicle offset)
        // This becomes (yellow start + 3.0) - (green start + (2.0 * vehicle's number))
        return distTraveled(speedLimit, timeRemaining);
    }

    /**
     * Gets the distance an object will travel in a given time.
     * 
     * @param speedLimit Speed limit in m/s
     * @param timeRemaining Time remaining in light cycle in s
     * @return The distance traveled in cm
     */
    public double distTraveled(double speedLimit, double timeRemaining) {
        return UtilsUnitConversion.convertMetersToCentimeters(speedLimit * timeRemaining);
    }

    /**
     * Estimates how many vehicles (and each vehicle's distance to the stop line) are in the queue.
     * 
     * @param queueLength The length of the queue (cm).
     * @param jamDensity The average front bumper to front bumper distances of vehicles (cm).
     * @return A list of vehicle distances from closest to stop line to furthest from stop line.
     */
    public List<Double> genVehicleDistEstimates(double queueLength, double jamDensity) {
        List<Double> ret = new LinkedList<Double>();
        double consumedLength = 0.0;

        while (queueLength > 0.0) {
            ret.add(new Double(consumedLength));
            consumedLength += jamDensity;
            queueLength -= jamDensity;
        }

        return ret;
    }
}
