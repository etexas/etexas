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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A calculator which uses DSRC and Detector data to determine the length of queues (or number of
 * stopped vehicles that are lined up) in each lane. TODO ttevendale 1/4/2018 look into creating
 * unit tests and/or updating this calc to use the new spec
 * 
 * @author bbadillo
 */
public class QueueLengthMOECalc extends BaseMOECalc implements ILaneMOECalc {

    private static class VehicleComparator implements Comparator<IVehicle>, Serializable {

        double x;

        double y;

        VehicleComparator(double x, double y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int compare(IVehicle v1, IVehicle v2) {
            return Double.compare(UtilsCalculations.getDist(v1.getX(), v1.getY(), x, y), UtilsCalculations.getDist(v2.getX(), v2.getY(), x, y));
        }
    }

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(QueueLengthMOECalc.class);

    /**
     * A constant defining the front of queue output name.
     */
    final public static String QUEUE_FRONT = "q-front";

    /**
     * A constant defining the back of queue output name.
     */
    final public static String QUEUE_BACK = "q-back";

    /**
     * A constant defining the length of queue output name.
     */
    final public static String QUEUE_LENGTH = "q-length";

    /**
     * A constant defining the queue delay output name.
     */
    final public static String QUEUE_DELAY = "q-delay";

    /**
     * A constant defining the minimum possible back of queue value output name.
     */
    final public static String QUEUE_BACK_MIN = "q-back-min";

    /**
     * A constant defining the maximum possible back of queue value output name.
     */
    final public static String QUEUE_BACK_MAX = "q-back-max";

    /**
     * A counter for the internal time of the calculator.
     */
    double currTime = 0.0;

    /**
     * A configurable value which specifies the speed that constitutes a stopped vehicle.
     */
    private final double STOPPED_SPEED_THRESHOLD = UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(5.0);

    /**
     * A configurable value which specifies the window of time to within which to perform a flow
     * calculation. NOTE: bbadillo - This magic number represents 20 seconds which is just above the
     * time it takes a vehicle to travel from the detector to the intersection.
     */
    private final double FLOW_CALCULATION_WINDOW = 20.0; // seconds

    /**
     * The intersection model on which to base calculations.
     */
    // private InterRep model;
    private final EstimatedDataModel model;

    /**
     * A collection of information about queues such as the last vehicle stopped.
     */
    private final Map<Integer, QueueInfo> queues = new HashMap<Integer, QueueInfo>();

    /**
     * A collection of information gleaned from detectors.
     */
    private final Map<Integer, DetectorInformation> detectors = new HashMap<Integer, DetectorInformation>();

    /**
     * A collection of outputs per lane.
     */
    private final Set<String> laneOutputs = new HashSet<String>();

    public QueueLengthMOECalc(EstimatedDataModel model) {
        super();

        this.model = model;
    }

    /**
     * Advances the internal time of this stateful class to perform dead reckoning modeling logic.
     * 
     * @param elapsedTime The amount of time to advance (in seconds).
     */
    @Override
    public void advance(double elapsedTime) {
        for (DetectorInformation IDetector : detectors.values()) {
            IDetector.setTime(currTime);
        }
        // TODO: bbadillo - Calculate the jam density in real time.
        double jamDensity = 0.050602; // Vehicles per foot - value calculated by simulation.

        // Fill the output map with min and max queue lengths.
        Iterator<QueueInfo> iterator = queues.values().iterator();
        while (iterator.hasNext()) {
            QueueInfo queue = iterator.next();

            // Update projected queue length with detector data
            DetectorInformation IDetector = detectors.get(queue.getLaneId());
            double waveSpeed = 0.0;
            if (IDetector != null) {
                double flow = IDetector.getFlow();
                submitOutput("Flow", IDetector.getLaneId(), flow);
                double density = flow / IDetector.getTimeMeanSpeed();
                submitOutput("Density", IDetector.getLaneId(), density);
                double densityDiff = density - jamDensity;
                if (densityDiff != 0.0) {
                    waveSpeed = UtilsUnitConversion.convertFeetPerSecondToCentimetersPerSecond(Math.abs(flow / densityDiff)); // ft/sec
                    submitOutput("Queue Wave Speed", IDetector.getLaneId(), waveSpeed);
                }
            }

            // Now update queue length data.
            Double estMaxDist = null;

            // Determine minimum queue length using the last joining vehicle in the queue.
            if (queue.getJoining() != null) {
                IVehicle tmp = queue.getJoining();
                double minDist = UtilsCalculations.calcDistToStopLine(tmp, model.getLaneManager().getLaneById(tmp.getLaneID())); // cm
                estMaxDist = minDist + (currTime - queue.getJoinTime()) * queue.getMaxGrowthRate(); // cm
                if (queue.getJoinTime() == currTime) {
                    submitOutput("Join", queue.getLaneId(), minDist);
                    // Project the queue length to see how closely it matches with
                    // the actual queue length.
                    queue.grow(elapsedTime * waveSpeed);
                    double projectedQueue = queue.getProjectedQueue();
                    submitOutput("Projected Q Error", queue.getLaneId(), minDist - projectedQueue);

                    queue.setProjectedQueue(minDist); // cm

                }
                else {
                    queue.grow(elapsedTime * waveSpeed);
                }

                LOGGER.debug("Queue length min distance :{}, projected length : {},  current time: {}, lane id: {} ", minDist, queue.getProjectedQueue(), currTime, queue.getLaneId());

                submitOutput(QUEUE_BACK, queue.getLaneId(), queue.getProjectedQueue());
                submitOutput(QUEUE_BACK_MIN, queue.getLaneId(), minDist);
            }
            else { // joining == null
                queue.grow(elapsedTime * waveSpeed);
                if (QueueInfo.FULL_RED.equals(queue.getMode()) && currTime == queue.getChangeTime()) {
                    // TODO: bbadillo - This should be reset to the known queue value if any.
                    queue.setProjectedQueue(0.0);
                }
                submitOutput(QUEUE_BACK, queue.getLaneId(), queue.getProjectedQueue());
                submitOutput(QUEUE_BACK_MIN, queue.getLaneId(), 0.0d);
            }

            // Set a maximum queue length based on the approaching vehicle.
            if (queue.getApproaching() != null) {
                IVehicle tmp = queue.getApproaching();
                double absMaxDist = UtilsCalculations.calcDistToStopLine(tmp, model.getLaneManager().getLaneById(tmp.getLaneID())); // cm

                // Also estimate the max queue length based on maximum queue growth.
                if (estMaxDist != null) {
                    if (estMaxDist > absMaxDist) {
                        estMaxDist = absMaxDist;
                    }
                    submitOutput(QUEUE_BACK_MAX, queue.getLaneId(), estMaxDist);
                }
                else {
                    submitOutput(QUEUE_BACK_MAX, queue.getLaneId(), absMaxDist);
                }
            }
            else if (estMaxDist != null) {
                submitOutput(QUEUE_BACK_MAX, queue.getLaneId(), estMaxDist);
            }

            // Determine queue discharge when the signal is green.
            if (!QueueInfo.FULL_RED.equals(queue.getMode())) {
                // Construct the macroscopic discharge model
                // TODO: janway - this was originally 7.5 feet per second, or about 5.11 mph.
                // Faster than the stopped speed threshold of 5 mph. No way the queue discharges
                // that fast.
                // Changed to half the threshold. Is there a more legit way to determine this rate?
                double dischargeRate = 111.76; // cm / sec
                if (currTime > queue.getChangeTime() + 2.0) { // 2 second delay for drivers to
                                                              // register the light turned green
                    queue.discharge(elapsedTime * dischargeRate);
                }

                // Correct model using the last known departing vehicle of the queue.
                if (queue.getDeparting() != null) {
                    double departTime = queue.getDepartTime();
                    IVehicle tmp = queue.getDeparting();
                    if (currTime == departTime) {
                        submitOutput("Depart", queue.getLaneId(), UtilsCalculations.calcDistToStopLine(tmp, model.getLaneManager().getLaneById(tmp.getLaneID())));
                        queue.setProjectedDischarge(UtilsCalculations.calcDistToStopLine(tmp, model.getLaneManager().getLaneById(tmp.getLaneID()))); // cm
                    }
                    submitOutput("Last Depart", queue.getLaneId(), UtilsCalculations.calcDistToStopLine(tmp, model.getLaneManager().getLaneById(tmp.getLaneID())));
                }
                submitOutput(QUEUE_FRONT, queue.getLaneId(), queue.getProjectedDischarge());
            }
            else {
                queue.setProjectedDischarge(0.0);
                submitOutput(QUEUE_FRONT, queue.getLaneId(), 0.0d);
            }
            // Determine queue length
            double qLen = queue.getProjectedQueue() - queue.getProjectedDischarge();
            if (qLen < 0.0) {
                qLen = 0.0;
            }
            submitOutput(QUEUE_LENGTH, queue.getLaneId(), qLen);
            // Determine total travel time
            queue.setTotalTravelTime(queue.getTotalTravelTime() + qLen);
            submitOutput(QUEUE_DELAY, queue.getLaneId(), queue.getTotalTravelTime());
            // Reset the total travel time calculation if the signal just
            // changed red.
            if (QueueInfo.FULL_RED.equals(queue.getMode())) {
                if (currTime == queue.getChangeTime()) {
                    queue.setTotalTravelTime(0.0);
                }
            }
        }

        // Advance the internal clock.
        currTime = currTime + elapsedTime;

        // LOGGER.info("\n");

        // Notify listeners that the output map has changed.
        // fireMOEChangeEvent(null, null);

    }

    /**
     * Kicks off the queue length calculation for a specific lane using detector data as the basis
     * for calculation.
     * 
     * @param laneId The lane identifier of the lane to recalculate.
     */
    private void calcQueueInfoFromDetectors(int laneId) {
        // Get current information about the detector and create info if not available.
        DetectorInformation IDetector = detectors.get(laneId);
        if (IDetector == null) {
            return;
        }

        // Get the last detector in the lane (furthest from intersection), and
        // count vehicles passing over them.
        @SuppressWarnings("unchecked")
        List<IDetector> detectorList = (List<IDetector>)model.getDetectorManager().getDetectorFromLaneId(laneId);
        if (detectorList != null && !detectorList.isEmpty()) {
            IDetector lastDetector = detectorList.get(detectorList.size() - 1);
            if (lastDetector.isPulseDetectCap() && lastDetector.getDetEvent() != null) {
                IDetector.add(lastDetector.getDetEvent().getPulse());
            }
            else {
                // TODO: ablatt - fill in
            }
            if (lastDetector.isSpeedDetectCap()) {
                // TODO: bbadillo - Add speed from detector to calculate space-mean speed.
            }
            else {
                // TODO: ablatt - fill in
            }
        }
        else {
            // TODO: ablatt - fill in
        }
    }

    /**
     * Kicks off the queue length calculation for a specific lane using vehicle positions as the
     * basis for calculation.
     * 
     * @param laneId The lane identifier of the lane to recalculate.
     */
    private void calcQueueInfoFromVehiclePos(int laneId) {

        // Get all the vehicles in a specific lane.
        @SuppressWarnings("unchecked")
        List<IVehicle> vehicles = (List<IVehicle>)model.getVehicleManager().getVehiclesInLane(laneId);

        // Get current information about the queue and create info if not available.
        QueueInfo queue = queues.get(laneId);
        if (queue == null) {
            return;
        }

        // Sort vehicles by distance to intersection
        ILane lane = model.getLaneManager().getLaneById(laneId);
        @SuppressWarnings("unchecked")
        List<ILaneNode> nodes = (List<ILaneNode>)lane.getLaneGeomList();
        if (nodes.size() > 0) {
            List<IVehicle> tmpDefClone = new ArrayList<IVehicle>(vehicles.size());
            for (IVehicle vi : vehicles) {
                tmpDefClone.add(vi);
            }
            vehicles = tmpDefClone;
            Collections.sort(vehicles, new VehicleComparator(nodes.get(0).getX(), nodes.get(0).getY()));
        }

        // Find the last stopped BSM sensed vehicle in the lane and the following
        // moving vehicle after that.
        IVehicle lastStopped = null;
        queue.setApproaching(null);
        for (int i = vehicles.size() - 1; i >= 0; i--) {
            IVehicle vehicle = vehicles.get(i);
            // getSpeed() returns speed in meters/second.
            if (vehicle.getSpeed() <= STOPPED_SPEED_THRESHOLD) {
                lastStopped = vehicle;
                break;
            }
            queue.setApproachTime(currTime);
            queue.setApproaching(vehicle);
        }
        if (lastStopped != null) {
            LOGGER.debug("Lane id: {}, Last stopped vehicle: {}, current time: {}", laneId, lastStopped, currTime);
            LOGGER.debug("Vehicle X: {}, Vehicle Y: {}, Speed: {}", lastStopped.getX(), lastStopped.getY(), lastStopped.getSpeed());
        }

        // Only replace the last stopped vehicle if it is different so that
        // the time of stop is preserved.
        IVehicle joiningQueue = queue.getJoining();
        if (lastStopped == null || joiningQueue == null) {
            if (lastStopped != null || joiningQueue != null) {
                queue.setJoining(lastStopped);
                queue.setJoinTime(currTime);
            }
        }
        else if (lastStopped.getVehicleID() != joiningQueue.getVehicleID()) {
            queue.setJoining(lastStopped);
            queue.setJoinTime(currTime);
        }

        // Find the first BSM sensed vehicle that is departing the queue and
        // the vehicle that is leading the queue.
        IVehicle firstMoving = null;
        IVehicle firstStopped = null;
        if (lastStopped != null) {
            for (int i = 0; i < vehicles.size(); i++) {
                IVehicle vehicle = vehicles.get(i);
                if (vehicle.getSpeed() <= STOPPED_SPEED_THRESHOLD) {
                    firstStopped = vehicle;
                    break;
                }
                firstMoving = vehicle;
            }
        }
        else {
            // If there was an end of queue sensed before but now there isn't then
            // that vehicle must be moving again. In the absence of a departing vehicle
            // this vehicle must be the departing vehicle.
            firstMoving = joiningQueue;
        }

        // Only replace the vehicle departing the queue if it is different so that
        // the time of departure is preserved.
        IVehicle departingQueue = queue.getDeparting();
        if (firstMoving == null || departingQueue == null) {
            if (firstMoving != null || departingQueue != null) {
                queue.setDeparting(firstMoving);
                queue.setDepartTime(currTime);
            }
        }
        else if (firstMoving.getVehicleID() != departingQueue.getVehicleID()) {
            queue.setDeparting(firstMoving);
            queue.setDepartTime(currTime);
        }
        // Only replace the vehicle leading the queue if it is different so that
        // the lead time is preserved.
        IVehicle leadingQueue = queue.getLeading();
        if (firstStopped == null || leadingQueue == null) {
            if (firstStopped != null || leadingQueue != null) {
                queue.setLeading(firstStopped);
                queue.setLeadTime(currTime);
            }
        }
        else if (firstStopped.getVehicleID() != leadingQueue.getVehicleID()) {
            queue.setLeading(firstStopped);
            queue.setLeadTime(currTime);
        }
    }

    /**
     * Listens to detector updates in the intersection model.
     * 
     * @param detectors The detectors that have changed.
     */
    public void detectorChanged(Collection<IDetector> detectors) {
        // Determine the set of lanes that must be recalculated.
        Set<Integer> dirtyLanes = new HashSet<Integer>();
        for (IDetector detector : detectors) {
            for (Integer laneId : detector.getLaneIDs()) {
                dirtyLanes.add(laneId);
            }
        }

        // Recalculate the queue length of dirty lanes.
        for (Integer laneId : dirtyLanes) {
            // Recalculate the queue length of the lane.
            calcQueueInfoFromDetectors(laneId);
        }
    }

    /**
     * The identifier of the calculator.
     * 
     * @return This calculators identifier.
     */
    @Override
    public String getCalcID() {
        return "Queue Length MOE Calculator";
    }

    @Override
    public Set<String> getEnabledLaneOutputs() {
        return laneOutputs;
    }

    @Override
    public Set<Integer> getLanes() {
        return queues.keySet();
    }

    @Override
    public Set<String> getPossibleLaneOutputs() {
        Set<String> retList = new HashSet<String>();
        retList.add(QUEUE_BACK);
        retList.add(QUEUE_FRONT);
        retList.add(QUEUE_LENGTH);
        retList.add(QUEUE_DELAY);
        return retList;
    }

    @Override
    public void registerLane(int laneId) {
        QueueInfo queue = new QueueInfo(laneId);
        queues.put(laneId, queue);

        DetectorInformation det = new DetectorInformation(laneId, FLOW_CALCULATION_WINDOW);
        detectors.put(laneId, det);
    }

    /**
     * Listens to signal updates in the intersection model.
     */
    public void signalUpdated() {
        ISignalManager signalManager = model.getSignalManager();

        // Update the signal indication of managed queues.
        // Iterator<QueueInfo> iterator = queues.values().iterator();
        // while (iterator.hasNext()) {
        for (QueueInfo queue : queues.values()) {
            // QueueInfo queue = iterator.next();
            int laneId = queue.getLaneId();
            List<ISignalIndication> signals = (List<ISignalIndication>)signalManager.getSignalsByLaneId(laneId);
            if ((signals != null) && !signals.isEmpty()) {
                // If there is any GREEN indication for this lane, treat the queue as GREEN.
                ISignalIndication indication = signals.get(0);
                for (int k = 1; k < signals.size(); ++k) {
                    if (!indication.getColorIndication().equals(SignalIndication.Color.GREEN)) {
                        indication = signals.get(k);
                    }
                }

                if (indication.getColorIndication().equals(SignalIndication.Color.RED)) {
                    if (!QueueInfo.FULL_RED.equals(queue.getMode())) {
                        queue.setMode(QueueInfo.FULL_RED);
                        queue.setChangeTime(currTime);
                        queue.setProjectedDischarge(0.0);
                    }
                }
                else {
                    if (!QueueInfo.FLOW.equals(queue.getMode())) {
                        queue.setMode(QueueInfo.FLOW);
                        queue.setChangeTime(currTime);
                        queue.setProjectedDischarge(0.0);
                    }
                }
            }
        }
    }

    /**
     * Update the MOE calculator output map with appropriate values.
     * 
     * @param output The output name to update.
     * @param laneId The lane of the output to update.
     * @param value The value to update.
     */
    void submitOutput(String output, int laneId, double value) {
        String valueStr = Double.toString(value);

        // if (laneOutputs.contains(output)) {
        String key = "Lane " + laneId + ", " + output + " (ft)";
        outputs.put(key, valueStr);
        // }

        // Use this block when re-validating this MOE. Uncomment the corresponding code in
        // InterRep.update().
        // if (laneId == 1 && (
        // output.equals(QUEUE_BACK) ||
        // output.equals(QUEUE_LENGTH) ||
        // output.equals(QUEUE_FRONT)
        // )) {
        // LOGGER.info(key + ": \t\t\t" + valueStr);
        // }
    }

    /**
     * Listens to vehicle changes in the intersection model.
     * 
     * @param vehicles A collection of vehicles that have changed.
     */
    public void vehicleUpdated(Collection<IVehicle> vehicles) {
        // Determine the set of lanes that must be recalculated.
        Set<Integer> dirtyLanes = new HashSet<Integer>();
        for (IVehicle vehicle : vehicles) {
            dirtyLanes.add(vehicle.getLaneID());
        }

        // Recalculate the queue length of dirty lanes.
        for (Integer laneId : dirtyLanes) {
            // Recalculate the queue length of the lane.
            calcQueueInfoFromVehiclePos(laneId);
        }

    }
}