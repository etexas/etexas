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

import java.text.NumberFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;

/**
 * A calculator to calculate the percentage of vehicles that are equipped with DSRC versus those
 * that are not. TODO ttevendale 1/4/2018 look into creating unit tests and/or updating this calc to
 * use the new spec
 * 
 * @author bbadillo
 * @author dranker
 * @author ablatt
 */
public class DSRCPercentageMOECalc extends BaseMOECalc implements ILaneMOECalc {

    /**
     * A constant defining the DSRC Percentage output name.
     */
    final public static String PERCENTAGE = "dsrc-percentage";

    /**
     * A counter for the internal time of the calculator.
     */
    double currTime = 0.0;

    /**
     * A map containing a list of vehicle counts over time keyed by lane ID.
     */
    Map<Integer, List<Count>> allVehicles = new HashMap<Integer, List<Count>>();

    /**
     * A map containing the distances of detectors from lane stop bars keyed by lane ID.
     */
    Map<Integer, Double> detectorDistances = new HashMap<Integer, Double>();

    /**
     * A map containing a count of DSRC sensed vehicles keyed by lane ID.
     */
    Map<Integer, List<Count>> dsrcVehicles = new HashMap<Integer, List<Count>>();

    /**
     * A map containing the instantaneous count of newly sensed DSRC vehicles.
     */
    Map<Integer, Count> dsrcVehicleCount = new HashMap<Integer, Count>();

    /**
     * A mapping between vehicle IDs and the lanes in which the vehicles reside (keyed by vehicle
     * ID).
     */
    Map<Integer, Integer> sensedVehicles = new HashMap<Integer, Integer>();

    /**
     * A collection of outputs per lane.
     */
    private Set<String> laneOutputs = new HashSet<String>();

    EstimatedDataModel model;

    /**
     * Default Constructor
     * 
     * @param model The estimated data model.
     */
    public DSRCPercentageMOECalc(EstimatedDataModel model) {
        super();
        this.model = model;
    }

    @CoberturaIgnore
    @Override
    public String getCalcID() {
        return "DSRC Percentage MOE Calculator";
    }

    @Override
    public Set<String> getPossibleLaneOutputs() {
        Set<String> retList = new HashSet<String>();
        retList.add(PERCENTAGE);
        return retList;
    }

    public void detectorChanged(Collection<IDetector> detectors) { // NOTE: app logger only there
                                                                   // for debugging purposes
        // Count vehicles for each lane.
        for (IDetector detector : detectors) {
            List<Integer> laneIds = detector.getLaneIDs();

            Double dist = UtilsCalculations.getDistance(detector, model.getLaneManager().getLaneById(laneIds.get(0)));
            Iterator<Integer> iterator = detector.getLaneIDs().iterator();
            while (iterator.hasNext()) {
                Integer laneID = iterator.next();
                if (allVehicles.containsKey(laneID)) {
                    // Save the detector distance to filter out DSRC percentages.
                    detectorDistances.put(laneID, dist); // Note: ablatt - this distance might be
                                                         // used semantically elsewhere...
                    List<Count> countForLane = allVehicles.get(laneID);
                    // if (detector.isPulseDetectCap()) {
                    if (detector.getDetEvent() != null) {
                        Count count = new Count();
                        count.count = detector.getDetEvent().getPulse();
                        countForLane.add(count);
                    }
                    // } else {
                    // TODO: ablatt - what if the detector is not a pulse detect cap?
                    // }
                }
                else {
                    // TODO: ablatt - what if allVehicles doesn't have the laneID? Is that case
                    // possible? (If not, we should delete the if statement)
                    // Shouldn't we at least log the outcome?
                }
            }

        }
    }

    public void vehicleUpdated(Collection<IVehicle> vehicles) {
        // Count DSRC vehicles for each lane.
        for (IVehicle vehicle : vehicles) {
            int laneID = vehicle.getLaneID();
            if (dsrcVehicles.containsKey(laneID) && !sensedVehicles.containsKey(vehicle.getVehicleID())) {
                // Make sure the vehicle is within the detection zone
                Double dist = detectorDistances.get(laneID); // Note: ablatt - it looks like this
                                                             // value should already be in cm
                // if (dist != null && vehicle.getDistToStopLine() <= dist) {
                if (dist != null && UtilsCalculations.calcDistToStopLine(vehicle, model.getLaneManager().getLaneById(vehicle.getLaneID())) <= dist) {
                    // Count the newly entered vehicle
                    Count countForLane = dsrcVehicleCount.get(laneID);
                    if (countForLane == null) {
                        countForLane = new Count();
                        dsrcVehicleCount.put(laneID, countForLane);
                    }
                    countForLane.count += 1;

                    // Record the vehicle
                    sensedVehicles.put(vehicle.getVehicleID(), vehicle.getLaneID());
                }
                else {
                    // TODO: ablatt - I assume we should just ignore this?
                    // -- shouldn't we log something if dist is null?
                    // -- does intellifusion care if the vehicle exists but is not "queued"?
                }
            }
            else {
                // TODO: ablatt - fill in
            }

        }
    }

    /**
     * Advances the internal time of this stateful class.
     * 
     * @param elapsedTime The amount of time to advance (in seconds).
     */
    @Override
    public void advance(double elapsedTime) {

        // Advance the internal clock.
        currTime = currTime + elapsedTime;

        // Calculate the DSRC count for each lane.
        Set<Entry<Integer, Count>> countEntrySet = dsrcVehicleCount.entrySet();
        Iterator<Entry<Integer, Count>> entryIter = countEntrySet.iterator();
        while (entryIter.hasNext()) {
            Entry<Integer, Count> entry = entryIter.next();
            Integer laneID = entry.getKey();
            Count count = entry.getValue();
            List<Count> countForLane = dsrcVehicles.get(laneID);
            if (countForLane == null) {
                countForLane = new LinkedList<Count>();
                dsrcVehicles.put(laneID, countForLane);
            }
            countForLane.add(count);
        }
        dsrcVehicleCount.clear();

        // Calculate the DSRC percentage of each lane.
        for (Entry<Integer, List<Count>> entry : allVehicles.entrySet()) {
            int dsrcCount = 0;
            int allCount = 0;
            Integer laneID = entry.getKey();
            List<Count> dsrcVehicleLaneCount = dsrcVehicles.get(laneID);
            if (dsrcVehicleLaneCount != null) {
                Iterator<Count> dsrcCountIter = dsrcVehicleLaneCount.iterator();
                while (dsrcCountIter.hasNext()) {
                    Count currCount = dsrcCountIter.next();
                    dsrcCount += currCount.count;
                }
                List<Count> allVehicleLaneCount = entry.getValue();
                Iterator<Count> allCountIter = allVehicleLaneCount.iterator();
                while (allCountIter.hasNext()) {
                    Count currCount = allCountIter.next();
                    allCount += currCount.count;
                }

                // Now calculate the DSRC percentage for the lane.
                if (allCount == 0) {
                    submitOutput(PERCENTAGE, laneID, "N/A");
                }
                else {
                    double percentage = (((double)dsrcCount) / ((double)allCount));
                    NumberFormat percentFormat = NumberFormat.getPercentInstance();
                    String percentageStr = percentFormat.format(percentage);
                    submitOutput(PERCENTAGE, laneID, percentageStr);
                }
            }

        }

        // Notify listeners that the output map has changed.
        // fireMOEChangeEvent(null, null);
    }

    /**
     * Update the MOE calculator output map with appropriate values.
     * 
     * @param output The output name to update.
     * @param laneId The lane of the output to update.
     * @param value The value to update.
     */
    void submitOutput(String output, int laneId, String value) {
        // if (laneOutputs.contains(output)) {
        String key = "Lane " + laneId + ", " + output + " (%)";
        outputs.put(key, value);
        // }
    }

    @Override
    public void registerLane(int laneId) {
        List<Count> countForLane = new LinkedList<Count>();
        allVehicles.put(laneId, countForLane);
        List<Count> dsrcForLane = new LinkedList<Count>();
        dsrcVehicles.put(laneId, dsrcForLane);
    }

    @Override
    public Set<Integer> getLanes() {
        return allVehicles.keySet();
    }

    @Override
    public Set<String> getEnabledLaneOutputs() {
        return laneOutputs;
    }

    /**
     * An internal class structure to hold and associate time and count values.
     */
    static private class Count {

        private int count;

        private Count() {}
    }
}