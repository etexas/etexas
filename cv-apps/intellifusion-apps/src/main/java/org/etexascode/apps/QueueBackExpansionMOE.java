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
import java.util.Map;
import java.util.Set;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A class for estiamting the amount that the queue back has grown during a green light. Expected
 * usage: 1) Initialize the class using the set of all the lanes you want to watch (typically use
 * LaneManagerInfo.getLaneIds()) (Note: there is a constructor which just takes in the lane manager
 * if you prefer) 2) At each time step, call the update method. Check javadocs for update to
 * determine what to pass in. 3) Check the App Logs for key: QueueBackExpansionMOE
 * 
 * @author ablatt
 */
public class QueueBackExpansionMOE {

    /** Static logger */
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(QueueBackExpansionMOE.class);

    /** Key: Lane Id Value: Queue Back on Green */
    Map<Integer, Double> queueBackOnGreen = new HashMap<Integer, Double>();

    /** Key: Lane Id Value: Maximum observed queue back for this cycle */
    Map<Integer, Double> queueBackMax = new HashMap<Integer, Double>();

    /**
     * Key: Lane Id Value: Has a green light been observed for this lane This map weeds out errors
     * from when the light immediately goes to yellow/red
     */
    Map<Integer, Boolean> hasBeenGreen = new HashMap<Integer, Boolean>();

    /**
     * Constructor
     * 
     * @param lm Initialize the calculator using all the lanes in the provided lane manager
     */
    public QueueBackExpansionMOE(ILaneManager lm) {
        this(lm.getLaneIds());
    }

    /**
     * Constructor
     * 
     * @param laneIds Initialize the calculator using all the lanes in the provided set
     */
    public QueueBackExpansionMOE(Set<Integer> laneIds) {
        for (Integer i : laneIds) {
            queueBackOnGreen.put(i, -1.0);
            queueBackMax.put(i, -1.0);
            hasBeenGreen.put(i, false);
        }
    }

    /**
     * Update the queue back expansion data.
     * 
     * @param queueBacksByLane Map to Key: Lane Id, Value: Queue Back (dist from intersection)
     * @param lightChanges Results of the Light Change Calculator; Map to Key: Color Change, Value:
     *        List of lights which experienced that color change
     * @param logger The logger.
     */
    public void update(Map<Integer, Double> queueBacksByLane, Map<String, Set<Integer>> lightChanges, AppLogger logger) {

        // process on green data
        for (Integer laneId : lightChanges.get("GREEN-GREEN")) {
            onGreen(laneId, queueBacksByLane);
        }

        // process red to green and yellow to green
        for (Integer laneId : lightChanges.get("RED-GREEN")) {
            toGreen(laneId, queueBacksByLane);
        }

        for (Integer laneId : lightChanges.get("YELLOW-GREEN")) {
            toGreen(laneId, queueBacksByLane);
        }

        // process green to yellow and green to red
        for (Integer laneId : lightChanges.get("GREEN-YELLOW")) {
            fromGreen(laneId, logger);
        }

        for (Integer laneId : lightChanges.get("GREEN-RED")) {
            fromGreen(laneId, logger);
        }
    }

    /**
     * Checks if the queue back has expanded
     * 
     * @param laneId
     * @param queueBacksByLane The queue backs for this time step
     */
    void onGreen(Integer laneId, Map<Integer, Double> queueBacksByLane) {
        if ((hasBeenGreen.get(laneId) != null) && hasBeenGreen.get(laneId)) {
            double max = queueBackMax.get(laneId); // null here
            double curr = queueBacksByLane.get(laneId);

            if (curr > max) {
                queueBackMax.put(laneId, curr);
            } // else do nothing
        }
    }

    /**
     * Starts a new queue back measurement
     * 
     * @param laneId
     * @param queueBacksByLane
     */
    void toGreen(Integer laneId, Map<Integer, Double> queueBacksByLane) {
        if (queueBackOnGreen.get(laneId) != null) {
            Double queueLength = queueBacksByLane.get(laneId);
            queueBackOnGreen.put(laneId, queueLength);
            queueBackMax.put(laneId, queueLength);
            hasBeenGreen.put(laneId, true);
        } // else {} // do nothing
    }

    /**
     * Log the measured maximum of the queue expansion on green
     * 
     * @param laneId
     * @param logger
     */
    void fromGreen(Integer laneId, AppLogger logger) {
        if ((hasBeenGreen.get(laneId) != null) && hasBeenGreen.get(laneId)) {
            logger.log("QueueBackExpansionMOE", laneId + ", " + (queueBackMax.get(laneId) - queueBackOnGreen.get(laneId)));
            hasBeenGreen.put(laneId, false);
        }
    }
}
