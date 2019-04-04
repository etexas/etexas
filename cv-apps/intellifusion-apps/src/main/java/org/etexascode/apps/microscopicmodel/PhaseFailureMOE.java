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
package org.etexascode.apps.microscopicmodel;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * MOE class for dealing with phase failures. Methods of interest: getPhaseFails getTotalFails
 * writeLogs
 * 
 * @author ablatt
 */
public class PhaseFailureMOE {

    /** The state of queues as of that lane's previous green light. */
    Map<Integer, List<IVehicle>> onPrevGreen = new HashMap<Integer, List<IVehicle>>();

    /**
     * A running count of the total number of phase failures thus far as stipulated by
     * getTotalFails.
     */
    int runningFails = 0;

    /**
     * Get the number of phase failures for this time step. Store any queues which have turned to
     * green this time step
     * 
     * @param queues All the queues for this time step
     * @param lightChanges The light changes for this time step
     * @return The number of phase failures experienced this time step
     */
    public int getPhaseFails(Map<Integer, List<IVehicle>> queues, Map<String, Set<Integer>> lightChanges) {
        int ret = 0;

        ret += onRed(queues, lightChanges.get("GREEN-RED"));
        ret += onRed(queues, lightChanges.get("YELLOW-RED"));

        onGreen(queues, lightChanges.get("RED-GREEN"));
        onGreen(queues, lightChanges.get("YELLOW-GREEN"));

        return ret;
    }

    /**
     * Store the queues which just turned green into onPrevGreen
     * 
     * @param queues All queues this time step
     * @param changed The set of lane ids whose lights turned green this step
     */
    void onGreen(Map<Integer, List<IVehicle>> queues, Set<Integer> changed) {
        for (Integer i : changed) {
            onPrevGreen.put(i, queues.get(i));
        }
    }

    /**
     * Calculate the number of phase failures which happened this cycle.
     * 
     * @param queues The queues to examine against onPrevGreen queues
     * @param changed The ids of the lanes which have been changed to red this time step.
     * @return The number of phase failues detected.
     */
    int onRed(Map<Integer, List<IVehicle>> queues, Set<Integer> changed) {
        int ret = 0;

        for (Integer i : changed) {
            if (queues.get(i) == null) {
                continue;
            }
            else if (onPrevGreen.get(i) == null) {
                continue;
            }
            else {
                ret += isOverlap(queues.get(i), onPrevGreen.get(i));
            }
        }

        return ret;
    }

    /**
     * Returns 1 if l1 and l2 have at least 1 vehicle with the same id in common. Returns 0
     * otherwise.
     * 
     * @param l1 A list of vehicle infos (presumably a queue)
     * @param l2 A list of vehicle infos (presumably a queue)
     * @return 0 if no vehicles in common, 1 if at least 1 vehicle in common
     */
    int isOverlap(List<IVehicle> l1, List<IVehicle> l2) {
        for (IVehicle vi1 : l1) {
            for (IVehicle vi2 : l2) {
                if (vi1.getVehicleID() == vi2.getVehicleID()) {
                    return 1;
                }
            }
        }

        return 0;
    }

    /**
     * Manipulate the total number of phase failures seen thus far.
     * 
     * @param failsThisStep Number of phase failures from this step.
     * @return The total number of phase failures seen thus far (is the sum of all the failsThisStep
     *         passed in thus far)
     */
    public int getTotalFails(int failsThisStep) {
        runningFails += failsThisStep;
        return runningFails;
    }

    /**
     * Specialized logging facility for the phase failure calculator. Important keys: Phase Fails
     * This Step Total Phase Fails Right Now
     * 
     * @param failsThisStep Number to associate with "Phase Fails This Step"
     * @param totalFails Number to associate with "Total Phase Fails Right Now"
     * @param logger The logger to write the appropriate logs to.
     */
    public static void writeLogs(int failsThisStep, int totalFails, AppLogger logger) {
        logger.log("Phase Fails This Step", Integer.toString(failsThisStep));
        logger.log("Total Phase Fails Right Now", Integer.toString(totalFails));
    }
}