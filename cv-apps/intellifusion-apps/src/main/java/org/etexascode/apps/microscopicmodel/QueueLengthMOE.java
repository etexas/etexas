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
import java.util.Map.Entry;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;

/**
 * Module for calculating the queue lengths.
 * 
 * @author ablatt
 */
public class QueueLengthMOE {

    /**
     * Calculate the queue length from the queues. (Expcected to be the queues from FindQueues)
     * 
     * @param queues The queues to calculate the lengths of
     * @param lanMan The lanes these cars exist in
     * @return Key: lane id; Value: length of the queue
     */
    public static Map<Integer, Double> getQueueLengths(Map<Integer, List<IVehicle>> queues, ILaneManager lanMan) {
        Map<Integer, Double> ret = new HashMap<Integer, Double>();

        for (Entry<Integer, List<IVehicle>> entry : queues.entrySet()) {
            Integer key = entry.getKey();
            List<IVehicle> value = entry.getValue();
            if (value.size() == 0) {
                ret.put(key, 0.0);
            }
            else {
                ret.put(key, UtilsCalculations.getDistance(value.get(value.size() - 1), lanMan.getLaneById(key)));
            }
        }
        return ret;
    }

    /**
     * Specialized logging facility for the queue length calculator. Important Keys: Queue Length
     * Lane Id = [lane id here]
     * 
     * @param queues The queues to log (assumed to be the queues from this time step)
     * @param logger The logger to log in.
     */
    public static void writeLogs(Map<Integer, Double> queues, AppLogger logger) {
        for (Entry<Integer, Double> entry : queues.entrySet()) {
            logger.log("Queue Length Lane Id = " + entry.getKey().toString(), entry.getValue().toString());
        }
    }
}
