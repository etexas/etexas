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

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.devicedata.AppLogger;

/**
 * Keeps track of queue delays over time.
 * 
 * @author janway
 */
public class QueueDelayMOECalc {

    /**
     * The total delay over the simulation, per lane.
     */
    private Map<Integer, BigDecimal> totalDelays = null;

    /**
     * The moving queue delay windows, per lane.
     */
    private Map<Integer, List<Double>> delays = null;

    /**
     * Default constructor - do not delete.
     */
    QueueDelayMOECalc() {}

    /**
     * Updates lists of the most recent queue delays by lane, and total queue delays by lane.
     * 
     * @param queueLens The newest delay values.
     * @param simTime The time of the simulation.
     * @param windowSize The number of previous delay values to store and sum.
     * @return The sum of the previous windowSize queue delay values.
     */
    public Map<Integer, Double> update(Map<Integer, Double> queueLens, Double simTime, Integer windowSize) {

        // 16 digit precision, rounding mode "half even" - rounds to nearest neighbor, taking the
        // even one if they are equidistant.
        MathContext mc = MathContext.DECIMAL64;

        if (totalDelays == null) {
            totalDelays = new HashMap<Integer, BigDecimal>();
        }
        if (delays == null) {
            delays = new HashMap<Integer, List<Double>>();
        }

        Map<Integer, Double> delaySums = new HashMap<Integer, Double>();

        for (Map.Entry<Integer, Double> entry : queueLens.entrySet()) {
            // Add to the running total
            BigDecimal toAdd = new BigDecimal(entry.getValue(), mc);
            if (!totalDelays.containsKey(entry.getKey())) {
                totalDelays.put(entry.getKey(), toAdd);
            }
            else {
                totalDelays.put(entry.getKey(), totalDelays.get(entry.getKey()).add(toAdd, mc));
            }

            // Add newest value to the window
            List<Double> curList;
            if (!delays.containsKey(entry.getKey())) {
                curList = new LinkedList<Double>();
            }
            else {
                curList = delays.get(entry.getKey());
            }
            curList.add(entry.getValue());

            // Remove oldest value from the window if window is full
            while (curList.size() > windowSize) {
                curList.remove(0);
            }
            delays.put(entry.getKey(), curList);

            // Sum the values in the window
            Double sum = 0.0;
            for (Double d : curList) {
                sum += d;
            }
            delaySums.put(entry.getKey(), sum);
        }

        return delaySums;
    }

    /**
     * Logs the total queue delay tallied for each lane.
     * 
     * @param applogger The logger to use.
     */
    public void onDestroy(AppLogger applogger) {
        for (Map.Entry<Integer, BigDecimal> entry : totalDelays.entrySet()) {
            applogger.log("Queue Delay - Total", "lane: " + entry.getKey() + ", total delay: " + entry.getValue().toString());
        }
    }

    /**
     * Getter (for testing)
     * 
     * @return The map of total delays by lane.
     */
    Map<Integer, BigDecimal> getTotalDelays() {
        return totalDelays;
    }
}
