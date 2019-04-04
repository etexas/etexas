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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IAppLifecycle;

/**
 * The app for calculating the Hybrid Intellifusion MOEs. The MOEs are based on shock wave analysis
 * used in phase 1. TODO ttevendale 1/4/2018 look into creating unit tests and/or updating this app
 * to use the new spec
 * 
 * @author ablatt
 */
public class HybridIntellifusionMOEApp extends RSEBasePopulatedApp implements IAppLifecycle {

    QueueLengthMOECalc queueLengthCalc = null;

    DSRCPercentageMOECalc dsrcCalc = null;

    PhaseFailureCalc phaseCalc = null;

    QueueDelayMOECalc queueDelayCalc = null;

    FuelUsageMOECalc fuelUsageCalc = null;

    double prevTime = -1.0;

    QueueBackExpansionMOE queueBackCalc = null;

    LightChangeCalculator lightChangeCalc = null;

    @Override
    public void performUpdate(EstimatedDataModel model, RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
        if (lightChangeCalc == null) {
            lightChangeCalc = new LightChangeCalculator(model.getLaneManager().getLaneIds());
        }

        Map<String, Set<Integer>> lightChangeData = lightChangeCalc.update(model.getSignalManager(), simTime);
        Map<Integer, Double> timeToChange = lightChangeCalc.getTimeToChangeByLane();

        // ------------------------------------------------------
        // Queue length calculations
        if (queueLengthCalc == null) {
            queueLengthCalc = new QueueLengthMOECalc(model);

            for (Integer laneId : model.getLaneManager().getLaneIds()) {
                queueLengthCalc.registerLane(laneId);
            }
        }

        queueLengthCalc.vehicleUpdated(model.getUpdatedVehicles());
        queueLengthCalc.detectorChanged(model.getUpdatedDetectors());
        queueLengthCalc.signalUpdated();

        if (prevTime == -1.0) {
            queueLengthCalc.advance(0.0);
        }
        else {
            queueLengthCalc.advance(simTime - prevTime);
        }

        // get data out of the queue length calc
        Map<String, String> outs = queueLengthCalc.getOutputs();
        Map<Integer, Double> queueLens = new HashMap<Integer, Double>();
        Map<Integer, Double> queueBacks = new HashMap<Integer, Double>();

        for (Entry<String, String> entry : outs.entrySet()) {
            String key = entry.getKey();
            if (key.contains(QueueLengthMOECalc.QUEUE_LENGTH)) {
                int laneId = Integer.parseInt(key.substring(5, key.indexOf(',')));
                queueLens.put(laneId, new Double(entry.getValue()));
            }

            if (key.contains(QueueLengthMOECalc.QUEUE_BACK)) {
                int laneId = Integer.parseInt(key.substring(5, key.indexOf(',')));
                queueBacks.put(laneId, new Double(entry.getValue()));
            }
        }

        for (Entry<Integer, Double> i : queueLens.entrySet()) {
            logger.log("Queue Length MOE", "(lan id = " + i.getKey().toString() + ") " + i.getValue().toString());
        }

        // ------------------------------------------------------
        // dsrc percentage calculations
        if (dsrcCalc == null) {
            dsrcCalc = new DSRCPercentageMOECalc(model);

            for (Integer laneId : model.getLaneManager().getLaneIds()) {
                dsrcCalc.registerLane(laneId);
            }
        }

        dsrcCalc.vehicleUpdated(model.getUpdatedVehicles());
        dsrcCalc.detectorChanged(model.getUpdatedDetectors());

        if (prevTime == -1.0) {
            dsrcCalc.advance(0.0);
        }
        else {
            dsrcCalc.advance(simTime - prevTime);
        }

        for (String key : dsrcCalc.getOutputs().keySet()) {
            logger.log("DSRC Calc", "key = " + key + "   value = " + dsrcCalc.getOutput(key));
        }

        // ------------------------------------------------------
        // Phase failure calculator percentage
        if (phaseCalc == null) {
            Map<Integer, Double> speedLims = new HashMap<Integer, Double>();
            for (Integer laneId : device.getLaneManager(intersection).getLaneIds()) {
                speedLims.put(laneId, device.getLaneManager(intersection).getLaneById(laneId).getSpeedLimitInMetersPerSecond());
            }
            phaseCalc = new PhaseFailureCalc(speedLims);
            phaseCalc = new PhaseFailureCalc(device.getLaneManager(intersection));
        }

        int prevFailureCount = phaseCalc.getNumPhaseFailure();
        // phaseCalc.update(queueLens, model.signalManager, model.vehicleManager, simTime);
        phaseCalc.update(queueLens, lightChangeData, timeToChange, model.getVehicleManager(), simTime);
        if (phaseCalc.getNumPhaseFailure() > prevFailureCount) {}

        // ------------------------------------------------------
        // queue delay calculations
        // TODO: janway - what should the window size be? Where should it be set? -- ablatt - we
        // could tie the window size to the light cycle - using the light change data
        if (queueDelayCalc == null) {
            queueDelayCalc = new QueueDelayMOECalc();
        }

        int queueDelayWindowSize = 20;
        Map<Integer, Double> delays = queueDelayCalc.update(queueLens, simTime, queueDelayWindowSize);
        for (Map.Entry<Integer, Double> entry : delays.entrySet()) {
            logger.log("Queue Delay Calc", "lane: " + entry.getKey() + ", delay: " + entry.getValue());
        }

        // ------------------------------------------------------
        // fuel usage calculations
        if (fuelUsageCalc == null) {
            fuelUsageCalc = new FuelUsageMOECalc(model.getLaneManager(), model.getDetectorManager());
        }

        fuelUsageCalc.update(model.getUpdatedVehicles(), model.getUpdatedDetectors(), model.getVehicleManager(), simTime);

        // ------------------------------------------------------
        // queue back expansions calculations
        if (queueBackCalc == null) {
            queueBackCalc = new QueueBackExpansionMOE(model.getLaneManager());
        }

        queueBackCalc.update(queueBacks, lightChangeData, logger);

        // shutdown();

        prevTime = simTime;

    }

    @Override
    public void init(String[] appConfigs) {

        intersection = Integer.parseInt(appConfigs[0]);
    }

    @Override
    public void appShutdown(AppLogger logger) {
        phaseCalc.onDestroy(logger);
        queueDelayCalc.onDestroy(logger);
        fuelUsageCalc.onDestroy(logger);
    }
}
