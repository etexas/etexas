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

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.apps.LightChangeCalculator;
import org.etexascode.apps.QueueBackExpansionMOE;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * Driver for the MOE calculators for the microscopic model.
 * 
 * @author ablatt
 */
public class MicroscopicModelDriver {

    /** Gets the changes in the lights */
    LightChangeCalculator lcc = null;

    /** Module for finding the queues */
    FindQueues fq = new FindQueues();

    /** Module for calculating phase failures */
    PhaseFailureMOE pfmoe = new PhaseFailureMOE();

    /**
     * Module for calculating the expansion of the back of the queue accross a time step
     */
    QueueBackExpansionMOE qbemoe = null;

    /**
     * Module for calculating the travel time of vehicles in a lane from a configured distance from
     * stop line.
     */
    TravelTimeMOE ttmoe = new TravelTimeMOE();

    /**
     * Calculate MOEs based on the managers which were just passed in
     * 
     * @param vmi The vehicle manager for this time step
     * @param lmi The lane manager for this time step
     * @param smi The signal manager for this time step
     * @param simTime The current sim time
     * @param logger An app logger
     */
    public void update(IVehicleManager vmi, ILaneManager lmi, ISignalManager smi, Double simTime, AppLogger logger) {
        if (lcc == null) {
            lcc = new LightChangeCalculator(lmi.getLaneIds());
            qbemoe = new QueueBackExpansionMOE(lmi);
        }

        Map<String, Set<Integer>> sigChanges = lcc.update(smi, simTime);
        Map<Integer, List<IVehicle>> queues = fq.getCarsInQueue(vmi, lmi);
        Map<Integer, Double> queueLens = QueueLengthMOE.getQueueLengths(queues, lmi);
        QueueLengthMOE.writeLogs(queueLens, logger);
        int currFails = pfmoe.getPhaseFails(queues, sigChanges);
        int totFails = pfmoe.getTotalFails(currFails);
        PhaseFailureMOE.writeLogs(currFails, totFails, logger);
        qbemoe.update(queueLens, sigChanges, logger);
        ttmoe.update(vmi, lmi, simTime, logger);
    }
}
