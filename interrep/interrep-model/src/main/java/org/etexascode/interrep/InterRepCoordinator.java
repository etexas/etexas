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
package org.etexascode.interrep;

import java.util.Arrays;
import java.util.List;

import org.etexascode.datalayer.interfaces.IInterRepCoordinationDataLayer;

/**
 * Basic, in-memory design for the interRep coordination layer
 * 
 * @author ablatt
 */
public class InterRepCoordinator implements IInterRepCoordinator {

    /** Container for all the external intersection simulations. */
    List<IInterRep> internal = null;

    /** The internal times of all the simulations in internal. */
    double[] internalTimes = null;

    /** The step sizes of the different simulations in internal. */
    double[] stepSizes = null;

    /** A reference to the data layer viewable by the this coordination layer. */
    IInterRepCoordinationDataLayer data = null;

    /** The smallest step size in stepSizes. */
    double minStepSize = Double.MAX_VALUE;

    /**
     * Constructor
     * 
     * @param interReps List of simulations to coordinate.
     * @param data The data layer to get relevant data from.
     */
    public InterRepCoordinator(List<IInterRep> interReps, IInterRepCoordinationDataLayer data) {
        internal = interReps;
        this.data = data;

        internalTimes = new double[internal.size()];
        Arrays.fill(internalTimes, 0.0);

        stepSizes = new double[internal.size()];
        int i = 0;
        for (IInterRep ir : internal) {
            double d = ir.getTimeStepInterval();
            stepSizes[i] = d;
            if (d < minStepSize) {
                minStepSize = d;
            }
            i++;
        }
    }

    /**
     * Checks to see if there are more updates to occur while bringing all interreps up to the
     * current time.
     * 
     * @param stepNum The step number of the simulation.
     * @return True/False if there are more updates to occur.
     */
    @Override
    public boolean update(int stepNum) {
        boolean ret = true;
        double currTime = data.getSimTime(stepNum); // the sim time this coordinator will have at
                                                    // the end of this update
        for (int i = 0; i < internalTimes.length; i++) { // bring all interReps up to at least
                                                         // currTime
            double time = internalTimes[i] + stepSizes[i]; // get the time for a specific interRep
            if (time <= currTime) { // if this interRep is not caught up to the current sim time
                ret &= internal.get(i).update(); // update interRep, switch ret to false if the
                                                 // interRep returns false
                internalTimes[i] = time; // set new interRep sim time
            }
        }

        return ret;
    }

    /**
     * Gets the minimum step size.
     * 
     * @return The minimum step size as a double.
     */
    @Override
    public double getMinStepSize() {
        return minStepSize;
    }

    /**
     * Shut down Interrep
     */
    @Override
    public void shutdown() {
        for (IInterRep iis : internal) {
            iis.close();
        }

        internal = null;
        internalTimes = null;
        stepSizes = null;
        data = null;
    }
}
