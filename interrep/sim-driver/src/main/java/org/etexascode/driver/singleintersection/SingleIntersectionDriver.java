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
package org.etexascode.driver.singleintersection;

import org.etexascode.driver.ComponentContainer;
import org.etexascode.driver.IDriver;
import org.etexascode.driver.SimDriverException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An implementation which executes each component in order.
 * 
 * @author ablatt
 * @author jrutherford
 */
public class SingleIntersectionDriver implements IDriver {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SingleIntersectionDriver.class);

    /**
     * The components to be used in this simulation
     */
    private ComponentContainer contain;

    /**
     * The step size being used in this simulation
     */
    private double stepSize;

    /**
     * The current step number
     */
    private int currStepNum = 1;

    /**
     * sets step size for driver from component container and a container parameter for use.
     * 
     * @param cc Component container for driver.
     */
    public SingleIntersectionDriver(ComponentContainer cc) {
        stepSize = cc.interRepCoordinator.getMinStepSize();
        contain = cc;
    }

    /**
     * Iterates through wave simulation till no more steps are in the execution.
     * 
     * @param numSteps number of steps in execution.
     * @throws SimDriverException Used for useful messages.
     * @return returns boolean if there are more steps for execution.
     */
    // All thrown SimDriverExceptions should have user-friendly titles and messages.
    @Override
    public boolean execSteps(int numSteps) throws SimDriverException {

        boolean moreSteps = true;

        for (int i = 0; (i < numSteps) && (moreSteps); i++) {
            contain.waveSim.waveSim(currStepNum - 1);
            try {
                moreSteps = contain.interRepCoordinator.update(currStepNum);
            }
            catch (Exception e) {
                throw new SimDriverException("Exception occured while executing simulation.", e.getMessage());
            }

            if (moreSteps) {
                try {
                    contain.locationManager.manageLocations(currStepNum);
                }
                catch (InstantiationException e) {
                    LOGGER.info("problems with apps... shutting down the simulation", e);
                    throw new SimDriverException("Error in Simulation", "Could not find an app. Stopping simulation.");
                }
                catch (IllegalAccessException e) {
                    LOGGER.info("problems with apps... shutting down the simulation", e);
                    throw new SimDriverException("Error in Simulation", "Could not create an app. Stopping simulation.");
                }

                try {
                    contain.shutdownLayer.shutdown(currStepNum);
                }
                catch (Exception e) {
                    LOGGER.info("here now with exception");
                    e.printStackTrace(System.out);
                    throw new SimDriverException("Error in Simulation", e.getMessage());
                }

                try {
                    contain.appLayer.execApps(currStepNum);
                }
                catch (Exception e) {
                    LOGGER.info("problems with updating apps... shutting down the simulation", e);
                    throw new SimDriverException("Error in simulation.", e.getMessage());
                }
                currStepNum++;
            }
        }
        return moreSteps;
    }

    /**
     * Gracefully shuts down driver simulation
     */
    @Override
    public void shutdownNicely() {
        contain.shutdownLayer.shutdownAll();
        contain.interRepCoordinator.shutdown();
        contain.data.shutdown();
    }
}
