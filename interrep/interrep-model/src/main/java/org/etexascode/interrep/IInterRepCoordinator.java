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

/**
 * Interface governing the interrep coordination layer
 * 
 * @author ablatt
 */
public interface IInterRepCoordinator {

    /**
     * Update the interReps in the interRep coordination layer
     * 
     * @param stepNum The step number to update to
     * @return There are more steps in this multi-intersection simulation (true/false)
     */
    public boolean update(int stepNum);

    /**
     * Gets the minimum step size among all the interReps
     * 
     * @return The minimum step size among all the interReps
     */
    public double getMinStepSize();

    /**
     * Shutdown the intersections.
     */
    public void shutdown();
}
