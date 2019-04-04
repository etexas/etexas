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

package org.etexascode.apps.dcs.model;

import org.etexascode.apps.RSEDevice;

/**
 * An interface for objects used to change/hold signals or get signal information.
 * 
 * @author jrutherford
 */
public interface SignalController {

    /** Advance the signal controller to the next phase. */
    public void changePhase();

    /**
     * Check to see if the signal controller is holding a green phase.
     * 
     * @param laneIds The lane IDs to check for.
     * @return Whether or not a signal controller is holding a green phase.
     */
    public boolean isHoldingGreen(int laneIds[]);

    /** Continue to hold the current phase of the signal controller. */
    public void holdPhase();

    /**
     * Update the device.
     * 
     * @param device The device.
     */
    public void updateDevice(RSEDevice device);
}