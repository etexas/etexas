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

package org.etexascode.apps.dcs;

import org.etexascode.apps.RSEDevice;
import org.etexascode.apps.dcs.model.SignalController;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;

/**
 * A binding class which binds the DCS signal controller to the TEXAS signal controller.
 * 
 * @author jrutherford
 */
public class SignalControllerImpl implements SignalController {

    /** The TEXAS model which contains Public API methods. */
    private RSEDevice device;

    /** The ID of the intersection. */
    private int intersection;

    /**
     * Constructor used to wrap the TEXAS model public API object.
     * 
     * @param intersection The ID of the relevant intersection.
     * @param device The application device.
     * @param logger The AppLogger.
     */
    public SignalControllerImpl(int intersection, RSEDevice device, AppLogger logger) {
        this.intersection = intersection;
        this.device = device;
        // this.log = logger;
    }

    /**
     * Update the device.
     * 
     * @param device The device.
     */
    @Override
    public void updateDevice(RSEDevice device) {
        this.device = device;
    }

    /** Advance the signal controller to the next phase. */
    @Override
    public void changePhase() {
        SignalCommand command = new SignalCommand(SignalCommand.CHANGE_SIGNAL, 0);
        device.addSignalCommand(command);
    }

    /** Continue to hold the current phase of the signal controller. */
    @Override
    public void holdPhase() {
        SignalCommand command = new SignalCommand(SignalCommand.HOLD_SIGNAL, 0.5);
        device.addSignalCommand(command);
    }

    /**
     * Check to see if the signal controller is holding a green phase.
     * 
     * @return Whether or not a signal controller is holding a green phase.
     */
    @Override
    public boolean isHoldingGreen(int laneIds[]) {

        ISignalManager spatData = device.getSignalManager(intersection);
        boolean allGreen = true;
        for (int laneId : laneIds) {
            for (ISignalIndication signal : spatData.getSignalsByLaneId(laneId)) {
                Color color = signal.getColorIndication();
                if (color.equals(Color.RED) || color.equals(Color.YELLOW)) {
                    allGreen = false;
                }
            }
        }
        return allGreen;
    }
}