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

import java.util.Arrays;
import java.util.Collection;

import org.etexascode.apps.RSEDevice;
import org.etexascode.apps.dcs.model.SignalController;
import org.etexascode.apps.dcs.model.VehicleDilemmaZoneData;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.InterRepInfoModel;

/**
 * A Java implementation of Bonneson's D-CS algorithm
 * 
 * @author jrutherford
 * @author janway
 */
public class DCSAlgorithm {

    /**
     * A component used to check the status of vehicles and provide dilemma zone information.
     */
    private VehicleStatusComponent vehicleStatusComponent;

    /**
     * A component used to find the best time to change the signal and actually change the signal.
     */
    private PhaseStatusComponent phaseStatusComponent;

    /** The signal controller. */
    private SignalController signalController;

    /**
     * The set of lanes the algorithm is governing.
     */
    private int[] laneIDs;

    /**
     * Constructor.
     * 
     * @param device The DeviceEmulatorRSE.
     * @param model The InterRepInfoModel.
     * @param logger The Application Logger.
     * @param laneIDs The array of lane IDs on this app.
     */
    public DCSAlgorithm(RSEDevice device, InterRepInfoModel model, AppLogger logger, int laneIDs[]) {

        this.laneIDs = Arrays.copyOf(laneIDs, laneIDs.length);
        int intersection = model.getLmi().getIntersectionId();
        this.signalController = new SignalControllerImpl(intersection, device, logger);
        this.vehicleStatusComponent = new VehicleStatusComponent(device.getLaneManager(intersection), model.vmi, laneIDs);
        this.phaseStatusComponent = new PhaseStatusComponent(signalController, device.getLaneManager(intersection), device.getSignalManager(intersection), model.vmi,
                device.getDetectorManager(intersection), laneIDs);
    }

    /**
     * Update the managers.
     * 
     * @param interrep The interrep model.
     * @param device The road side equipment device.
     */
    public void updateManagers(InterRepInfoModel interrep, RSEDevice device) {
        vehicleStatusComponent.updateManagers(interrep);
        phaseStatusComponent.updateManagers(interrep);
        signalController.updateDevice(device);
    }

    /**
     * Configure the D-CS Algorithm with parameters. This method should be called before the method
     * to update the algorithm is called.
     * 
     * @param stage1Timeout Number of seconds to wait before moving algorithm to stage two.
     * @param stage2Timeout Number of seconds to wait before moving algorithm to max out stage.
     */
    public void configureTimeouts(double stage1Timeout, double stage2Timeout) {
        phaseStatusComponent.setStage1Timeout(stage1Timeout);
        phaseStatusComponent.setStage2Timeout(stage2Timeout);
    }

    /**
     * Update the algorithm with current information and perform vehicle detection and signal
     * control.
     * 
     * @param currentTime The current time in seconds.
     */
    public void performDetectionControlLoop(double currentTime) {
        // Detect vehicles and calculate a list of dilemma zone information.
        if (signalController.isHoldingGreen(laneIDs)) {
            vehicleStatusComponent.performDetection(currentTime);
        }
        else {
            vehicleStatusComponent.resetDilemmaZoneMatrix();
        }
        Collection<VehicleDilemmaZoneData> dilemmaZoneMatrix = vehicleStatusComponent.getDilemmaZoneMatrix();

        // Calculate the best time to end signal phases and change signals as needed.
        phaseStatusComponent.performControl(currentTime, dilemmaZoneMatrix);
    }
}
