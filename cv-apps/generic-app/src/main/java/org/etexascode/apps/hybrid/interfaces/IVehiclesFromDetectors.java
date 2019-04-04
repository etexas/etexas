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
package org.etexascode.apps.hybrid.interfaces;

import java.util.List;

import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;

/**
 * Interface defining a function/method which finds new vehicles from detectors.
 * 
 * @author ablatt
 */
public interface IVehiclesFromDetectors {

    /**
     * function/method which finds new vehicles from detectors
     * 
     * @param prevMan the vehicle manager from the previous time step
     * @param currMan the vehicle manager from this time step (only includes DSRC vehicles)
     * @param detMan the detector manager from the device for this time step
     * @param laneMan the lane manager which new vehicles are supposed to fit into
     * @return A list of non-DSRC vehicles which were found by detectors this time step
     */
    public List<Vehicle> getVehiclesFromDetectors(VehicleManager prevMan, VehicleManager currMan, IDetectorManager detMan, LaneManager laneMan);

}
