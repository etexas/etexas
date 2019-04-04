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

/**
 * Interface defining a function/method which gathers all non-dsrc vehicles into a single list.
 * Changed logoutZone to use timeStep
 * 
 * @author ablatt
 * @author bmauldon
 */
public interface INotDSRCVehicles {

    /**
     * function/method which gathers all non-dsrc vehicles into a single list
     * 
     * @param prevMan Vehicle manager from the previous time step
     * @param currMan Vehicle manager from the current time step (only constructed from BSM and BSMV
     *        messages)
     * @param vehsFromDriveway vehicles which entered from driveways
     * @param vehsFromDetectors vehicles which passed over detectors
     * @param lm the ongoing lane manager
     * @param timeStep the size of the timeStep in the simulation
     * @return a list of all non DSRC vehicles found this time step
     */
    public List<Vehicle> getVehiclesNotDSRC(VehicleManager prevMan, VehicleManager currMan, List<Vehicle> vehsFromDriveway, List<Vehicle> vehsFromDetectors, LaneManager lm, double timeStep);
}
