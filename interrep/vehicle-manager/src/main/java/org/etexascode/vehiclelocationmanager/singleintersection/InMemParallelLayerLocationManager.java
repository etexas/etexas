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
package org.etexascode.vehiclelocationmanager.singleintersection;

import java.util.List;

import org.etexascode.vehiclelocationmanager.IVehicleLocationManager;
import org.etexascode.vehiclelocationmanager.IVehicleLocationManagerCoordinator;

/**
 * An In Memory Parallelizaton Layer for the Location Manager. This implementation runs all code
 * linearly.
 * 
 * @author ablatt
 */
public class InMemParallelLayerLocationManager implements IVehicleLocationManagerCoordinator {

    /**
     * Managers to execute against
     */
    final List<IVehicleLocationManager> vehLocMan;

    /**
     * Constructor
     * 
     * @param mans The location managers this module is to execute at each time step
     */
    public InMemParallelLayerLocationManager(List<IVehicleLocationManager> mans) {
        vehLocMan = mans;
    }

    /**
     * Manages the location of the vehicle managers at a specific time step
     * 
     * @param stepNum step number of the simulation
     * @throws InstantiationException If the instantiation exception occurs.
     * @throws IllegalAccessException If the illegal access exception occurs.
     */
    @Override
    public void manageLocations(int stepNum) throws InstantiationException, IllegalAccessException {
        for (IVehicleLocationManager vlm : vehLocMan) {
            vlm.manageLocations(stepNum);
        }
    }
}
