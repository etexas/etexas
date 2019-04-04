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
package org.etexascode.apps.hybrid.defaultimplementation;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.etexascode.apps.hybrid.interfaces.INotDSRCVehicles;
import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Basic implementation of the gathering non-DSRC vehicles
 * 
 * @author ablatt
 */
public class SimpleNotDSRCVehicles implements INotDSRCVehicles {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SimpleNotDSRCVehicles.class);

    @Override
    public List<Vehicle> getVehiclesNotDSRC(VehicleManager prevMan, VehicleManager currMan, List<Vehicle> vehsFromDriveway, List<Vehicle> vehsFromDetectors, LaneManager lm, double timeStep) {
        List<Vehicle> seen = alreadySeen(prevMan, currMan, lm, timeStep);
        List<Vehicle> ret = new ArrayList<Vehicle>(seen.size() + vehsFromDriveway.size() + vehsFromDetectors.size());

        ret.addAll(seen);
        ret.addAll(vehsFromDetectors);
        ret.addAll(vehsFromDriveway);

        return ret;
    }

    /**
     * Gets a list of vehicles which have already been seen Filters for only vehicles which should
     * still be in the intersection
     * 
     * @param prevMan The vehicle manager from the previous time step
     * @param currMan The vehicle manager from this time step (constructed from dsrc messages only)
     * @param lm The lane manager which the vehicles fit into
     * @param zone The size of the logout zone in cm
     * @return A list of vehicles which should still be in the intersection but which did not send a
     *         dsrc message this time step
     */
    List<Vehicle> alreadySeen(VehicleManager prevMan, VehicleManager currMan, LaneManager lm, double timeStep) {
        List<Vehicle> ret = new LinkedList<Vehicle>();

        for (String i : prevMan.getAllVehicleIds()) {
            if (currMan.getVehicle(i) == null) {
                if (!UtilsInterRep.vehicleInLogoutZone(lm, prevMan.getVehicle(i), timeStep)) {
                    ret.add(prevMan.getVehicle(i).clone());
                }
            }
        }

        return ret;
    }
}
