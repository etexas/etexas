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

import org.etexascode.interrep.datamodel.Vehicle;

/**
 * Interface for defining a function which takes in vehicles and returns those vehicles with a
 * length attached
 * 
 * @author ablatt
 */
public interface IDetermineVehicleLength {

    /**
     * function which takes in vehicles and returns those vehicles with a length attached
     * 
     * @param vehs list of vehicles to attach lengths to
     * @return vehs with lenghts attached
     */
    public List<Vehicle> getVehicleLengths(List<Vehicle> vehs);
}
