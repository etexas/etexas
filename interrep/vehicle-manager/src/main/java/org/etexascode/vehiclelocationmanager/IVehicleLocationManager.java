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
package org.etexascode.vehiclelocationmanager;

/**
 * Interface governing a single vehicle location manager. This is used by a Parallel Location
 * Manager and typically governs a single intersection.
 * 
 * @author ablatt
 */
public interface IVehicleLocationManager {

    /**
     * The method which manages the vehicles. It will initializes apps for new vehicles. It will
     * sort logging out vehicles based on where they are supposed to go.
     * 
     * @param stepNum The step number to manage vehicle locations on.
     * @throws InstantiationException If the instantiation exception occurs.
     * @throws IllegalAccessException If the illegal access exception occurs.
     */
    public void manageLocations(int stepNum) throws InstantiationException, IllegalAccessException;
}
