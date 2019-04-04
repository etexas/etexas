/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.datalayer.interfaces;

import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * A component to manage global vehicle IDs in data layer classes.
 * 
 * @author ttevendale
 */
public interface IVehicleIdComponent {

    /**
     * Adds a vehicle ID to this component.
     * 
     * @param vehicle The vehicle to get the global ID for.
     * @return The global ID attached to the vehicle's proper ID.
     */
    public long addVehicleId(IVehicle vehicle);

    /**
     * Gets the global ID of a vehicle by proper ID.
     * 
     * @param properId The proper ID to get the global ID with.
     * @return The vehicle's global ID.
     */
    public Long getGlobalVehicleId(String properId);

    /**
     * Gets the proper ID of a vehicle by global ID.
     * 
     * @param globalId The global ID to get the proper ID with.
     * @return The vehicle's proper ID.
     */
    public String getProperVehicleId(long globalId);

    /**
     * Checks to see if the vehicle passed in is a stalled vehicle or not.
     * 
     * @param vehicle The vehicle to check.
     * @return True if the vehicle is stalled, false otherwise.
     */
    public boolean isStalledVehicle(IDable vehicle);

    /**
     * Returns the next local injected vehicle ID.
     * 
     * @return The integer ID for the next injected vehicle.
     */
    public int nextInjectedVehicleId();

    /**
     * Signals that the vehicle is no longer a stalled vehicle which will move it's global ID to
     * being active.
     * 
     * @param returnedVehicle The vehicle that has returned.
     */
    public void putReturnedVehicle(IDable returnedVehicle);

    /**
     * Puts a vehicle that is in middle of switching simulations in as a stalled vehicle. This will
     * also replace the old vehicle ID that's attached to a global ID with the new vehicle ID.
     * 
     * @param oldVehicle The old proper ID of the vehicle.
     * @param newVehicle The new proper ID of the vehicle.
     */
    public void putStalledVehicle(IDable oldVehicle, IDable newVehicle);
}
