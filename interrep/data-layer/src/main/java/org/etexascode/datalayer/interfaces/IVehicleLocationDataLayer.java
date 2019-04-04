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
package org.etexascode.datalayer.interfaces;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * Data Layer for Managing Vehicle Locations
 * 
 * @author ablatt
 * @author ttevendale
 */
public interface IVehicleLocationDataLayer {

    /**
     * Adds a vehicle ID to this component.
     * 
     * @param vehicle The vehicle to get the global ID for.
     * @return The global ID attached to the vehicle's proper ID.
     */
    public long addVehicleId(IVehicle vehicle);

    /**
     * Add a Vehicle Injection Request to the data layer
     * 
     * @param source The source of the request.
     * @param stepNum The step number which the Data Layer should use
     * @param request The request to add
     * @param interRepId The id used by the InterRep when creating its InfoModel
     */
    public void addVehicleInjectionRequest(String source, int stepNum, VehicleInjectionRequest request, int interRepId);

    /**
     * Gets the global ID of a vehicle by proper ID.
     * 
     * @param properId The proper ID to get the global ID with.
     * @return The vehicle's global ID.
     */
    public Long getGlobalVehicleId(String properId);

    /**
     * Get the ids of the vehicles injected into interRepId's interRep on stepNum
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The used by the InterRep to when creating its InfoModel
     * @return Ids of those Vehicles injected
     */
    public Set<String> getInjectedVehicles(int stepNum, int interRepId);

    /**
     * Gets the proper ID of a vehicle by global ID.
     * 
     * @param globalId The global ID to get the proper ID with.
     * @return The vehicle's proper ID.
     */
    public String getProperVehicleId(long globalId);

    /**
     * Get the random seed to be used this time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The used by the InterRep to when creating its InfoModel
     * @return The random seed to be used on this time step
     */
    public int getRandomSeed(int stepNum, int interRepId);

    /**
     * Get the current time of the simulation
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return The time in the simulation (in seconds)
     */
    public double getSimTime(int stepNum);

    /**
     * Get the VehicleManager associated with a specific step num and intersection id
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The used by the InterRep to when creating its InfoModel
     * @return The Vehicles in the associated InterRepInfoModel
     */
    public IVehicleManager getVehicleManagerInfo(int stepNum, int interRepId);

    /**
     * Checks to see if the vehicle passed in is a stalled vehicle or not.
     * 
     * @param vehicle The vehicle to check.
     * @return True if the vehicle is stalled, false otherwise.
     */
    public boolean isStalledVehicle(IDable vehicle);

    /**
     * Returns the next ID for vehicle injection.
     * 
     * @return The next integer ID for vehicle injection.
     */
    public int nextInjectedVehicleId();

    /**
     * Add new Devices to the Data Layer
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param deviceData The devices to add
     */
    public void putAppDeviceData(int stepNum, List<? extends IDeviceData> deviceData);

    /**
     * Add Vehicles logging out of this intersection this time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param logoutVehicles The vehicles logging out
     */
    public void putLoggedOutVehicles(int stepNum, Map<DualIntIdentifier, List<IVehicle>> logoutVehicles);

    /**
     * Associate the random seed with step num and interRepId
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The used by the InterRep to when creating its InfoModel
     * @param randomSeed The random seed (will be used for getRandomSeed for the next stepNum)
     */
    public void putRandomSeed(int stepNum, int interRepId, int randomSeed);

    /**
     * Signals that the vehicle is no longer a stalled vehicle.
     * 
     * @param returnedVehicle The vehicle that has returned.
     */
    public void putReturnedVehicle(IDable returnedVehicle);

    /**
     * Puts a vehicle that is in middle of switching simulations in as a stalled vehicle.
     * 
     * @param oldVehicle The old proper ID of the vehicle.
     * @param newVehicle The new proper ID of the vehicle.
     */
    public void putStalledVehicle(IDable oldVehicle, IDable newVehicle);
}
