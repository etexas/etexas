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

import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * Interface for accessing a container for managing devices.
 * 
 * @author ablatt
 */
public interface IDevicesComponent {

    /**
     * Get a sequence of all devices which are active on step num
     * 
     * @param stepNum The step number to get active devices for
     * @return A sequence of active devices
     */
    public Iterable<IDeviceData> getActiveDevices(int stepNum);

    /**
     * Gets the proper IDs for the devices of the vehicle being passed in.
     * 
     * @param vehicle The vehicle to get the device proper IDs from.
     * @return The list of device proper IDs on the vehicle.
     */
    public List<String> getDeviceProperIds(IDable vehicle);

    /**
     * Get a sequence of all devices which are new on step num
     * 
     * @param stepNum The step number to get new devices for
     * @return A sequence of new devices
     */
    public Iterable<IDeviceData> getNewDevices(int stepNum);

    /**
     * Get a sequence of all devices which were stopped on step num
     * 
     * @param stepNum The step number to get stopped devices for
     * @return A sequence of stopped devices
     */
    public Iterable<IDeviceData> getStoppedDevices(int stepNum);

    /**
     * Remove devices from active devices as of step num
     * 
     * @param stepNum The step number to stop devices on
     * @param logoutVehicles A sequence of the ids of the devices to stop (typically vehicle ids)
     */
    public void putLogoutVehicles(int stepNum, Iterable<? extends IDable> logoutVehicles);

    /**
     * Add new devices to the data layer as of step num
     * 
     * @param stepNum The step number the devices are being added on
     * @param newDevices A sequence of the devices to add
     */
    public void putNewDevices(int stepNum, Iterable<? extends IDeviceData> newDevices);

    /**
     * Signals that the vehicle is no longer a stalled vehicle which will move it's devices to being
     * active.
     * 
     * @param vehicle The vehicle that has returned.
     */
    public void putReturnedVehicle(IDable vehicle);

    /**
     * Puts a vehicle that is in middle of switching simulations in as a stalled vehicle. stalled
     * vehicles will have their devices removed from active devices. This will also replace the old
     * vehicle ID with the new vehicle ID.
     * 
     * @param oldVehicle The old proper ID of the vehicle.
     * @param newVehicle The new proper ID of the vehicle.
     * @return The list of new device proper IDs that the stalled vehicle has.
     */
    public List<String> putStalledVehicle(IDable oldVehicle, IDable newVehicle);

    /**
     * Used for safe shutdowns. Will clean up all internal references and return all devices which
     * were active for shutdown.
     * 
     * @return All devices which were active on the most recent time step.
     */
    public Iterable<IDeviceData> stopAllDevices();
}
