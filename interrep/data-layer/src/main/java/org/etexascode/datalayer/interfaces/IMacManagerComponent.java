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

import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * Interface for accessing a container for managing mac addresses for devices.
 * 
 * @author ablatt
 * @author ttevendale
 */
public interface IMacManagerComponent {

    /**
     * Get a mac associated with an id on a time step.
     * 
     * @param stepNum The step number to get the mac on.
     * @param deviceId The device id whose mac we want.
     * @return The mac associated with deviceId on step num.
     * @throws RuntimeException If no such mac exists, there is no mitigation strategy for semantic
     *         errors in the way we deal with devices.
     */
    public Long getMac(int stepNum, String deviceId);

    /**
     * Add new devices to the container. This will associate a newly generated mac with the proper
     * id of the devices. (yes this does mean you could associate a mac with anything with a proper
     * id)
     * 
     * @param stepNum The step num these ids are being added on.
     * @param newDevices The ids being added to the container.
     */
    public void putNewDevices(int stepNum, Iterable<? extends IDable> newDevices);

    /**
     * Remove ids from the container. This provides 2 benefits.
     * 
     * @param stepNum The step num to remove the vehicles on.
     * @param logoutVehicles The ids to log out.
     */
    public void removeDevices(int stepNum, Iterable<? extends IDable> logoutVehicles);

    /**
     * Safely shuts down the mac manager.
     */
    public void shutdown();

    /**
     * Updates device proper ID that the mac address is attached to.
     * 
     * @param oldDeviceId The old device proper ID.
     * @param newDeviceId The new device proper ID.
     */
    public void updateMacAddress(String oldDeviceId, String newDeviceId);
}
