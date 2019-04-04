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
package org.etexascode.datalayer.inmemory;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.devicedata.AbstractEmbeddedDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * A component for managing devices in a single intersection context.
 * 
 * @author ablatt
 */
public class SingleIntersectionDeviceComponent implements IDevicesComponent {

    /**
     * The new devices
     */
    Collection<IDeviceData> newDevices = new LinkedList<IDeviceData>();

    /**
     * The devices to be shut down
     */
    Collection<IDeviceData> shutdownDevices = new LinkedList<IDeviceData>();

    /**
     * A map for vehicle IDs to list of device properIDs
     */
    Map<String, List<String>> activeVehicles = new HashMap<String, List<String>>();

    /**
     * The devices that are currently active Key: Device Id (Vehicle Id for OBU devices or Vehicle
     * Id and Cell Id for Cell Devices), Value: active device
     */
    Map<String, IDeviceData> activeDevices = new HashMap<String, IDeviceData>();

    /**
     * Current time step
     */
    int currStep = Integer.MIN_VALUE;

    /**
     * Holder for currently stopped devices (used because shutdown devices can be requested multiple
     * times for the same step but must begin accumulating new stopped devices immediately)
     */
    Iterable<IDeviceData> currStopped = null;

    /**
     * Constructor
     * 
     * @param fixedDevices The devices that are 'fixed' into place and won't move during execution
     *        (should be active as soon as the simulation starts)
     */
    public SingleIntersectionDeviceComponent(List<IDeviceData> fixedDevices) {
        for (IDeviceData dat : fixedDevices) {
            activeDevices.put(dat.getProperId(), dat);
            newDevices.add(dat);
        }
    }

    /**
     * Gets the list of active devices.
     * 
     * @param stepNum The current step number
     * @return The active devices
     */
    @Override
    public Iterable<IDeviceData> getActiveDevices(int stepNum) {
        return activeDevices.values();
    }

    /**
     * Gets a list of new devices
     * 
     * @param stepNum The current step number
     * @return The list of new devices
     */
    @Override
    public Iterable<IDeviceData> getNewDevices(int stepNum) {
        Collection<IDeviceData> ret = newDevices;
        newDevices = new LinkedList<IDeviceData>();
        return ret;
    }

    /**
     * Gets a list of devices that are currently stopped
     * 
     * @param stepNum The current step number
     * @return The list of currently stopped devices
     */
    @Override
    public Iterable<IDeviceData> getStoppedDevices(int stepNum) {
        if ((currStopped == null) || (currStep != stepNum)) {
            currStopped = shutdownDevices;
            shutdownDevices = new LinkedList<IDeviceData>();
            currStep = stepNum;
        }
        return currStopped;
    }

    /**
     * Sets new devices into the IDeviceData
     * 
     * @param stepNum The current step number
     * @param newDevices The list of new devices to add
     */
    @Override
    public void putNewDevices(int stepNum, Iterable<? extends IDeviceData> newDevices) {
        for (IDeviceData dat : newDevices) {
            if (dat instanceof AbstractEmbeddedDeviceData) {
                AbstractEmbeddedDeviceData embeddedDevice = (AbstractEmbeddedDeviceData)dat;
                String vehId = embeddedDevice.getVehicleId();

                List<String> deviceProperIds = activeVehicles.get(vehId);
                if (deviceProperIds == null) {
                    deviceProperIds = new LinkedList<String>();
                    activeVehicles.put(vehId, deviceProperIds);
                }
                deviceProperIds.add(dat.getProperId());

            }
            activeDevices.put(dat.getProperId(), dat);
            this.newDevices.add(dat);
        }
    }

    /**
     * Sets vehicles to log out of the active devices
     * 
     * @param stepNum The current step number
     * @param logoutVehicles The vehicles to logout of the devices
     */
    @Override
    public void putLogoutVehicles(int stepNum, Iterable<? extends IDable> logoutVehicles) {
        if (logoutVehicles == null) {
            return;
        }

        for (IDable vi : logoutVehicles) {
            List<String> deviceProperIds = activeVehicles.remove(vi.getProperId());
            if (deviceProperIds != null) {
                for (String deviceProperId : deviceProperIds) {
                    IDeviceData dat = activeDevices.remove(deviceProperId);
                    if (dat != null) {
                        shutdownDevices.add(dat);
                    }
                }
            }
        }
    }

    /**
     * Stops all active devices
     * 
     * @return shutdownDevices The list values of the devices that were shut down
     */
    @Override
    public Iterable<IDeviceData> stopAllDevices() {
        shutdownDevices = activeDevices.values();
        activeDevices.clear();
        activeVehicles.clear();
        currStopped = null;
        return shutdownDevices;
    }

    @Override
    public List<String> putStalledVehicle(IDable oldVehicle, IDable newVehicle) {

        throw new UnsupportedOperationException();
    }

    @Override
    public void putReturnedVehicle(IDable vehicle) {

        throw new UnsupportedOperationException();
    }

    @Override
    public List<String> getDeviceProperIds(IDable vehicle) {

        return activeVehicles.get(vehicle.getProperId());
    }
}
