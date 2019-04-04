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
package org.etexascode.datalayer.inmemory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.devicedata.AbstractEmbeddedDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * The default devices component.
 * 
 * @author ablatt
 * @author emyers
 * @author ttevendale
 */
public class DefaultDevicesComponent implements IDevicesComponent {

    /** The collection of new devices. */
    private Collection<IDeviceData> newDevices;

    /** The collection of shutdown devices. */
    private Collection<IDeviceData> shutdownDevices;

    /** The current step number. */
    private int stepNum;

    /** The iterable stopped devices. */
    private Iterable<IDeviceData> stoppedDevices;

    /** The map of active devices. */
    private Map<String, IDeviceData> activeDevicesMap;

    /** The map of active vehicles. */
    private Map<String, List<String>> activeVehiclesMap;

    /** The map of stalled vehicles. */
    private Map<String, List<IDeviceData>> stalledVehiclesMap;

    /**
     * Creates a new <code>DefaultDevicesComponent</code> instance.
     * 
     * @param devices The list of devices.
     */
    public DefaultDevicesComponent(List<IDeviceData> devices) {

        stepNum = Integer.MIN_VALUE;
        newDevices = new ArrayList<IDeviceData>();
        shutdownDevices = new ArrayList<IDeviceData>();
        activeDevicesMap = new HashMap<String, IDeviceData>();
        activeVehiclesMap = new HashMap<String, List<String>>();
        stalledVehiclesMap = new HashMap<String, List<IDeviceData>>();

        for (IDeviceData device : devices) {

            newDevices.add(device);
            activeDevicesMap.put(device.getProperId(), device);
        }
    }

    @Override
    public Iterable<IDeviceData> getActiveDevices(int stepNum) {

        return activeDevicesMap.values();
    }

    @Override
    public List<String> getDeviceProperIds(IDable vehicle) {

        List<String> deviceProperIds = activeVehiclesMap.get(vehicle.getProperId());
        return (deviceProperIds != null) ? deviceProperIds : new ArrayList<String>();
    }

    @Override
    public Iterable<IDeviceData> getNewDevices(int stepNum) {

        Collection<IDeviceData> devices = newDevices;
        newDevices = new ArrayList<IDeviceData>();
        return devices;
    }

    @Override
    public Iterable<IDeviceData> getStoppedDevices(int stepNum) {

        if ((stoppedDevices == null) || (this.stepNum != stepNum)) {

            stoppedDevices = shutdownDevices;
            shutdownDevices = new ArrayList<IDeviceData>();
            this.stepNum = stepNum;
        }

        return stoppedDevices;
    }

    @Override
    public void putLogoutVehicles(int stepNum, Iterable<? extends IDable> logoutVehicles) {

        if (logoutVehicles == null) {

            return;
        }

        for (IDable vehicle : logoutVehicles) {

            List<String> deviceProperIds = activeVehiclesMap.remove(vehicle.getProperId());

            if (deviceProperIds != null) {

                for (String deviceProperId : deviceProperIds) {

                    IDeviceData device = activeDevicesMap.remove(deviceProperId);

                    if (device != null) {

                        shutdownDevices.add(device);
                    }
                }
            }
        }
    }

    @Override
    public void putNewDevices(int stepNum, Iterable<? extends IDeviceData> newDevices) {

        for (IDeviceData device : newDevices) {

            if (device instanceof AbstractEmbeddedDeviceData) {

                String vehId = ((AbstractEmbeddedDeviceData)device).getVehicleId();
                List<String> deviceProperIds = activeVehiclesMap.get(vehId);

                if (deviceProperIds == null) {

                    deviceProperIds = new ArrayList<String>();
                    activeVehiclesMap.put(vehId, deviceProperIds);
                }

                deviceProperIds.add(device.getProperId());
            }

            activeDevicesMap.put(device.getProperId(), device);
            this.newDevices.add(device);
        }
    }

    @Override
    public void putReturnedVehicle(IDable vehicle) {

        List<IDeviceData> devices = stalledVehiclesMap.remove(vehicle.getProperId());

        if (devices == null) {

            return;
        }

        List<String> deviceProperIds = new ArrayList<String>();
        for (IDeviceData device : devices) {

            deviceProperIds.add(device.getProperId());
            activeDevicesMap.put(device.getProperId(), device);
        }

        activeVehiclesMap.put(vehicle.getProperId(), deviceProperIds);
    }

    @Override
    public List<String> putStalledVehicle(IDable oldVehicle, IDable newVehicle) {

        List<String> deviceProperIds = activeVehiclesMap.remove(oldVehicle.getProperId());

        if (deviceProperIds == null) {

            return new ArrayList<String>();
        }

        List<String> newDeviceProperIds = new ArrayList<String>();
        List<IDeviceData> stalledDevices = new ArrayList<IDeviceData>();
        for (String deviceProperId : deviceProperIds) {

            AbstractEmbeddedDeviceData deviceData = (AbstractEmbeddedDeviceData)activeDevicesMap.remove(deviceProperId);
            deviceData.setVehicleId(newVehicle.getProperId());
            stalledDevices.add(deviceData);
            newDeviceProperIds.add(deviceData.getProperId());
        }
        stalledVehiclesMap.put(newVehicle.getProperId(), stalledDevices);

        return newDeviceProperIds;
    }

    @Override
    public Iterable<IDeviceData> stopAllDevices() {

        shutdownDevices.clear();
        shutdownDevices.addAll(activeDevicesMap.values());
        activeDevicesMap.clear();
        activeVehiclesMap.clear();
        stoppedDevices = null;
        return shutdownDevices;
    }
}
