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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.devicedata.CellDeviceData;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.Vehicle;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the default devices component.
 * 
 * @author emyers
 */
public class DefaultDevicesComponentTest {

    /** The devices component. */
    private IDevicesComponent devicesComponent;

    /**
     * Creates a new default devices component before each test.
     */
    @Before
    public void init() {

        List<IDeviceData> devices = new ArrayList<IDeviceData>();
        devices.add(new FixedCellDeviceData(101, null, 102030, 0, 0, 0));
        devices.add(new RSEDeviceData(102, null, new ReferencePoint[0], new int[] { 0 }, 50, 50, 50));
        devicesComponent = new DefaultDevicesComponent(devices);
    }

    /**
     * Tests the <code>getStoppedDevices</code> method in the default devices component.
     */
    @Test
    public void testGetStoppedDevices() {

        // confirm that the existing devices were stopped
        devicesComponent.stopAllDevices();
        assertTrue(devicesComponent.getStoppedDevices(0).iterator().hasNext());

        // confirm that the stopped devices remain for repeated calls
        assertTrue(devicesComponent.getStoppedDevices(0).iterator().hasNext());

        // confirm that the stopped devices are removed for future time steps
        assertFalse(devicesComponent.getStoppedDevices(1).iterator().hasNext());
    }

    /**
     * Tests the <code>putLogoutVehicles</code> method in the default devices component.
     */
    @Test
    public void testPutLogoutVehicles() {

        // add a device from a vehicle
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        OBUDeviceData obuDevice = new OBUDeviceData(201, null, vehicle.getProperId());
        devicesComponent.putNewDevices(0, Arrays.asList(obuDevice));

        // logout the source vehicle
        devicesComponent.putLogoutVehicles(0, Arrays.asList(vehicle));

        // confirm that the device was stopped
        boolean obuStopped = false;

        for (IDeviceData device : devicesComponent.getStoppedDevices(0)) {

            if (device.equals(obuDevice)) {

                obuStopped = true;
            }
        }

        assertTrue(obuStopped);

        // confirm that logging out a nonexistent vehicle does not break the method
        devicesComponent.putLogoutVehicles(0, Arrays.asList(new Vehicle(2, 100.0, 50.0, 0.0, 0.0, 0.0)));

        // confirm that a null vehicle list does not break the method
        devicesComponent.putLogoutVehicles(0, null);
    }

    /**
     * Tests the <code>putNewDevices</code> method in the default devices component.
     */
    @Test
    public void testPutNewDevices() {

        // add 2 devices from the same vehicle and 1 standalone device
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        OBUDeviceData obuDevice = new OBUDeviceData(201, null, vehicle.getProperId());
        CellDeviceData cellDevice = new CellDeviceData(202, null, vehicle.getProperId(), Integer.toString(1));
        FixedCellDeviceData fixedCellDevice = new FixedCellDeviceData(203, null, 203040, 100, 100, 100);
        devicesComponent.putNewDevices(0, Arrays.asList(obuDevice, cellDevice, fixedCellDevice));

        // confirm that the devices were added as active devices
        boolean obuAdded = false;
        boolean cellAdded = false;
        boolean fixedCellAdded = false;

        for (IDeviceData device : devicesComponent.getActiveDevices(0)) {

            if (device.equals(obuDevice)) {

                obuAdded = true;
            }
            else if (device.equals(cellDevice)) {

                cellAdded = true;
            }
            else if (device.equals(fixedCellDevice)) {

                fixedCellAdded = true;
            }
        }

        assertTrue(obuAdded && cellAdded && fixedCellAdded);

        // confirm that the devices were added as new devices
        obuAdded = false;
        cellAdded = false;
        fixedCellAdded = false;

        for (IDeviceData device : devicesComponent.getNewDevices(0)) {

            if (device.equals(obuDevice)) {

                obuAdded = true;
            }
            else if (device.equals(cellDevice)) {

                cellAdded = true;
            }
            else if (device.equals(fixedCellDevice)) {

                fixedCellAdded = true;
            }
        }

        assertTrue(obuAdded && cellAdded && fixedCellAdded);

        // confirm that the new devices call reset the new devices
        assertFalse(devicesComponent.getNewDevices(0).iterator().hasNext());
    }

    /**
     * Tests the <code>stopAllDevices</code> method in the default devices component.
     */
    @Test
    public void testStopAllDevices() {

        // add 2 devices from the same vehicle and 1 standalone device
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        OBUDeviceData obuDevice = new OBUDeviceData(201, null, vehicle.getProperId());
        CellDeviceData cellDevice = new CellDeviceData(202, null, vehicle.getProperId(), Integer.toString(1));
        FixedCellDeviceData fixedCellDevice = new FixedCellDeviceData(203, null, 203040, 100, 100, 100);
        devicesComponent.putNewDevices(0, Arrays.asList(obuDevice, cellDevice, fixedCellDevice));

        boolean obuStopped = false;
        boolean cellStopped = false;
        boolean fixedCellStopped = false;

        // confirm that all devices are stopped
        for (IDeviceData device : devicesComponent.stopAllDevices()) {

            if (device.equals(obuDevice)) {

                obuStopped = true;
            }
            else if (device.equals(cellDevice)) {

                cellStopped = true;
            }
            else if (device.equals(fixedCellDevice)) {

                fixedCellStopped = true;
            }
        }

        assertTrue(obuStopped && fixedCellStopped && cellStopped);

        // confirm that there are no more active devices
        assertFalse(devicesComponent.getActiveDevices(0).iterator().hasNext());
    }
}
