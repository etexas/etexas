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
import java.util.Arrays;
import java.util.Random;

import org.etexascode.datalayer.interfaces.IMacManagerComponent;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the default MAC manager component.
 * 
 * @author emyers
 */
public class DefaultMacManagerComponentTest {

    /** The MAC manager component. */
    private IMacManagerComponent macManagerComponent;

    /**
     * Creates a new default MAC manager component before each test.
     */
    @Before
    public void init() {

        macManagerComponent = new DefaultMacManagerComponent(new Random().nextInt(),
                Arrays.asList(new FixedCellDeviceData(101, null, 102030, 0, 0, 0), new RSEDeviceData(102, null, new ReferencePoint[0], new int[] { 0 }, 50, 50, 50)));
    }

    /**
     * Tests the ability to add new devices to the default MAC manager component.
     */
    @Test
    public void testPutNewDevices() {

        // add a new device from a vehicle
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        vehicle.setSimulationId(0);
        OBUDeviceData obuDevice = new OBUDeviceData(201, null, vehicle.getProperId());
        macManagerComponent.putNewDevices(0, Arrays.asList(obuDevice));

        // confirm that no exception is thrown when getting the device MAC address
        macManagerComponent.getMac(0, "Simulation:0-Vehicle:1");
    }

    /**
     * Tests the ability to remove devices from the default MAC manager component.
     */
    @Test(expected = RuntimeException.class)
    public void testRemoveDevices() {

        // add a new device
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        macManagerComponent.putNewDevices(0, Arrays.asList(new OBUDeviceData(201, null, vehicle.getProperId())));

        // remove the added device
        macManagerComponent.removeDevices(0, Arrays.asList(vehicle));

        // confirm that the device was removed
        macManagerComponent.getMac(0, vehicle.getProperId());
    }

    /**
     * Tests the ability to shut down the default MAC manager component.
     */
    @Test
    public void testShutdown() {

        // shutdown the component
        macManagerComponent.shutdown();

        // confirm that the method calls did not break
        macManagerComponent.getMac(0, "");
        macManagerComponent.putNewDevices(0, new ArrayList<IDeviceData>());
        macManagerComponent.removeDevices(0, new ArrayList<IVehicle>());
    }
}
