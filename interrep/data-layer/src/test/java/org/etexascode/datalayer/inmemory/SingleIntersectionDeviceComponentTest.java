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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.etexascode.devicedata.CellDeviceData;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.testclasses.TestDeviceData;
import org.etexascode.devicedata.testclasses.TestIDable;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class SingleIntersectionDeviceComponentTest {

    List<IDeviceData> rar = null;

    SingleIntersectionDeviceComponent sidc = null;

    TestDeviceData tdd = null;

    String testId = null;

    Iterable<IDable> logoutIds = null;

    @Before
    public void setup() {
        testId = "testid";
        rar = new ArrayList<IDeviceData>(1);
        tdd = new TestDeviceData(testId, null);
        rar.add(tdd);

        logoutIds = new ArrayList<IDable>(1);
        ((ArrayList<IDable>)logoutIds).add(new TestIDable(testId));

        sidc = new SingleIntersectionDeviceComponent(rar);
    }

    @After
    public void teardown() {
        rar = null;
        tdd = null;
        sidc = null;
        testId = null;
        logoutIds = null;
    }

    @Test
    public void testConstructor() {
        SingleIntersectionDeviceComponent sidc = new SingleIntersectionDeviceComponent(rar);
        assertEquals(1, sidc.activeDevices.size());
        assertEquals(1, sidc.newDevices.size());
    }

    @Test
    public void testGetActiveDevices() {
        for (IDeviceData idd : sidc.getActiveDevices(0)) {
            assertTrue(rar.contains(idd));
        }
    }

    @Test
    public void testGetNewDevices() {
        assertEquals(rar, sidc.getNewDevices(0));
        assertEquals(0, sidc.newDevices.size());
    }

    @Test
    public void testGetStoppedDevices() {
        Iterable<IDeviceData> idd1 = sidc.getStoppedDevices(0);
        Iterable<IDeviceData> idd2 = sidc.getStoppedDevices(0);
        Iterable<IDeviceData> idd3 = sidc.getStoppedDevices(1);
        assertTrue(idd1 == idd2);
        assertFalse(idd1 == idd3);
    }

    @Test
    public void testPutNewDevices1() {
        sidc.putNewDevices(0, rar);
        assertEquals(2, sidc.newDevices.size());
        assertEquals(1, sidc.activeDevices.size());
    }

    @Test
    public void testPutNewDevices2() {
        sidc.activeDevices.clear();
        sidc.newDevices.clear();
        sidc.putNewDevices(0, rar);
        assertEquals(1, sidc.newDevices.size());
        assertEquals(1, sidc.activeDevices.size());
    }

    /**
     * Tests the <code>putNewDevices</code> method in the default devices component.
     */
    @Test
    public void testPutNewDevices3() {

        // add 2 devices from the same vehicle and 1 standalone device
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        OBUDeviceData obuDevice = new OBUDeviceData(201, null, vehicle.getProperId());
        CellDeviceData cellDevice = new CellDeviceData(202, null, vehicle.getProperId(), Integer.toString(1));
        FixedCellDeviceData fixedCellDevice = new FixedCellDeviceData(203, null, 203040, 100, 100, 100);
        sidc.putNewDevices(0, Arrays.asList(obuDevice, cellDevice, fixedCellDevice));

        // confirm that the devices were added as active devices
        boolean obuAdded = false;
        boolean cellAdded = false;
        boolean fixedCellAdded = false;

        for (IDeviceData device : sidc.getActiveDevices(0)) {

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

        for (IDeviceData device : sidc.getNewDevices(0)) {

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
        assertFalse(sidc.getNewDevices(0).iterator().hasNext());
    }

    @Test
    public void testPutLogoutVehicles1() {
        sidc.putLogoutVehicles(0, null);
        assertEquals(1, sidc.activeDevices.size());
        assertEquals(0, sidc.shutdownDevices.size());
    }

    @Test
    public void testPutLogoutVehicles2() {
        for (IDable vi : logoutIds) {
            sidc.activeDevices.remove(vi.getProperId());
        }
        sidc.putLogoutVehicles(0, logoutIds);
        assertEquals(0, sidc.shutdownDevices.size());
    }

    /**
     * Tests the <code>putLogoutVehicles</code> method in the default devices component.
     */
    @Test
    public void testPutLogoutVehicles3() {

        // add a device from a vehicle
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        OBUDeviceData obuDevice = new OBUDeviceData(201, null, vehicle.getProperId());
        sidc.putNewDevices(0, Arrays.asList(obuDevice));

        // logout the source vehicle
        sidc.putLogoutVehicles(0, Arrays.asList(vehicle));

        // confirm that the device was stopped
        boolean obuStopped = false;

        for (IDeviceData device : sidc.getStoppedDevices(0)) {

            if (device.equals(obuDevice)) {

                obuStopped = true;
            }
        }

        assertTrue(obuStopped);

        // confirm that logging out a nonexistent vehicle does not break the method
        sidc.putLogoutVehicles(0, Arrays.asList(new Vehicle(2, 100.0, 50.0, 0.0, 0.0, 0.0)));

        // confirm that a null vehicle list does not break the method
        sidc.putLogoutVehicles(0, null);
    }

    @Test
    public void testStopAllDevices() {
        Iterable<IDeviceData> idd = sidc.stopAllDevices();
        assertTrue(idd == sidc.shutdownDevices);
        Iterable<IDeviceData> devs = sidc.getStoppedDevices(0);
        assertTrue(idd == devs);
    }
}
