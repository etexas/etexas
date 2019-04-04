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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the default vehicle ID component.
 * 
 * @author ttevendale
 */
public class DefaultVehicleIdComponentTest {

    private IVehicleIdComponent vehicleIdComponent;

    @Before
    public void init() {

        vehicleIdComponent = new DefaultVehicleIdComponent();
    }

    @After
    public void cleanup() {

        vehicleIdComponent = null;
    }

    @Test
    public void testAddVehicleId() {

        long globalId = 1;
        IVehicle vehicle = new Vehicle(10, 300, 100, 0, 0, 0);

        // assure that the vehicle doesn't exist yet
        assertNull(vehicleIdComponent.getGlobalVehicleId(vehicle.getProperId()));
        assertNull(vehicleIdComponent.getProperVehicleId(vehicle.getGlobalId()));

        // add vehicle and make sure that the global ID starts at 1
        assertTrue(globalId == vehicleIdComponent.addVehicleId(vehicle));

        // ensure that the vehicle has been added
        assertTrue(globalId == vehicleIdComponent.getGlobalVehicleId(vehicle.getProperId()));
        assertTrue(vehicle.getProperId().equals(vehicleIdComponent.getProperVehicleId(globalId)));

        // add vehicle and make sure that the global ID stays the same
        assertTrue(globalId == vehicleIdComponent.addVehicleId(vehicle));

        // ensure that the vehicle is still there
        assertTrue(globalId == vehicleIdComponent.getGlobalVehicleId(vehicle.getProperId()));
        assertTrue(vehicle.getProperId().equals(vehicleIdComponent.getProperVehicleId(globalId)));
    }

    @Test
    public void testAddVehicleId2() {

        long globalId = -1;
        IVehicle vehicle = new Vehicle(-5, 300, 100, 0, 0, 0);

        // assure that the vehicle doesn't exist yet
        assertNull(vehicleIdComponent.getGlobalVehicleId(vehicle.getProperId()));
        assertNull(vehicleIdComponent.getProperVehicleId(vehicle.getGlobalId()));

        // add vehicle and make sure that the global ID starts at -1
        assertTrue(globalId == vehicleIdComponent.addVehicleId(vehicle));

        // ensure that the vehicle has been added
        assertTrue(globalId == vehicleIdComponent.getGlobalVehicleId(vehicle.getProperId()));
        assertTrue(vehicle.getProperId().equals(vehicleIdComponent.getProperVehicleId(globalId)));

        // add vehicle and make sure that the global ID stays the same
        assertTrue(globalId == vehicleIdComponent.addVehicleId(vehicle));

        // ensure that the vehicle is still there
        assertTrue(globalId == vehicleIdComponent.getGlobalVehicleId(vehicle.getProperId()));
        assertTrue(vehicle.getProperId().equals(vehicleIdComponent.getProperVehicleId(globalId)));
    }

    @Test
    public void testNextInjectedVehicleId() {

        for (int i = 1; i < 65536; i++) {

            assertTrue(i == vehicleIdComponent.nextInjectedVehicleId());
        }
    }

    @Test
    public void testStalledVehicle() {

        long globalId = 1;
        IVehicle oldVehicle = new Vehicle(1, 300, 100, 0, 0, 0);
        IVehicle newVehicle = new Vehicle(2, 300, 100, 0, 0, 0);

        // assure that the vehicle doesn't exist yet and isn't stalled
        assertNull(vehicleIdComponent.getGlobalVehicleId(oldVehicle.getProperId()));
        assertNull(vehicleIdComponent.getProperVehicleId(oldVehicle.getGlobalId()));
        assertFalse(vehicleIdComponent.isStalledVehicle(oldVehicle));

        // add vehicle and make sure that the global ID starts at 1
        assertTrue(globalId == vehicleIdComponent.addVehicleId(oldVehicle));

        // ensure that the vehicle has been added
        assertTrue(globalId == vehicleIdComponent.getGlobalVehicleId(oldVehicle.getProperId()));
        assertTrue(oldVehicle.getProperId().equals(vehicleIdComponent.getProperVehicleId(globalId)));

        vehicleIdComponent.putStalledVehicle(oldVehicle, newVehicle);

        // assure that the vehicle is stalled and that the old vehicle is gone
        assertNull(vehicleIdComponent.getGlobalVehicleId(oldVehicle.getProperId()));
        assertNull(vehicleIdComponent.getProperVehicleId(oldVehicle.getGlobalId()));
        assertNull(vehicleIdComponent.getGlobalVehicleId(newVehicle.getProperId()));
        assertNull(vehicleIdComponent.getProperVehicleId(newVehicle.getGlobalId()));
        assertTrue(vehicleIdComponent.isStalledVehicle(newVehicle));

        vehicleIdComponent.putReturnedVehicle(newVehicle);

        // ensure that the vehicle has been added back
        assertTrue(globalId == vehicleIdComponent.getGlobalVehicleId(newVehicle.getProperId()));
        assertTrue(newVehicle.getProperId().equals(vehicleIdComponent.getProperVehicleId(globalId)));
    }
}
