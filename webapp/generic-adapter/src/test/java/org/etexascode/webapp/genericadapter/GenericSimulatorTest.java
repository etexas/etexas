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

package org.etexascode.webapp.genericadapter;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.rmi.RemoteException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.powermock.api.mockito.PowerMockito;

/**
 * @author ablatt
 */
public class GenericSimulatorTest {

    GenericManagedConnection gmc = null;

    TestSim ts = null;

    GenericSimulator gs = null;

    @Before
    public void setup() {
        gmc = PowerMockito.mock(GenericManagedConnection.class);
        ts = new TestSim();
        PowerMockito.when(gmc.getSimDriver()).thenReturn(ts);
        gs = new GenericSimulator(gmc);
    }

    @After
    public void tearDown() {
        gmc = null;
        ts = null;
        gs = null;
    }

    @Test
    public void testConstructor() {
        GenericSimulator gs = new GenericSimulator(null);
        assertTrue(gs instanceof GenericSimulator);
    }

    @Test(expected = NullPointerException.class)
    public void testInvalidate() throws RemoteException {
        gs.invalidate();
        gs.getStaticData();
    }

    @Test(expected = NullPointerException.class)
    public void testAssociateConnection() throws RemoteException {
        gs.associateConnection(null);
        gs.getStaticData();
    }

    @Test(expected = NullPointerException.class)
    public void testClose1() throws RemoteException {
        gs.close();
        gs.getStaticData();
    }

    @Test(expected = NullPointerException.class)
    public void testClose2() throws RemoteException {
        gs.invalidate();
        gs.close();
        gs.getStaticData();
    }

    @Test
    public void testGetStaticData() {
        assertFalse(ts.gotStatic);

        try {
            assertNull(gs.getStaticData());
        }
        catch (RemoteException e) {
            e.printStackTrace();
        }

        assertTrue(ts.gotStatic);
    }

    @Test
    public void testGetStepData() {
        assertFalse(ts.gotStep);

        try {
            assertNull(ts.getStepData(42));
        }
        catch (RemoteException e) {
            e.printStackTrace();
        }

        assertTrue(ts.gotStep);
    }

    @Test
    public void testAddVehicleCommand() {
        assertFalse(ts.isVehCom);

        try {
            ts.addVehicleCommand(null);
        }
        catch (RemoteException e) {
            e.printStackTrace();
        }

        assertTrue(ts.isVehCom);
    }

    @Test
    public void testAddSignalCommand() {
        assertFalse(ts.isSigCom);

        try {
            ts.addSignalCommand(null);
        }
        catch (RemoteException e) {
            e.printStackTrace();
        }

        assertTrue(ts.isSigCom);
    }

    @Test
    public void testInit() {
        // Note: init does nothing in this context...
        try {
            ts.init(null);
        }
        catch (RemoteException e) {
            e.printStackTrace();
            assertTrue(false);
        }
        assertTrue(true);
    }
}
