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
package org.etexascode.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 */
public class TestHarnessCaseTest {

    /**
     * Initialize TestHarnessCase and Mangers
     */
    TestHarnessCase thc = new TestHarnessCase();

    SignalManager sm = thc.getSignalManager();

    LaneManager lm = thc.getLaneManager();

    DetectorManager dm = thc.getDetectorManager();

    VehicleManager vm = thc.getVehicleManager();

    private List<IConnectedVehicleApp<?>> appList;

    OBUDeviceData obu = thc.getTestDeviceOBU(appList, "1");

    RSEDeviceData rse = thc.getTestDeviceRSE(appList, null, 1, 0, 0, 0, 0);

    TestInterRep interrep = null;

    @Before
    public void setUp() throws Exception {
        try {
            interrep = thc.getTestInterRep(true);
        }
        catch (TestException e) {
            throw new Exception("Could not create test InterRep", e);
        }
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testGetTestDeviceOBU() {
        assertTrue(obu instanceof OBUDeviceData);
    }

    @Test
    public void testGetTestDeviceRSE() {
        assertTrue(rse instanceof RSEDeviceData);
    }

    @Test
    public void testGetSignalManager() {
        assertTrue(sm instanceof SignalManager);
    }

    @Test
    public void testGetLaneManager() {
        assertTrue(lm instanceof LaneManager);
    }

    @Test
    public void testGetDetectorManager() {
        assertTrue(dm instanceof DetectorManager);
    }

    @Test
    public void testGetVehicleManager() {
        assertTrue(vm instanceof VehicleManager);
    }

    @Test
    public void testGetTestInterRep() {
        assertNotNull(interrep.getDetectorManager());
        assertNotNull(interrep.getLaneManager());
        assertNotNull(interrep.getSignalManager());
        assertNotNull(interrep.getVehicleManager());

    }

}
