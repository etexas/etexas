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
package org.etexascode.apps.microscopicmodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.test.TestAppLoggerReadOutput;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class MicroscopicModelDriverTest {

    IVehicleManager vmi = null;

    ILaneManager lmi = null;

    ISignalManager smi = null;

    TestAppLoggerReadOutput testLogger = null;

    MicroscopicModelDriver mmd = null;

    @Before
    public void setup() {
        vmi = new VehicleManager();
        lmi = new LaneManager();
        smi = new SignalManager();
        testLogger = new TestAppLoggerReadOutput();
        mmd = new MicroscopicModelDriver();
    }

    @After
    public void teardown() {
        vmi = null;
        lmi = null;
        smi = null;
        testLogger = null;
        mmd = null;
    }

    @Test
    public void testUpdate() {
        mmd.update(vmi, lmi, smi, 0.0, testLogger);
        assertEquals(2, testLogger.logs.size());
        assertTrue(testLogger.containsKeyValue("Phase Fails This Step", "" + 0));
        assertTrue(testLogger.containsKeyValue("Total Phase Fails Right Now", "" + 0));
    }
}
