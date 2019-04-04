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
package org.etexascode.appslayerdata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jconnelly
 */
public class AppShutdownLayerInputTest {

    private long deviceId = -1;

    IConnectedVehicleApp<String> app = null;

    AppShutdownLayerInput asli = null;

    @Before
    public void setup() {
        deviceId = 9876;
        app = new IConnectedVehicleApp<String>() {

            @Override
            public void performUpdate(String device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {}
        };
        asli = produceAppSLI();
    }

    @After
    public void teardown() {
        deviceId = -1;
        app = null;
        asli = null;
    }

    @Test
    public void testAppShutdownLayerInput() {
        assertEquals(asli, produceAppSLI());
    }

    @Test
    public void testEqualsTrue() {
        assertTrue(asli.equals(produceAppSLI()));
    }

    @Test
    public void testEqualsFalse() {
        AppShutdownLayerInput testF = new AppShutdownLayerInput(1234, app);
        assertFalse(asli.equals(testF));
        String str = "";
        long num = (long)0;
        DistanceImpl location = new DistanceImpl(0, 0, 0);
        AppLayerOutput test2 = new AppLayerOutput(str, num, location);
        assertFalse(asli.equals(test2));
    }

    @Test
    public void testEqualsFalse2() {
        IConnectedVehicleApp<String> diffApp = new IConnectedVehicleApp<String>() {

            @Override
            public void performUpdate(String device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {}
        };

        AppShutdownLayerInput testF = new AppShutdownLayerInput(deviceId, diffApp);
        assertFalse(asli.equals(testF));
    }

    @Test
    public void testHashCode() {
        assertEquals(asli.hashCode(), produceAppSLI().hashCode());
    }

    private AppShutdownLayerInput produceAppSLI() {
        AppShutdownLayerInput appin = new AppShutdownLayerInput(deviceId, app);
        return appin;
    }
}
