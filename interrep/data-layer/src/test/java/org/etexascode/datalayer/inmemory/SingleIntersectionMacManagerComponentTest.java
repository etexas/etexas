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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.devicedata.testclasses.TestIDable;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.wavesim.WaveMessage;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class SingleIntersectionMacManagerComponentTest {

    int randomSeed = 0;

    String testId = null;

    SingleIntersectionMacManagerComponent simc = null;

    List<IDable> removeDeviceIds = null;

    long genedMac = 565297855087l;

    long expectedMac = 301145486492l;

    long plantedMac = 42l;

    List<IDable> testIds = null;

    TestIDable test = null;

    String testId2 = "something";

    @Before
    public void setup() {
        randomSeed = 1729;
        testId = null;
        simc = new SingleIntersectionMacManagerComponent(randomSeed, Arrays.asList(new FixedCellDeviceData(
                101, null, 102030, 0, 0, 0), new RSEDeviceData(102, null, new ReferencePoint[0], new int[] { 0 }, 50, 50, 50)));
        removeDeviceIds = new ArrayList<IDable>(1);
        removeDeviceIds.add(new TestIDable(testId));
        simc.currMacs.put(testId, plantedMac);

        test = new TestIDable(testId2);
        testIds = new ArrayList<IDable>(1);
        testIds.add(test);
    }

    @After
    public void teardown() {
        randomSeed = 0;
        simc = null;
        removeDeviceIds = null;
        testId = null;
        test = null;
        testIds = null;
    }

    @Test
    public void testConstructor() {
        SingleIntersectionMacManagerComponent simc = new SingleIntersectionMacManagerComponent(randomSeed, new ArrayList<IDeviceData>());
        assertEquals(new Random(randomSeed).nextInt(), simc.r.nextInt());
    }

    @Test
    public void testPutNewDevices1() {
        simc = new SingleIntersectionMacManagerComponent(randomSeed, new ArrayList<IDeviceData>());
        simc.putNewDevices(0, testIds);
        assertTrue(simc.currMacs.containsKey(testId2));
    }

    @Test
    public void testPutNewDevices2() {
        simc = new SingleIntersectionMacManagerComponent(randomSeed, new ArrayList<IDeviceData>());
        simc.allMacs = null;
        simc.putNewDevices(0, testIds);
        assertFalse(simc.currMacs.containsKey(testId2));
    }

    @Test
    public void testRemoveDevices1() {
        assertTrue(simc.currMacs.containsKey(testId));
        simc.removeDevices(0, removeDeviceIds);
        assertFalse(simc.currMacs.containsKey(testId));
    }

    @Test
    public void testRemoveDevices2() {
        simc.currMacs = null;
        simc.removeDevices(0, removeDeviceIds);
        assertNull(simc.currMacs);
    }

    @Test
    public void testGetMac1() {
        simc.allMacs = null;
        assertEquals(WaveMessage.MACBROADCAST, simc.getMac(0, testId).longValue());
    }

    @Test(expected = RuntimeException.class)
    public void testGetMac2() {
        simc.getMac(0, "not a real thing");
    }

    @Test
    public void testGetMac3() {
        assertEquals(plantedMac, simc.getMac(0, testId).longValue());
    }

    @Test
    public void testShutdown() {
        assertNotNull(simc.allMacs);
        assertNotNull(simc.currMacs);
        simc.shutdown();
        assertNull(simc.allMacs);
        assertNull(simc.currMacs);
    }

    @Test
    public void testGenMac() {
        simc.allMacs.add(expectedMac);
        assertEquals(genedMac, simc.genMac(simc.allMacs).longValue());
    }

    @Test
    public void testGenRandomMac() {
        Random r = new Random(randomSeed);
        assertEquals(expectedMac, simc.genRandomMac(r).longValue());
    }
}
