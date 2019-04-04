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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.test.GenVehicleFunctions;
import org.etexascode.test.TestAppLoggerReadOutput;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class PhaseFailureMOETest {

    List<IVehicle> zeroOverlap1 = null;

    List<IVehicle> zeroOverlap2 = null;

    List<IVehicle> oneOverlap1 = null;

    List<IVehicle> oneOverlap2 = null;

    List<IVehicle> twoOverlap1 = null;

    List<IVehicle> twoOverlap2 = null;

    Map<String, Set<Integer>> lightChanges = null;

    Map<Integer, List<IVehicle>> onGreenQueues = null;

    TestAppLoggerReadOutput testLogger = null;

    PhaseFailureMOE pfmoe = null;

    Set<Integer> testSet = null;

    int fail1 = 4;

    int fail2 = 7;

    int fail3 = -3;

    @Before
    public void setup() {
        testLogger = new TestAppLoggerReadOutput();
        pfmoe = new PhaseFailureMOE();

        zeroOverlap1 = genZeroOverlap1();
        zeroOverlap2 = genZeroOverlap2();
        oneOverlap1 = genOneOverlap1();
        oneOverlap2 = genOneOverlap2();
        twoOverlap1 = genTwoOverlap1();
        twoOverlap2 = genTwoOverlap2();

        onGreenQueues = new HashMap<Integer, List<IVehicle>>();

        onGreenQueues.put(0, zeroOverlap1);
        onGreenQueues.put(1, zeroOverlap2);
        onGreenQueues.put(2, oneOverlap1);

        testSet = new HashSet<Integer>();

        testSet.add(0);
        testSet.add(2);
        testSet.add(4);

        lightChanges = new HashMap<String, Set<Integer>>();

        lightChanges.put("GREEN-RED", new HashSet<Integer>());
        lightChanges.put("YELLOW-RED", new HashSet<Integer>());
        lightChanges.put("RED-GREEN", new HashSet<Integer>());
        lightChanges.put("YELLOW-GREEN", new HashSet<Integer>());
    }

    @After
    public void teardown() {
        testLogger = null;
        pfmoe = null;
        zeroOverlap1 = null;
        zeroOverlap2 = null;
        oneOverlap1 = null;
        oneOverlap2 = null;
        twoOverlap1 = null;
        twoOverlap2 = null;
        onGreenQueues = null;
        testSet = null;
        lightChanges = null;
    }

    @Test
    public void testGetPhaseFails() {
        assertEquals(0, pfmoe.getPhaseFails(onGreenQueues, lightChanges));
    }

    @Test
    public void testOnGreen() {
        pfmoe.onGreen(onGreenQueues, testSet);
        assertTrue(pfmoe.onPrevGreen.get(0) == zeroOverlap1);
        assertTrue(pfmoe.onPrevGreen.get(2) == oneOverlap1);
        assertTrue(pfmoe.onPrevGreen.get(4) == null);
    }

    @Test
    public void testOnRed1() {
        pfmoe.onPrevGreen.put(2, oneOverlap1);
        assertEquals(1, pfmoe.onRed(onGreenQueues, testSet));
    }

    @Test
    public void testOnRed2() {
        pfmoe.onPrevGreen.put(2, oneOverlap1);
        pfmoe.onPrevGreen.put(0, zeroOverlap1);
        assertEquals(2, pfmoe.onRed(onGreenQueues, testSet));
    }

    @Test
    public void testIsOverlap0() {
        assertEquals(0, pfmoe.isOverlap(zeroOverlap1, zeroOverlap2));
    }

    @Test
    public void testIsOverlap1() {
        assertEquals(1, pfmoe.isOverlap(oneOverlap1, oneOverlap2));
    }

    @Test
    public void testIsOverlap2() {
        assertEquals(1, pfmoe.isOverlap(twoOverlap1, twoOverlap2));
    }

    @Test
    public void testGetTotalFails() {
        assertEquals(0, pfmoe.runningFails);
        int f1 = pfmoe.getTotalFails(fail1);
        assertEquals(fail1, f1);
        assertEquals(f1, pfmoe.runningFails);
        int f2 = pfmoe.getTotalFails(fail2);
        assertEquals(fail1 + fail2, f2);
        assertEquals(f2, pfmoe.runningFails);
        int f3 = pfmoe.getTotalFails(fail3);
        assertEquals(fail1 + fail2 + fail3, f3);
        assertEquals(f3, pfmoe.runningFails);
    }

    @Test
    public void testWriteLogs() {
        PhaseFailureMOE.writeLogs(4, 13, testLogger);
        assertTrue(containsKeyValue(testLogger, "Phase Fails This Step", "4"));
        assertTrue(containsKeyValue(testLogger, "Total Phase Fails Right Now", "13"));
    }

    private boolean containsKeyValue(TestAppLoggerReadOutput logger, String key, String value) {
        for (String[] log : logger.logs) {
            if (log[0].equals(key) && log[1].equals(value)) {
                return true;
            }
        }

        return false;
    }

    private List<IVehicle> genZeroOverlap1() {
        List<IVehicle> ret = new ArrayList<IVehicle>(1);
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 0));
        return ret;
    }

    private List<IVehicle> genZeroOverlap2() {
        List<IVehicle> ret = new ArrayList<IVehicle>(1);
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 1));
        return ret;
    }

    private List<IVehicle> genOneOverlap1() {
        List<IVehicle> ret = new ArrayList<IVehicle>(1);
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 0));
        return ret;
    }

    private List<IVehicle> genOneOverlap2() {
        List<IVehicle> ret = new ArrayList<IVehicle>(1);
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 0));
        return ret;
    }

    private List<IVehicle> genTwoOverlap1() {
        List<IVehicle> ret = new ArrayList<IVehicle>(1);
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 0));
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 1));
        return ret;
    }

    private List<IVehicle> genTwoOverlap2() {
        List<IVehicle> ret = new ArrayList<IVehicle>(1);
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 1));
        ret.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, 0));
        return ret;
    }
}
