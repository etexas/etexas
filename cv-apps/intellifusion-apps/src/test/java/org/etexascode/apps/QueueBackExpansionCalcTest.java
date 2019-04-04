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
package org.etexascode.apps;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.test.TestAppLoggerReadOutput;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class QueueBackExpansionCalcTest {

    QueueBackExpansionMOE moe;

    QueueBackExpansionMOE moe2;

    TestAppLoggerReadOutput logOut;

    TestAppLoggerReadOutput logOut2;

    @Before
    public void setUp() {
        Set<Integer> s = new HashSet<Integer>();
        s.add(0);
        moe = new QueueBackExpansionMOE(s);
        moe2 = new QueueBackExpansionMOE(s);
        logOut = new TestAppLoggerReadOutput();
        logOut2 = new TestAppLoggerReadOutput();
    }

    @Test
    public void testConstructor2() {
        ILaneManager lm = new LaneManager();
        lm.getLanes().put(0, getLane());
        performChecks(new QueueBackExpansionMOE(lm), -1.0, -1.0, false);
        performChecks(moe, -1.0, -1.0, false);
    }

    @Test
    public void testConstructor() {
        Set<Integer> laneIds = new HashSet<Integer>();
        laneIds.add(getLane().getLaneId());
        performChecks(new QueueBackExpansionMOE(laneIds), -1.0, -1.0, false);
        performChecks(moe, -1.0, -1.0, false);
    }

    @Test
    public void testUpdate() {
        moe.update(genBackMap(6), genTestMap("RED-RED"), null);
        moe.update(genBackMap(6), genTestMap("RED-GREEN"), null);
        performChecks(moe, 6.0, 6.0, true);
        moe2.update(genBackMap(6), genTestMap("YELLOW-YELLOW"), null);
        moe2.update(genBackMap(6), genTestMap("YELLOW-GREEN"), null);
        performChecks(moe2, 6.0, 6.0, true);
        moe.update(genBackMap(7), genTestMap("GREEN-GREEN"), null);
        performChecks(moe, 6.0, 7.0, true);
        moe.update(genBackMap(8), genTestMap("GREEN-GREEN"), null);
        performChecks(moe, 6.0, 8.0, true);
        moe.update(genBackMap(7), genTestMap("GREEN-GREEN"), null);
        performChecks(moe, 6.0, 8.0, true);

        // bring moe 2 up to moe
        moe2.update(genBackMap(7), genTestMap("GREEN-GREEN"), null);
        moe2.update(genBackMap(8), genTestMap("GREEN-GREEN"), null);
        moe2.update(genBackMap(7), genTestMap("GREEN-GREEN"), null);
        moe.update(genBackMap(7), genTestMap("GREEN-YELLOW"), logOut);
        assertEquals(0, getLaneIdFromLogs(logOut));
        assertEquals(2.0, getMOEFromLogs(logOut), 0.05);
        moe2.update(genBackMap(7), genTestMap("GREEN-RED"), logOut2);
        assertEquals(0, getLaneIdFromLogs(logOut2));
        assertEquals(2.0, getMOEFromLogs(logOut2), 0.05);
    }

    @Test
    public void testToGreen() {
        moe.toGreen(0, genBackMap(6));
        performChecks(moe, 6.0, 6.0, true);
        moe.toGreen(1, null);
        performChecks(moe, 6.0, 6.0, true);
    }

    @Test
    public void testFromGreen() {
        moe.fromGreen(0, logOut);
        assertEquals(0, logOut.logs.size());
        moe.fromGreen(1, null);
        moe.toGreen(0, genBackMap(6));
        moe.fromGreen(0, logOut);
        assertEquals(0, getLaneIdFromLogs(logOut));
        assertEquals(0.0, getMOEFromLogs(logOut), 0.05);
        performChecks(moe, 6, 6, false);
    }

    @Test
    public void testOnGreen() {
        moe.onGreen(0, null);
        performChecks(moe, -1.0, -1.0, false);
        moe.onGreen(1, null);
        performChecks(moe, -1.0, -1.0, false);
        testToGreen();
        moe.onGreen(0, genBackMap(7));
        performChecks(moe, 6.0, 7.0, true);
        moe.onGreen(0, genBackMap(5));
        performChecks(moe, 6.0, 7.0, true);
    }

    /**
     * Gets the lane from the logs.
     * 
     * @param talro The reader output.
     * @return The lane ID.
     */
    private int getLaneIdFromLogs(TestAppLoggerReadOutput talro) {
        return Integer.parseInt(getMOEStr(talro).substring(0, 1));
    }

    /**
     * Gets the MOE from the logs.
     * 
     * @param talro The reader output.
     * @return The MOE.
     */
    private double getMOEFromLogs(TestAppLoggerReadOutput talro) {
        String tmp = getMOEStr(talro);
        return Double.parseDouble(tmp.substring(3, tmp.length()));
    }

    /**
     * Get the MOE String.
     * 
     * @param talro The reader output.
     * @return The string.
     */
    private String getMOEStr(TestAppLoggerReadOutput talro) {
        return talro.logs.get(talro.logs.size() - 1)[1];
    }

    /**
     * Generate the map.
     * 
     * @param back Which back.
     * @return The back map.
     */
    private Map<Integer, Double> genBackMap(double back) {
        Map<Integer, Double> ret = new HashMap<Integer, Double>();
        ret.put(0, back);
        return ret;
    }

    /**
     * Generates a test map.
     * 
     * @param key The key.
     * @return The map.
     */
    private Map<String, Set<Integer>> genTestMap(String key) {
        Map<String, Set<Integer>> ret = new HashMap<String, Set<Integer>>();
        String[] tmp = new String[] { "GREEN-GREEN", "RED-GREEN", "YELLOW-GREEN", "GREEN-YELLOW", "GREEN-RED" };
        for (String t : tmp) {
            ret.put(t, new HashSet<Integer>(0));
        }
        Set<Integer> value = new HashSet<Integer>(1);
        value.add(0);
        ret.put(key, value);
        return ret;
    }

    /**
     * Performs a check on the returned results.
     * 
     * @param qbem The expansion MOE.
     * @param backOnGreen The back on green value.
     * @param backMax The back max value.
     * @param hasBeenGreen True/False.
     */
    private void performChecks(QueueBackExpansionMOE qbem, double backOnGreen, double backMax, boolean hasBeenGreen) {
        assertEquals(1, qbem.queueBackMax.size());
        assertEquals(1, qbem.hasBeenGreen.size());
        assertEquals(1, qbem.queueBackOnGreen.size());
        assertEquals(backOnGreen, qbem.queueBackOnGreen.get(0).doubleValue(), 0.05);
        assertEquals(backMax, qbem.queueBackMax.get(0).doubleValue(), 0.05);
        assertEquals(hasBeenGreen, qbem.hasBeenGreen.get(0));
    }

    /**
     * Gets a lane.
     * 
     * @return The lane.
     */
    private Lane getLane() {
        Lane ret = new Lane();
        ret.setLaneId(0);
        return ret;
    }
}
