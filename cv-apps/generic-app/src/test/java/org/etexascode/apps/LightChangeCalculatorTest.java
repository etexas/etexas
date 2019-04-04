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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.etexascode.apps.LightChangeCalculator.Color;
import org.etexascode.apps.LightChangeCalculator.LightState;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author ablatt
 */
public class LightChangeCalculatorTest {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(LightChangeCalculatorTest.class);

    @Test
    public void testConstructor() {
        LightChangeCalculator calc = new LightChangeCalculator(genLaneIds());
        assertNull(calc.lightsByLane.get(0));
    }

    @Test
    public void testUpdate() {
        List<ISignalManager> sigManInfs = genUpdateMans();
        List<String> upRes = genResUpdates();
        Set<Integer> laneIDs = genLaneIds();
        laneIDs.add(9001);
        LightChangeCalculator calc = new LightChangeCalculator(laneIDs);

        assertEquals(sigManInfs.size(), upRes.size());

        for (int i = 0; i < sigManInfs.size(); i++) {
            assertTrue("failed on index " + i, checkUpdateMap(calc.update(sigManInfs.get(i), (double)i), upRes.get(i), 0));
        }
    }

    @Test
    public void testGetTimeToChangeByLane() {
        Map<Integer, LightState> lightsByLane = new HashMap<Integer, LightState>();

        LightState l = new LightState();
        l.color = Color.YELLOW;
        l.timeToChange = 5.5;
        lightsByLane.put(0, l);

        LightState l2 = new LightState();
        lightsByLane.put(1, l2);

        lightsByLane.put(1001, null);

        LightChangeCalculator calc = new LightChangeCalculator(lightsByLane.keySet());
        calc.lightsByLane = lightsByLane;

        Map<Integer, Double> actualResult = calc.getTimeToChangeByLane();
        Map<Integer, Double> expectedResult = new HashMap<Integer, Double>();
        expectedResult.put(0, l.timeToChange);
        expectedResult.put(1, -1.0);

        for (Integer key : expectedResult.keySet()) {
            assertEquals("Failed on index " + key, expectedResult.get(key), actualResult.get(key));
        }
        assertFalse(actualResult.get(0) == l.timeToChange);
    }

    @Test
    public void testGetLanesBySignalColor() {
        Map<Integer, LightState> lightsByLane = new HashMap<Integer, LightState>();

        LightState l1 = new LightState();
        l1.color = Color.GREEN;

        LightState l2 = new LightState();
        l2.color = Color.GREEN;

        LightState l3 = new LightState();
        l3.color = Color.YELLOW;

        LightState l4 = new LightState();
        l4.color = Color.RED;

        lightsByLane.put(1, l1);
        lightsByLane.put(2, l2);
        lightsByLane.put(3, l3);
        lightsByLane.put(4, l4);
        lightsByLane.put(1001, null);

        LightChangeCalculator calc = new LightChangeCalculator(lightsByLane.keySet());
        calc.lightsByLane = lightsByLane;

        Set<Integer> expectedGreen = new HashSet<Integer>(Arrays.asList(1, 2));
        Set<Integer> expectedYellow = new HashSet<Integer>(Arrays.asList(3));
        Set<Integer> expectedRed = new HashSet<Integer>(Arrays.asList(4));

        Map<Color, Set<Integer>> expected = new HashMap<Color, Set<Integer>>();
        expected.put(Color.GREEN, expectedGreen);
        expected.put(Color.YELLOW, expectedYellow);
        expected.put(Color.RED, expectedRed);

        Map<Color, Set<Integer>> actual = calc.getLanesBySignalColor();

        assertEquals(expected.keySet().size(), actual.keySet().size());
        for (Entry<Color, Set<Integer>> entry : expected.entrySet()) {
            assertTrue(entry.getValue().containsAll(actual.get(entry.getKey())));
            assertTrue(actual.get(entry.getKey()).containsAll(entry.getValue()));
        }
    }

    @Test
    public void testGetSignalManagerInfo() {
        Map<Integer, LightState> lightsByLane = genLightsByLaneForSignalManager();
        lightsByLane.put(9001, null);

        LightState ls = new LightState();
        ls.color = Color.RED;
        ls.timeToChange = 1.0;
        lightsByLane.put(1001, ls);

        LightChangeCalculator calc = new LightChangeCalculator(lightsByLane.keySet());
        calc.lightsByLane = lightsByLane;

        ISignalManager smi = calc.getSignalManager();

        ISignalManager expectedSMI = genExpectedSignalManager();

        assertEquals(expectedSMI, smi);
    }

    @Test
    public void testFoldLightState() {
        LightChangeCalculator calc = new LightChangeCalculator(genLaneIds());
        LightState ls = calc.foldLightState(genSignalIndications());

        assertEquals(LightChangeCalculator.Color.GREEN, ls.getColor());
        assertNotNull(ls.getTimeToChange());

        if (ls.getTimeToChange() != null) {
            assertEquals(80, ls.getTimeToChange(), 0.05);
        }
    }

    @Test
    public void testFoldLightState2() {
        LightChangeCalculator calc = new LightChangeCalculator(genLaneIds());
        List<SignalIndication> signalIndications = new ArrayList<SignalIndication>();

        LightState ls = calc.foldLightState(signalIndications);
        assertNull(ls);

        SignalIndication si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.RED);
        si.setTimeToChange(20);
        signalIndications.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.RED);
        si.setTimeToChange(20);
        signalIndications.add(si);

        ls = calc.foldLightState(signalIndications);

        assertEquals(LightChangeCalculator.Color.RED, ls.getColor());

        if (ls.getTimeToChange() != null) {
            assertEquals(20, ls.getTimeToChange(), 0.0000001);
        }
    }

    private static boolean checkUpdateMap(Map<String, Set<Integer>> updateOutput, String stateToCheck, int laneId) {
        for (String s : updateOutput.keySet()) {
            Set<Integer> tmp = updateOutput.get(s);
            if (!s.equals(stateToCheck)) {
                if (!tmp.isEmpty()) {
                    StringBuilder tstr = new StringBuilder();
                    UtilsStringOnModel.addSet(tstr, tmp, "failed list");
                    LOGGER.error("check update map failure code 0: s = " + s + "  state to check = " + stateToCheck + "  lane id = " + laneId + " \ntmp = " + tstr.toString());
                    return false;
                }
            }
            else {
                if (tmp.size() != 1) {
                    StringBuilder tstr = new StringBuilder();
                    UtilsStringOnModel.addSet(tstr, tmp, "failed list");
                    LOGGER.error("check update map failure code 1: s = " + s + "  state to check = " + stateToCheck + "  lane id = " + laneId + " \ntmp = " + tstr.toString());
                    return false;
                }
                else if (((Integer)tmp.toArray()[0]) != laneId) {
                    StringBuilder tstr = new StringBuilder();
                    UtilsStringOnModel.addSet(tstr, tmp, "failed list");
                    LOGGER.error("check update map failure code 2: s = " + s + "  state to check = " + stateToCheck + "  lane id = " + laneId + " \ntmp = " + tstr.toString());
                    return false;
                }
            }
        }

        return true;
    }

    private static Map<Integer, LightState> genLightsByLaneForSignalManager() {
        Map<Integer, LightState> lightsByLane = new HashMap<Integer, LightState>();

        LightState l1 = new LightState();
        l1.color = Color.GREEN;

        LightState l2 = new LightState();
        l2.color = Color.GREEN;

        LightState l3 = new LightState();
        l3.color = Color.YELLOW;

        LightState l4 = new LightState();
        l4.color = Color.RED;

        lightsByLane.put(1, l1);
        lightsByLane.put(2, l2);
        lightsByLane.put(3, l3);
        lightsByLane.put(4, l4);

        return lightsByLane;
    }

    private static SignalManager genExpectedSignalManager() {
        SignalIndication si1 = genSig(1, SignalIndication.Color.GREEN, -1);
        SignalIndication si2 = genSig(2, SignalIndication.Color.GREEN, -1);
        SignalIndication si3 = genSig(3, SignalIndication.Color.YELLOW, -1);
        SignalIndication si4 = genSig(4, SignalIndication.Color.RED, -1);
        SignalIndication si5 = genSig(1001, SignalIndication.Color.RED, 1);

        SignalManager expectedSM = new SignalManager();
        expectedSM.addSignal(si1);
        expectedSM.addSignal(si2);
        expectedSM.addSignal(si3);
        expectedSM.addSignal(si4);
        expectedSM.addSignal(si5);

        for (SignalIndication si : expectedSM.getIterable()) {
            si.setStateIndication(SignalIndication.State.STEADY);
            si.setTypeIndication(SignalIndication.Type.BALL);
        }

        return expectedSM;
    }

    private static List<String> genResUpdates() {
        List<String> ret = new LinkedList<String>();

        ret.add("");
        ret.add("GREEN-GREEN");
        ret.add("GREEN-YELLOW");
        ret.add("YELLOW-YELLOW");
        ret.add("YELLOW-RED");
        ret.add("RED-RED");
        ret.add("RED-GREEN");
        ret.add("GREEN-YELLOW");
        ret.add("YELLOW-YELLOW");
        ret.add("YELLOW-RED");
        ret.add("RED-YELLOW");
        ret.add("YELLOW-GREEN");
        ret.add("GREEN-RED");

        return ret;
    }

    private static List<ISignalManager> genUpdateMans() {
        List<ISignalManager> ret = new LinkedList<ISignalManager>();

        ret.add(genSigMan(0, SignalIndication.Color.GREEN, 2.0));
        ret.add(genSigMan(0, SignalIndication.Color.GREEN, 1.0));
        ret.add(genSigMan(0, SignalIndication.Color.YELLOW, 2.0));
        ret.add(genSigMan(0, SignalIndication.Color.YELLOW, 1.0));
        ret.add(genSigMan(0, SignalIndication.Color.RED, 2.0));
        ret.add(genSigMan(0, SignalIndication.Color.RED, 1.0));
        ret.add(genSigMan(0, SignalIndication.Color.GREEN, 1.0));
        ret.add(new SignalManager());
        ret.add(new SignalManager());
        ret.add(genSigMan(0, SignalIndication.Color.RED, 5.0));
        ret.add(genSigMan(0, SignalIndication.Color.YELLOW, 5.0));
        ret.add(genSigMan(0, SignalIndication.Color.GREEN, 5.0));
        ret.add(genSigMan(0, SignalIndication.Color.RED, 5.0));

        return ret;
    }

    private static ISignalManager genSigMan(int laneId, SignalIndication.Color color, double ttc) {
        SignalManager sigMan = new SignalManager();
        sigMan.addSignal(genSig(laneId, color, ttc));
        return sigMan;
    }

    private static SignalIndication genSig(int laneId, SignalIndication.Color color, double ttc) {
        SignalIndication ret = new SignalIndication();

        ret.setLaneId(laneId);
        ret.setColorIndication(color);
        ret.setTimeToChange(ttc);

        return ret;
    }

    private static Set<Integer> genLaneIds() {
        HashSet<Integer> ret = new HashSet<Integer>();
        ret.add(0);
        return ret;
    }

    private static List<ISignalIndication> genSignalIndications() {
        List<ISignalIndication> ret = new LinkedList<ISignalIndication>();

        SignalIndication si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.RED);
        si.setTimeToChange(20);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setTimeToChange(5);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.RED);
        si.setTimeToChange(10);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setTimeToChange(40);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setTimeToChange(80);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setTimeToChange(7);
        ret.add(si);

        return ret;
    }

    @Test
    public void testLightState() {
        LightState ls = new LightState();

        assertEquals(Color.RED, ls.getColor());
    }

    @Test
    public void testLightStateClone() {
        LightState ls = new LightState();
        LightState lsc = ls.clone();

        assertEquals(Color.RED, lsc.getColor());
    }

    @Test
    public void testLightStateAdvance() {
        LightState ls = new LightState();

        ls.advance();
        LightState lsc1 = ls.clone();
        ls.advance();
        LightState lsc2 = ls.clone();
        ls.advance();

        assertEquals(Color.GREEN, lsc1.getColor());
        assertEquals(Color.YELLOW, lsc2.getColor());
        assertEquals(Color.RED, ls.getColor());
    }

    @Test
    public void testCompareColor() {
        LightState ls = new LightState();

        ls.advance();
        LightState green = ls.clone();
        ls.advance();
        LightState yellow = ls.clone();
        ls.advance();
        LightState red = ls.clone();

        // 2 parameters x 3 possible values each = 3^2 possible inputs. Just try all of them.
        assertEquals(0, red.compareColor(Color.RED));
        assertEquals(-1, red.compareColor(Color.GREEN));
        assertEquals(-1, red.compareColor(Color.YELLOW));

        assertEquals(1, green.compareColor(Color.RED));
        assertEquals(0, green.compareColor(Color.GREEN));
        assertEquals(1, green.compareColor(Color.YELLOW));

        assertEquals(1, yellow.compareColor(Color.RED));
        assertEquals(-1, yellow.compareColor(Color.GREEN));
        assertEquals(0, yellow.compareColor(Color.YELLOW));
    }
}
