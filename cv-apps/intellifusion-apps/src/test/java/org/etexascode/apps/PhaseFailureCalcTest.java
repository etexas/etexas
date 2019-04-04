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

import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.test.TestAppLogger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class PhaseFailureCalcTest {

    VehicleManager vehMan;

    Map<Integer, Double> speedLims;

    Map<String, Set<Integer>> lightChange1;

    Map<String, Set<Integer>> lightChange2;

    Map<String, Set<Integer>> lightChange3;

    Map<String, Set<Integer>> lightChange4;

    Map<String, Set<Integer>> lightChange5;

    Set<Integer> lanes;

    Map<Integer, Double> queueLens1;

    Map<Integer, Double> queueLens2;

    Map<Integer, Lane> lmLanes;

    LaneManager lmi;

    Map<Integer, Double> onGreenTimeToChange2 = null;

    @Before
    public void setUp() {
        vehMan = new VehicleManager();
        speedLims = new HashMap<Integer, Double>();
        speedLims.put(1, UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(30.0));
        lanes = new HashSet<Integer>();
        lanes.add(1);

        lightChange1 = new HashMap<String, Set<Integer>>();
        lightChange1.put("GREEN-GREEN", lanes);
        lightChange1.put("GREEN-YELLOW", new HashSet<Integer>());
        lightChange1.put("GREEN-RED", new HashSet<Integer>());
        lightChange1.put("RED-GREEN", new HashSet<Integer>());
        lightChange1.put("YELLOW-GREEN", new HashSet<Integer>());

        lightChange2 = new HashMap<String, Set<Integer>>();
        lightChange2.put("GREEN-YELLOW", lanes);
        lightChange2.put("GREEN-GREEN", new HashSet<Integer>());
        lightChange2.put("GREEN-RED", new HashSet<Integer>());
        lightChange2.put("RED-GREEN", new HashSet<Integer>());
        lightChange2.put("YELLOW-GREEN", new HashSet<Integer>());

        lightChange3 = new HashMap<String, Set<Integer>>();
        lightChange3.put("GREEN-RED", lanes);
        lightChange3.put("GREEN-GREEN", new HashSet<Integer>());
        lightChange3.put("GREEN-YELLOW", new HashSet<Integer>());
        lightChange3.put("RED-GREEN", new HashSet<Integer>());
        lightChange3.put("YELLOW-GREEN", new HashSet<Integer>());

        lightChange4 = new HashMap<String, Set<Integer>>();
        lightChange4.put("RED-GREEN", lanes);
        lightChange4.put("GREEN-GREEN", new HashSet<Integer>());
        lightChange4.put("GREEN-YELLOW", new HashSet<Integer>());
        lightChange4.put("GREEN-RED", new HashSet<Integer>());
        lightChange4.put("YELLOW-GREEN", new HashSet<Integer>());

        lightChange5 = new HashMap<String, Set<Integer>>();
        lightChange5.put("YELLOW-GREEN", lanes);
        lightChange5.put("GREEN-GREEN", new HashSet<Integer>());
        lightChange5.put("GREEN-YELLOW", new HashSet<Integer>());
        lightChange5.put("GREEN-RED", new HashSet<Integer>());
        lightChange5.put("RED-GREEN", new HashSet<Integer>());

        queueLens1 = new HashMap<Integer, Double>();
        queueLens2 = new HashMap<Integer, Double>();

        queueLens1.put(1, 300.0);
        queueLens2.put(1, 30000.0);

        lmLanes = new HashMap<Integer, Lane>();
        Lane l1 = new Lane();
        l1.setLaneId(1);
        l1.setSpeedLimitInMetersPerSecond(UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(30.0));
        lmLanes.put(l1.getLaneId(), l1);
        LaneManager lm = new LaneManager();
        lm.setLanes(lmLanes);
        lm.setGeoCalculatorType(UtilsLatLongConversion.GEODETIC2D);
        lmi = lm;

        onGreenTimeToChange2 = new HashMap<Integer, Double>();
        onGreenTimeToChange2.put(0, 0.0);
    }

    @After
    public void teardown() {
        onGreenTimeToChange2 = null;
    }

    @Test
    public void testConstructor() {
        PhaseFailureCalc pft = new PhaseFailureCalc(speedLims);
        assertEquals(pft.getNumPhaseFailure(), 0);
    }

    @Test
    public void testUpdate() {
        PhaseFailureCalc pft = new PhaseFailureCalc(speedLims);

        pft.update(queueLens1, lightChange1, new HashMap<Integer, Double>(), vehMan, 1);

        pft.update(queueLens1, lightChange4, new HashMap<Integer, Double>(), vehMan, 1);

        pft.update(queueLens1, lightChange2, new HashMap<Integer, Double>(), vehMan, 15);

        assertEquals(pft.getNumPhaseFailure(), 0);
    }

    @Test
    public void testUpdate2() {
        PhaseFailureCalc pft = new PhaseFailureCalc(lmi);
        pft.update(queueLens2, lightChange5, new HashMap<Integer, Double>(), vehMan, 30);

        pft.update(queueLens2, lightChange3, new HashMap<Integer, Double>(), vehMan, 40);

        assertEquals(pft.getNumPhaseFailure(), 1);
    }

    @Test
    public void testUpdate3() {
        PhaseFailureCalc pft = new PhaseFailureCalc(speedLims);

        pft.update(queueLens1, lightChange2, new HashMap<Integer, Double>(), vehMan, 1);

        pft.update(queueLens1, lightChange2, new HashMap<Integer, Double>(), vehMan, 1);
    }

    @Test
    public void testOnDestroy() {
        AppLogger al = new TestAppLogger();
        PhaseFailureCalc pft = new PhaseFailureCalc(speedLims);
        pft.onDestroy(al);
    }

    @Test
    public void testOnGreen1() {
        PhaseFailureCalc pft = new PhaseFailureCalc(speedLims);
        assertEquals(0, pft.onGreen(new HashMap<Integer, Double>(), 0, 0));
    }

    @Test
    public void testOnGreen2() {
        PhaseFailureCalc pft = new PhaseFailureCalc(speedLims);
        assertEquals(0, pft.onGreen(onGreenTimeToChange2, 0, 0));
    }
}
