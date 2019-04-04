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
package org.etexascode.apps.hybrid.defaultimplementation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.hybrid.interfaces.ICarFollowingCalculator;
import org.etexascode.apps.hybrid.interfaces.ILaneChangeCalculator;
import org.etexascode.apps.hybrid.interfaces.ILeftTurnCalculator;
import org.etexascode.apps.hybrid.interfaces.IRightTurnCalculator;
import org.etexascode.apps.hybrid.interfaces.IVehiclesByLane;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SimpleCarMovementCalculatorTest {

    SimpleCarMovementCalculator scmc = null;

    TestVehByLane tvbl = null;

    TestLaneChangeCalc tlcc = null;

    TestLeftTurnCalc tltc = null;

    TestRightTurnCalc trtc = null;

    TestCarFollowingCalc tcfc = null;

    SimpleCarMovementCalculator scmc2 = null;

    List<Vehicle> orig = null;

    @Before
    public void setup() {
        scmc = new SimpleCarMovementCalculator();
        tvbl = new TestVehByLane();
        tlcc = new TestLaneChangeCalc();
        tltc = new TestLeftTurnCalc();
        trtc = new TestRightTurnCalc();
        tcfc = new TestCarFollowingCalc();
        scmc2 = new SimpleCarMovementCalculator();

        scmc2.setCarFollowingCalculator(tcfc);
        scmc2.setLaneChangeCalculator(tlcc);
        scmc2.setLeftTurnCalculator(tltc);
        scmc2.setRightTurnCalculator(trtc);
        scmc2.setVehiclesByLaneCalculator(tvbl);
        orig = new ArrayList<Vehicle>(0);
    }

    @After
    public void teardown() {
        scmc = null;
        tvbl = null;
        tlcc = null;
        tltc = null;
        trtc = null;
        tcfc = null;
        scmc2 = null;
        orig = null;
    }

    @Test
    public void testGetCarMovements1() {
        List<Vehicle> reted = scmc2.getCarMovements(orig, null);
        assertFalse(orig == reted);
        assertTrue(tvbl.cameIn == orig);
        assertTrue(tvbl.wentOut == tlcc.cameIn);
        assertTrue(tlcc.wentOut == tltc.cameIn);
        assertTrue(tltc.wentOut == trtc.cameIn);
        assertTrue(trtc.wentOut == tcfc.cameIn);
    }

    @Test
    public void testGetCarMovements2() {
        TestCarFollowingCalc2 tcfc2 = new TestCarFollowingCalc2();
        scmc2.setCarFollowingCalculator(tcfc2);
        List<Vehicle> reted = scmc2.getCarMovements(orig, null);
        List<Vehicle> all = genAllVehs();

        assertEquals(all.size(), reted.size());

        for (Vehicle v : all) {
            assertTrue(reted.contains(v));
        }
    }

    @Test
    public void testGetVehiclesByLaneCalculator() {
        assertTrue(scmc.getVehiclesByLaneCalculator() instanceof SimpleVehiclesByLane);
    }

    @Test
    public void testSetVehiclesByLaneCalculator() {
        scmc.setVehiclesByLaneCalculator(tvbl);
        assertTrue(scmc.getVehiclesByLaneCalculator() instanceof TestVehByLane);
    }

    @Test
    public void testGetLaneChangeCalculator() {
        assertTrue(scmc.getLaneChangeCalculator() instanceof SimpleLaneChangeCalculator);
    }

    @Test
    public void testSetLaneChangeCalculator() {
        scmc.setLaneChangeCalculator(tlcc);
        assertTrue(scmc.getLaneChangeCalculator() instanceof TestLaneChangeCalc);
    }

    @Test
    public void testGetLeftTurnCalculator() {
        assertTrue(scmc.getLeftTurnCalculator() instanceof SimpleLeftTurnCalculator);
    }

    @Test
    public void testSetLeftTurnCalculator() {
        scmc.setLeftTurnCalculator(tltc);
        assertTrue(scmc.getLeftTurnCalculator() instanceof TestLeftTurnCalc);
    }

    @Test
    public void testGetRightTurnCalculator() {
        assertTrue(scmc.getRightTurnCalculator() instanceof SimpleRightTurnCalculator);
    }

    @Test
    public void testSetRightTurnCalculator() {
        scmc.setRightTurnCalculator(trtc);
        assertTrue(scmc.getRightTurnCalculator() instanceof TestRightTurnCalc);
    }

    @Test
    public void testGetCarFollowingCalculator() {
        assertTrue(scmc.getCarFollowingCalculator() instanceof SuperSimpleCarFollowingCalculator);
    }

    @Test
    public void testSetCarFollowingCalculator() {
        scmc.setCarFollowingCalculator(tcfc);
        assertTrue(scmc.getCarFollowingCalculator() instanceof TestCarFollowingCalc);
    }

    private class TestVehByLane implements IVehiclesByLane {

        List<Vehicle> cameIn = null;

        Map<Integer, List<Vehicle>> wentOut = null;

        @Override
        public Map<Integer, List<Vehicle>> filterVehiclesByLane(List<Vehicle> vehs) {
            cameIn = vehs;
            wentOut = new HashMap<Integer, List<Vehicle>>();
            return wentOut;
        }
    }

    private class TestLaneChangeCalc implements ILaneChangeCalculator {

        Map<Integer, List<Vehicle>> cameIn = null;

        Map<Integer, List<Vehicle>> wentOut = null;

        @Override
        public Map<Integer, List<Vehicle>> getLaneChanges(Map<Integer, List<Vehicle>> vehiclesToTransform, InterRepInfoModel model) {
            cameIn = vehiclesToTransform;
            wentOut = new HashMap<Integer, List<Vehicle>>();
            return wentOut;
        }
    }

    private class TestLeftTurnCalc implements ILeftTurnCalculator {

        Map<Integer, List<Vehicle>> cameIn = null;

        Map<Integer, List<Vehicle>> wentOut = null;

        @Override
        public Map<Integer, List<Vehicle>> getLeftTurns(Map<Integer, List<Vehicle>> vehiclesByLane, InterRepInfoModel interRep) {
            cameIn = vehiclesByLane;
            wentOut = new HashMap<Integer, List<Vehicle>>();
            return wentOut;
        }
    }

    private class TestRightTurnCalc implements IRightTurnCalculator {

        Map<Integer, List<Vehicle>> cameIn = null;

        Map<Integer, List<Vehicle>> wentOut = null;

        @Override
        public Map<Integer, List<Vehicle>> getRightTurns(Map<Integer, List<Vehicle>> vehiclesByLane, InterRepInfoModel interRep) {
            cameIn = vehiclesByLane;
            wentOut = new HashMap<Integer, List<Vehicle>>();
            return wentOut;
        }
    }

    private class TestCarFollowingCalc implements ICarFollowingCalculator {

        Map<Integer, List<Vehicle>> cameIn = null;

        Map<Integer, List<Vehicle>> wentOut = null;

        @Override
        public Map<Integer, List<Vehicle>> performCarFollowing(Map<Integer, List<Vehicle>> vehiclesByLane, InterRepInfoModel interRep) {
            cameIn = vehiclesByLane;
            wentOut = new HashMap<Integer, List<Vehicle>>();
            return wentOut;
        }
    }

    private class TestCarFollowingCalc2 implements ICarFollowingCalculator {

        @Override
        public Map<Integer, List<Vehicle>> performCarFollowing(Map<Integer, List<Vehicle>> vehiclesByLane, InterRepInfoModel interRep) {
            Map<Integer, List<Vehicle>> ret = new HashMap<Integer, List<Vehicle>>();

            ret.put(1, genVehs1());
            ret.put(2, genVehs2());
            ret.put(3, genVehs3());

            return ret;
        }
    }

    private List<Vehicle> genVehs1() {
        List<Vehicle> ret = new ArrayList<Vehicle>(3);
        List<Vehicle> all = genAllVehs();

        ret.add(all.get(0));
        ret.add(all.get(1));
        ret.add(all.get(2));

        return ret;
    }

    private List<Vehicle> genVehs2() {
        List<Vehicle> ret = new ArrayList<Vehicle>(1);
        List<Vehicle> all = genAllVehs();
        ret.add(all.get(3));
        return ret;
    }

    private List<Vehicle> genVehs3() {
        List<Vehicle> ret = new ArrayList<Vehicle>(2);
        List<Vehicle> all = genAllVehs();
        ret.add(all.get(4));
        ret.add(all.get(5));
        return ret;
    }

    private List<Vehicle> genAllVehs() {
        List<Vehicle> ret = new ArrayList<Vehicle>(6);

        ret.add(GenVehicleFunctions.genVehicle(0.0, 0.0, 50.0, 50.0, 5, 1));
        ret.add(GenVehicleFunctions.genVehicle(0.0, 0.0, 50.0, 50.0, 5, 2));
        ret.add(GenVehicleFunctions.genVehicle(0.0, 0.0, 50.0, 50.0, 5, 3));
        ret.add(GenVehicleFunctions.genVehicle(0.0, 0.0, 50.0, 50.0, 5, 4));
        ret.add(GenVehicleFunctions.genVehicle(0.0, 0.0, 50.0, 50.0, 5, 5));
        ret.add(GenVehicleFunctions.genVehicle(0.0, 0.0, 50.0, 50.0, 5, 6));

        return ret;
    }
}
