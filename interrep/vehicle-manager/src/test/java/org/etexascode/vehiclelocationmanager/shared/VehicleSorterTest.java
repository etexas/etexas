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
package org.etexascode.vehiclelocationmanager.shared;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class VehicleSorterTest {

    Set<String> currVehs = null;

    Set<String> prevVehs = null;

    Set<String> injected = null;

    IVehicleManager currMan = null;

    IVehicleManager prevMan = null;

    VehicleSorter vs = null;

    List<IVehicle> logoutExpected = null;

    List<IVehicle> loginExpected = null;

    List<IVehicle> sortingVehList = null;

    Map<DualIntIdentifier, List<IVehicle>> sortingExpected = null;

    IVehicle pairingIVehicle = null;

    int pairingIntersectionId = 1729;

    int pairingVehId = 42;

    DualIntIdentifier pairingKey1 = null;

    DualIntIdentifier pairingValue1 = null;

    DualIntIdentifier pairingKey2 = null;

    DualIntIdentifier pairingValue2 = null;

    DualIntIdentifier pairingKey3 = null;

    DualIntIdentifier pairingValue3 = null;

    Map<DualIntIdentifier, DualIntIdentifier> pairingMap1 = null;

    Map<DualIntIdentifier, DualIntIdentifier> pairingMap2 = null;

    Map<DualIntIdentifier, DualIntIdentifier> pairingMap3 = null;

    Map<DualIntIdentifier, DualIntIdentifier> logoutVehiclePairingMap = null;

    int logoutIntersectionId = 1;

    @Before
    public void setup() {
        currVehs = getCurrVehs();
        prevVehs = getPrevVehs();
        injected = getInjected();
        currMan = getMan(currVehs);
        prevMan = getMan(prevVehs);
        vs = new VehicleSorter();

        logoutExpected = new ArrayList<IVehicle>(2);
        logoutExpected.add(prevMan.getVehicle("Vehicle:5"));
        logoutExpected.add(prevMan.getVehicle("Vehicle:6"));

        loginExpected = new ArrayList<IVehicle>(1);
        loginExpected.add(currMan.getVehicle("Vehicle:4"));

        pairingIVehicle = GenVehicleFunctions.genVehicle(0, 0, 0, 0, pairingVehId, 0);
        pairingIntersectionId = 1729;
        pairingKey1 = new DualIntIdentifier(pairingIntersectionId, pairingVehId, false);
        pairingValue1 = new DualIntIdentifier(pairingIntersectionId, 1, false);
        pairingKey2 = new DualIntIdentifier(pairingIntersectionId, 24, false);
        pairingValue2 = new DualIntIdentifier(32, 13, false);
        pairingKey3 = new DualIntIdentifier(32, 13, false);
        pairingValue3 = new DualIntIdentifier(12, 15, false);
        pairingMap1 = getPairingMap1();
        pairingMap2 = getPairingMap2();
        pairingMap3 = getPairingMap3();

        sortingVehList = new ArrayList<IVehicle>(2);
        sortingVehList.add(pairingIVehicle);
        sortingVehList.add(pairingIVehicle);

        sortingExpected = new HashMap<DualIntIdentifier, List<IVehicle>>();
        sortingExpected.put(pairingValue1, sortingVehList);

        logoutVehiclePairingMap = getLogoutPairingMap();
        logoutIntersectionId = 1;
    }

    @After
    public void teardown() {
        currVehs = null;
        prevVehs = null;
        injected = null;
        currMan = null;
        prevMan = null;
        vs = null;
        logoutExpected = null;
        loginExpected = null;

        pairingIVehicle = null;
        pairingIntersectionId = 1729;
        pairingKey1 = null;
        pairingValue1 = null;
        pairingKey2 = null;
        pairingValue2 = null;
        pairingKey3 = null;
        pairingValue3 = null;
        pairingMap1 = null;
        pairingMap2 = null;
        pairingMap3 = null;
        sortingVehList = null;
        sortingExpected = null;

        logoutVehiclePairingMap = null;
        logoutIntersectionId = 1;
    }

    @Test
    public void testConstructor() {
        VehicleSorter sorter = new VehicleSorter();
        assertTrue(sorter instanceof VehicleSorter);
    }

    @Test
    public void testGetSortedVehicles() {
        SortedVehicles sv = vs.sortVehicles(prevMan, currMan, injected, logoutIntersectionId, logoutVehiclePairingMap);
        assertTrue(orderUnimportant(loginExpected, sv.newVehicles));
        assertTrue(sv.logoutVehicles.containsKey(new DualIntIdentifier(0, 0, true)));
        assertFalse(sv.logoutVehicles.containsKey(new DualIntIdentifier(0, 0, false)));
        assertTrue(orderUnimportant(logoutExpected, sv.logoutVehicles.get(new DualIntIdentifier(0, 0, true))));
    }

    @Test
    public void testGetLogoutVehicles() {
        Map<DualIntIdentifier, List<IVehicle>> res = vs.getLogoutVehicles(prevVehs, currVehs, prevMan, logoutIntersectionId, logoutVehiclePairingMap);
        // assertTrue(orderUnimportant(logoutExpected, res));
        assertTrue(res.containsKey(new DualIntIdentifier(0, 0, true)));
        assertFalse(res.containsKey(new DualIntIdentifier(0, 0, false)));
        assertTrue(orderUnimportant(logoutExpected, res.get(new DualIntIdentifier(0, 0, true))));
    }

    @Test
    public void testGetLoginVehicles() {
        List<IVehicle> res = vs.getLoginVehicles(prevVehs, currVehs, injected, currMan);
        assertTrue(orderUnimportant(loginExpected, res));
    }

    @Test
    public void testGetPairing1() {
        DualIntIdentifier res = vs.getPairing(pairingIntersectionId, pairingIVehicle, pairingMap1);
        assertEquals(pairingValue1, res);
    }

    @Test
    public void testGetPairing2() {
        DualIntIdentifier res = vs.getPairing(pairingIntersectionId, pairingIVehicle, pairingMap2);
        assertEquals(new DualIntIdentifier(7, 9, true), res);
    }

    @Test
    public void testGetPairing3() {
        DualIntIdentifier res = vs.getPairing(pairingIntersectionId, pairingIVehicle, pairingMap3);
        assertEquals(new DualIntIdentifier(7, 9, true), res);
    }

    private Map<DualIntIdentifier, DualIntIdentifier> getPairingMap1() {
        Map<DualIntIdentifier, DualIntIdentifier> ret = new HashMap<DualIntIdentifier, DualIntIdentifier>();

        ret.put(pairingKey1, pairingValue1);

        return ret;
    }

    private Map<DualIntIdentifier, DualIntIdentifier> getPairingMap2() {
        Map<DualIntIdentifier, DualIntIdentifier> ret = new HashMap<DualIntIdentifier, DualIntIdentifier>();

        ret.put(pairingKey2, pairingValue2);

        return ret;
    }

    private Map<DualIntIdentifier, DualIntIdentifier> getPairingMap3() {
        Map<DualIntIdentifier, DualIntIdentifier> ret = new HashMap<DualIntIdentifier, DualIntIdentifier>();

        ret.put(pairingKey3, pairingValue3);

        return ret;
    }

    boolean orderUnimportant(List<IVehicle> l1, List<IVehicle> l2) {
        if (l1.size() != l2.size()) {
            return false;
        }

        for (IVehicle vi : l1) {
            boolean b = false;
            for (IVehicle vi2 : l2) {
                if (vi.getVehicleID() == vi2.getVehicleID()) {
                    b = true;
                    break;
                }
            }
            if (!b) {
                return false;
            }
        }

        for (IVehicle vi : l2) {
            boolean b = false;
            for (IVehicle vi2 : l1) {
                if (vi.getVehicleID() == vi2.getVehicleID()) {
                    b = true;
                    break;
                }
            }
            if (!b) {
                return false;
            }
        }

        return true;
    }

    private Set<String> getCurrVehs() {
        Set<String> ret = new HashSet<String>();

        ret.add("Vehicle:1");
        ret.add("Vehicle:2");
        ret.add("Vehicle:3");
        ret.add("Vehicle:4");

        return ret;
    }

    private Set<String> getPrevVehs() {
        Set<String> ret = new HashSet<String>();

        ret.add("Vehicle:1");
        ret.add("Vehicle:6");
        ret.add("Vehicle:5");

        return ret;
    }

    private Set<String> getInjected() {
        Set<String> ret = new HashSet<String>();

        ret.add("Vehicle:2");
        ret.add("Vehicle:3");

        return ret;
    }

    private IVehicleManager getMan(Set<String> ids) {
        VehicleManager vehMan = new VehicleManager();
        for (String i : ids) {
            vehMan.addVehicle(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, Integer.valueOf(i.substring(i.length() - 1))));
        }
        return vehMan;
    }

    private HashMap<DualIntIdentifier, DualIntIdentifier> getLogoutPairingMap() {
        HashMap<DualIntIdentifier, DualIntIdentifier> ret = new HashMap<DualIntIdentifier, DualIntIdentifier>();

        return ret;
    }
}
