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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.test.GenLaneFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SimpleVehiclesFromDetectorsTest {

    VehicleManager vm = null;

    List<Vehicle> vehs = null;

    SimpleVehiclesFromDetectors svfd = null;

    VehicleManager vm2 = null;

    IDetector di = null;

    VehicleManager vm3 = null;

    List<IDetector> dets = null;

    DetectorManager dm = null;

    @Before
    public void setup() {
        vm = new VehicleManager();
        vehs = genVehList();

        for (Vehicle v : vehs) {
            vm.addVehicle(v);
        }

        svfd = new SimpleVehiclesFromDetectors();

        vm2 = new VehicleManager();
        vm2.addVehicle(getVehOverDet());
        di = getDet();

        vm3 = new VehicleManager();
        vm3.addVehicle(getNotOverDet());

        dets = new ArrayList<IDetector>(1);
        dets.add(di);

        dm = new DetectorManager();
        dm.addDetector(0, getDet());
    }

    @After
    public void teardown() {
        vm = null;
        vehs = null;
        svfd = null;
        vm2 = null;
        di = null;
        vm3 = null;
        dets = null;
        dm = null;
    }

    @Test
    public void testGetVehiclesFromDetectors1() {
        List<Vehicle> actual = svfd.getVehiclesFromDetectors(vm2, vm3, dm, GenLaneFunctions.genLaneManager());
        assertEquals(1, actual.size());
    }

    @Test
    public void testGetVehiclesFromDetectors2() {
        List<Vehicle> actual = svfd.getVehiclesFromDetectors(vm3, vm2, dm, GenLaneFunctions.genLaneManager());
        assertEquals(0, actual.size());
    }

    @Test
    public void testPruneDetectorInfoList1() {
        List<IDetector> ldi = svfd.pruneDetectorList(dets, vm2, true);
        assertEquals(1, ldi.size());
        assertTrue(ldi.contains(di));
    }

    @Test
    public void testPruneDetectorInfoList2() {
        List<IDetector> ldi = svfd.pruneDetectorList(dets, vm3, true);
        assertEquals(0, ldi.size());
    }

    @Test
    public void testPruneDetectorInfoList3() {
        List<IDetector> ldi = svfd.pruneDetectorList(dets, vm2, false);
        assertEquals(0, ldi.size());
    }

    @Test
    public void testPruneDetectorInfoList4() {
        List<IDetector> ldi = svfd.pruneDetectorList(dets, vm3, false);
        assertEquals(1, ldi.size());
        assertTrue(ldi.contains(di));
    }

    @Test
    public void testVehicleOver1() {
        Vehicle v = svfd.vehicleOver(di, vm2);
        assertNotNull(v);
    }

    @Test
    public void testVehicleOver2() {
        Vehicle v = svfd.vehicleOver(di, vm3);
        assertNull(v);
    }

    private List<Vehicle> genVehList() {
        List<Vehicle> ret = new ArrayList<Vehicle>(3);

        ret.add(genVeh(5));
        ret.add(genVeh(42));
        ret.add(genVeh(1729));

        return ret;
    }

    private Vehicle genVeh(int id) {
        Vehicle ret = new Vehicle(id, 0.0, 0.0, 0.0, 0.0, 0.0);
        ret.setVehicleID(id);
        ret.setLaneID(1);
        return ret;
    }

    private Vehicle getVehOverDet() {
        Vehicle ret = new Vehicle(42, 250.0, 250.0, -15, -15, 0.5);
        ret.setVehicleID(42);
        ret.setX(-15.0);
        ret.setY(-15.0);
        ret.setLength(250.0);
        ret.setWidth(250.0);
        ret.setHeight(250.0);
        ret.setHeading(179.0);
        return ret;
    }

    private Vehicle getNotOverDet() {
        Vehicle ret = new Vehicle(42, 250.0, 250.0, 40000.0, 40000.0, 0.5);
        ret.setVehicleID(42);
        ret.setX(40000.0);
        ret.setY(40000.0);
        ret.setLength(250.0);
        ret.setWidth(250.0);
        ret.setHeight(250.0);
        ret.setHeading(179.0);
        return ret;
    }

    private Detector getDet() {
        Detector ret = new Detector();
        Polygon p = new Polygon();
        p.addPoint(1000, 1000);
        p.addPoint(-1000, 1000);
        p.addPoint(-1000, -1000);
        p.addPoint(1000, -1000);
        ret.setArea(p);
        List<Integer> lids = new ArrayList<Integer>(1);
        lids.add(1);
        ret.setLaneIDs(lids);
        ret.setDetEvent(new DetectorEvent());
        ret.getDetEvent().setPresence(true);
        return ret;
    }
}
