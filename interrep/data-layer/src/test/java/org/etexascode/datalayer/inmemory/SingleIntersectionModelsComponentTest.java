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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 * @author ttevendale
 */
public class SingleIntersectionModelsComponentTest {

    SingleIntersectionModelsComponent simc = null;

    InterRepInfoModel testModel = null;

    VehicleManager vmi = null;

    Vehicle v = null;

    LaneManager lm = null;

    Map<Integer, Lane> lanes = null;

    Lane l = null;

    SignalManager sm = null;

    SignalIndication si = null;

    DetectorManager dm = null;

    Detector d = null;

    @Before
    public void setup() {
        simc = new SingleIntersectionModelsComponent();

        lm = new LaneManager();
        lanes = new HashMap<Integer, Lane>();
        l = new Lane();
        l.setLaneId(1);
        lanes.put(1, l);
        lm.setLanes(lanes);

        sm = new SignalManager();
        si = new SignalIndication();
        si.setLaneId(l.getLaneId());
        sm.addSignal(si);

        vmi = new VehicleManager();
        v = new Vehicle(1, 0.0, 0.0, 0.0, 0.0, 0.0);
        vmi.addVehicle(v);

        dm = new DetectorManager();
        d = new Detector();
        d.setDetectorID(1);
        dm.addDetector(d.getDetectorID(), d);

        testModel = new InterRepInfoModel(lm, vmi, sm, dm, new ReferencePoint[0], null, null);
    }

    @After
    public void teardown() {
        simc = null;
        lm = null;
        lanes = null;
        l = null;
        vmi = null;
        v = null;
        testModel = null;
        dm = null;
        d = null;
    }

    @Test
    public void testConstructor() {
        SingleIntersectionModelsComponent simc = new SingleIntersectionModelsComponent();
        assertTrue(simc instanceof SingleIntersectionModelsComponent);
    }

    @Test
    public void testPutInterRepInfoModel() {
        simc.putInterRepInfoModel(0, 0, testModel);
        assertTrue(testModel == simc.model);
    }

    @Test
    public void testGetVehicles1() {
        assertEquals(new VehicleManager(), simc.getVehicles(0, 0));
    }

    @Test
    public void testGetVehicles2() {
        simc.model = testModel;
        assertTrue(vmi == simc.getVehicles(1, 1));
    }

    @Test
    public void testGetVehicleInfoById() {
        simc.model = testModel;
        IVehicle testV = simc.getVehicleInfoById(0, v.getProperId());
        assertTrue(testV.equals(v));
    }

    @Test
    public void testGetLaneInfoByVehicle() {
        simc.model = testModel;
        v.setLaneID(l.getLaneId());
        ILane testL = simc.getLaneInfoByVehicle(0, v);
        assertTrue(testL.equals(l));
    }

    @Test
    public void testGetSignalsByLane() {
        simc.model = testModel;
        List<? extends ISignalIndication> testSiList = simc.getSignalsByLane(0, l);

        boolean looped = false;
        for (ISignalIndication testSi : testSiList) {
            assertTrue(testSi.equals(si));
            looped = true;
        }
        assertTrue(looped);
    }

    @Test
    public void testGetInfoModel() {
        simc.model = testModel;
        assertTrue(simc.getInfoModel(0, 0) == testModel);
    }

    @Test
    public void testGetDetectors() {
        simc.model = testModel;
        Iterable<? extends IDetector> testDIter = simc.getDetectors(0, 0);

        boolean looped = false;
        for (IDetector testD : testDIter) {
            assertTrue(testD.equals(d));
            looped = true;
        }
        assertTrue(looped);
    }

    @Test
    public void testGetDetectors2() {
        simc.model = new InterRepInfoModel(null, null, null, null, new ReferencePoint[0], null, null);
        Iterable<? extends IDetector> testDIter = simc.getDetectors(0, 0);
        emptyDetectorLoopTest(testDIter);

        simc.model = null;
        testDIter = simc.getDetectors(0, 0);
        emptyDetectorLoopTest(testDIter);

    }

    private void emptyDetectorLoopTest(Iterable<? extends IDetector> testDIter) {
        for (IDetector testD : testDIter) {
            fail("There should be no detectors when either the model or model.dmi is null");
        }
    }
}
