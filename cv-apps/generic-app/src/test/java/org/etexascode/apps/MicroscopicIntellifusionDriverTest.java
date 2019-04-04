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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.hybrid.defaultimplementation.SimpleCarMovementCalculator;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleDetermineVehicleLength;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleMapParser;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleNotDSRCVehicles;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleSpatParser;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleVehicleParser;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleVehiclesFromDetectors;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleVehiclesFromDriveway;
import org.etexascode.apps.hybrid.interfaces.ICarMovementCalculator;
import org.etexascode.apps.hybrid.interfaces.IDetermineVehicleLength;
import org.etexascode.apps.hybrid.interfaces.IMapParser;
import org.etexascode.apps.hybrid.interfaces.INotDSRCVehicles;
import org.etexascode.apps.hybrid.interfaces.ISpatParser;
import org.etexascode.apps.hybrid.interfaces.IVehicleParser;
import org.etexascode.apps.hybrid.interfaces.IVehiclesFromDetectors;
import org.etexascode.apps.hybrid.interfaces.IVehiclesFromDriveway;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MicroscopicIntellifusionDriverTest {

    MicroscopicIntellifusionDriver hid = null;

    TestDetermineLens tdl = null;

    TestVehDriveway tvd = null;

    TestVehDets tvehd = null;

    TestNotDsrc tnd = null;

    TestMapParser tmp = null;

    TestSpatParser tsp = null;

    TestVehicleParser tvp = null;

    TestCarMovementCalculator tcmc = null;

    Object[] messArr = null;

    IDetectorManager dmi = null;

    LaneManager laneManager = null;

    List<Vehicle> notDsrcVehicles = null;

    List<Vehicle> dsrcVehicles = null;

    @Before
    public void setup() {
        hid = new MicroscopicIntellifusionDriver();
        tdl = new TestDetermineLens();
        tvd = new TestVehDriveway();
        tvehd = new TestVehDets();
        tnd = new TestNotDsrc();
        tmp = new TestMapParser();
        tsp = new TestSpatParser();
        tvp = new TestVehicleParser();
        tcmc = new TestCarMovementCalculator();
        messArr = new Object[0];
        dmi = new DetectorManager();

        Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();

        Lane lane = new Lane();
        lane.setApproachId(1);
        lane.setIntersectionId(1);
        lane.setLaneId(1);
        lane.setSpeedLimitInMetersPerSecond(15.0);
        lane.setType(Lane.INBOUND);

        List<LaneNode> laneNodes = new ArrayList<LaneNode>();
        laneNodes.add(new LaneNode(-1000, 0));
        laneNodes.add(new LaneNode(0, 0));
        lane.setLaneGeomList(laneNodes);

        lanes.put(lane.getLaneId(), lane);

        lane = new Lane();
        lane.setApproachId(1);
        lane.setIntersectionId(1);
        lane.setLaneId(2);
        lane.setSpeedLimitInMetersPerSecond(15.0);
        lane.setType(Lane.OUTBOUND);

        laneNodes = new ArrayList<LaneNode>();
        laneNodes.add(new LaneNode(0, 0));
        laneNodes.add(new LaneNode(1000, 0));
        lane.setLaneGeomList(laneNodes);

        lanes.put(lane.getLaneId(), lane);
        laneManager = new LaneManager();
        laneManager.setLanes(lanes);

        notDsrcVehicles = new ArrayList<Vehicle>();
        notDsrcVehicles.add(new Vehicle(1, 500, 200, 0, 0, 0));

        dsrcVehicles = new ArrayList<Vehicle>();
        dsrcVehicles.add(new Vehicle(1, 500, 200, 0, 0, 0));
    }

    @After
    public void teardown() {
        hid = null;
        tdl = null;
        tvd = null;
        tvehd = null;
        tnd = null;
        tmp = null;
        tsp = null;
        tvp = null;
        tcmc = null;
        messArr = null;
        dmi = null;
        laneManager = null;
        notDsrcVehicles = null;
        dsrcVehicles = null;
    }

    @Test
    public void testparserModel1() {
        assertNull(hid.parseModel(messArr, null, null, 0.0, UtilsLatLongConversion.GEODETIC2D, 0.1));
    }

    @Test
    public void testparserModel2() {
        hid.setMapParse(tmp);
        hid.setVehsFromDets(tvehd);
        hid.setNotDsrcVehs(tnd);
        hid.setIcmc(tcmc);
        InterRepInfoModel irim = hid.parseModel(messArr, dmi, laneManager, 0.0, UtilsLatLongConversion.GEODETIC2D, 0.1);
        assertNotNull(irim);
        assertEquals(laneManager, irim.lmi);
        assertTrue(dmi == irim.dmi);
        assertEquals(new SignalManager(), irim.smi);

        VehicleManager vehicleManager = new VehicleManager();
        vehicleManager.addVehicles(notDsrcVehicles);
        vehicleManager.addVehicles(dsrcVehicles);
        assertEquals(vehicleManager, irim.vmi);

        irim = hid.parseModel(messArr, dmi, laneManager, 0.0, UtilsLatLongConversion.GEODETIC2D, 0.1);
        assertNotNull(irim);
        assertEquals(laneManager, irim.lmi);
        assertTrue(dmi == irim.dmi);
        assertEquals(new SignalManager(), irim.smi);
        assertEquals(vehicleManager, irim.vmi);
    }

    @Test
    public void testparserModel3() {
        hid.setMapParse(tmp);
        hid.setSpatParse(tsp);
        hid.setVehsFromDets(tvehd);
        hid.setNotDsrcVehs(tnd);
        hid.setIcmc(tcmc);
        InterRepInfoModel irim = hid.parseModel(messArr, dmi, laneManager, 0.0, UtilsLatLongConversion.GEODETIC2D, 0.2);
        assertNotNull(irim);
        assertEquals(laneManager, irim.lmi);
        assertTrue(dmi == irim.dmi);
        assertEquals(new SignalManager(), irim.smi);

        VehicleManager vehicleManager = new VehicleManager();
        vehicleManager.addVehicles(notDsrcVehicles);
        vehicleManager.addVehicles(dsrcVehicles);
        assertEquals(vehicleManager, irim.vmi);
    }

    @Test
    public void testGetVehLens() {
        assertTrue(hid.getVehLens() instanceof SimpleDetermineVehicleLength);
    }

    @Test
    public void testSetVehLens() {
        hid.setVehLens(tdl);
        assertTrue(hid.getVehLens() instanceof TestDetermineLens);
    }

    @Test
    public void testGetVehsFromDriveway() {
        assertTrue(hid.getVehsFromDriveway() instanceof SimpleVehiclesFromDriveway);
    }

    @Test
    public void testSetVehsFromDriveway() {
        hid.setVehsFromDriveway(tvd);
        assertTrue(hid.getVehsFromDriveway() instanceof TestVehDriveway);
    }

    @Test
    public void testGetVehsFromDets() {
        assertTrue(hid.getVehsFromDets() instanceof SimpleVehiclesFromDetectors);
    }

    @Test
    public void testSetVehsFromDets() {
        hid.setVehsFromDets(tvehd);
        assertTrue(hid.getVehsFromDets() instanceof TestVehDets);
    }

    @Test
    public void testGetNotDsrcVehs() {
        assertTrue(hid.getNotDsrcVehs() instanceof SimpleNotDSRCVehicles);
    }

    @Test
    public void testSetNotDsrcVehs() {
        hid.setNotDsrcVehs(tnd);
        assertTrue(hid.getNotDsrcVehs() instanceof TestNotDsrc);
    }

    @Test
    public void testGetMapParser() {
        assertTrue(hid.getMapParse() instanceof SimpleMapParser);
    }

    @Test
    public void testSetMapParser() {
        hid.setMapParse(tmp);
        assertTrue(hid.getMapParse() instanceof TestMapParser);
    }

    @Test
    public void testGetSpatParser() {
        assertTrue(hid.getSpatParse() instanceof SimpleSpatParser);
    }

    @Test
    public void testSetSpatParser() {
        hid.setSpatParse(tsp);
        assertTrue(hid.getSpatParse() instanceof TestSpatParser);
    }

    @Test
    public void testGetVehicleParser() {
        assertTrue(hid.getVehParse() instanceof SimpleVehicleParser);
    }

    @Test
    public void testSetVehicleParser() {
        hid.setVehParse(tvp);
        assertTrue(hid.getVehParse() instanceof TestVehicleParser);
    }

    @Test
    public void testGetIcmc() {
        assertTrue(hid.getIcmc() instanceof SimpleCarMovementCalculator);
    }

    @Test
    public void testSetIcmc() {
        hid.setIcmc(tcmc);
        assertTrue(hid.getIcmc() instanceof TestCarMovementCalculator);
    }

    class TestDetermineLens implements IDetermineVehicleLength {

        @Override
        public List<Vehicle> getVehicleLengths(List<Vehicle> vehs) {
            return null;
        }
    }

    class TestVehDriveway implements IVehiclesFromDriveway {

        @Override
        public List<Vehicle> getVehiclesFromDriveway(InterRepInfoModel model) {
            return null;
        }
    }

    class TestVehDets implements IVehiclesFromDetectors {

        @Override
        public List<Vehicle> getVehiclesFromDetectors(VehicleManager prevMan, VehicleManager currMan, IDetectorManager detMan, LaneManager laneMan) {
            return new ArrayList<Vehicle>(0);
        }
    }

    class TestNotDsrc implements INotDSRCVehicles {

        @Override
        public List<Vehicle> getVehiclesNotDSRC(VehicleManager prevMan, VehicleManager currMan, List<Vehicle> vehsFromDriveway, List<Vehicle> vehsFromDetectors, LaneManager lm, double step) {
            return notDsrcVehicles;
        }
    }

    class TestMapParser implements IMapParser {

        @Override
        public LaneManager parseLaneManager(Object[] data, LaneManager curr) {

            return laneManager;
        }
    }

    class TestSpatParser implements ISpatParser {

        @Override
        public SignalManager parseSignalManager(Object[] data) {
            SignalManager sm = new SignalManager();
            sm.addSignal(new SignalIndication());
            return sm;
        }
    }

    class TestVehicleParser implements IVehicleParser {

        @Override
        public VehicleManager parseVehicleManager(Object[] data, LaneManager lm, int calcType) {
            return null;
        }
    }

    class TestCarMovementCalculator implements ICarMovementCalculator {

        @Override
        public List<Vehicle> getCarMovements(List<Vehicle> nonDsrc, InterRepInfoModel model) {
            return dsrcVehicles;
        }
    }
}
