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
package org.etexascode.interrep;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.tests.TestInterRepCoordinatorDataLayer;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.SimMetaData;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.powermock.reflect.Whitebox;

public class InterRepTest {

    private final double TOLERANCE = 0.005;

    @Rule
    public ExpectedException ee = ExpectedException.none();

    private InterRep ir;

    StaticData sd = null;

    TestSimInt tsi = null;

    TestInterRepCoordinatorDataLayer tdcl = null;

    ExecMetaData emd = null;

    InterRepInfoModel irim = null;

    double st = 0.0;

    int sn = 0;

    @Before
    public void setUp() {
        st = 1.0;
        sn = 1;
        sd = produceStaticData();
        tsi = genTSI();
        irim = produceInterRepInfoModel();
        tdcl = genTestInterRepCoordinatorDataLayer();
        emd = produceExecMetaData();

        ir = genInterRep();

    }

    @After
    public void teardown() {
        ir = null;
        tsi = null;
        tdcl = null;
        sd = null;
        emd = null;
        irim = null;
        st = 0.0;
        sn = 0;
    }

    @Test
    public void testInterRepSimulatorInterface() {
        assertEquals(1, ir.getCurrentTimeStep());
        assertEquals(0.5, ir.getTimeStepInterval(), TOLERANCE);
    }

    @Test
    public void testInterRepSimulatorInterface2() {
        TestSimInt tsi = new TestSimInt();
        TestInterRepCoordinatorDataLayer tdcl = new TestInterRepCoordinatorDataLayer();
        tsi.setStaticData(produceStaticData());
        tsi.setThrowRemoteException(true);
        new InterRep(0, tsi, null, tdcl);
    }

    @Test
    public void testInterRepSimulatorInterfaceExecMetaData() {
        LaneManager lm = Whitebox.getInternalState(ir, "laneManager");
        DetectorManager dm = Whitebox.getInternalState(ir, "detectorManager");
        assertEquals(1.0, lm.getLatitude(), TOLERANCE);
        assertEquals(2.0, lm.getLongitude(), TOLERANCE);
        assertEquals(1, dm.getKeys().size());
        assertNotNull(dm.getDetector(4));
    }

    @Test
    public void testClose() {
        InterRep ir = new InterRep(0, tsi, null, tdcl);
        ir.close();
        tsi.setThrowRemoteException(true);
        ir.close();
    }

    @Test
    public void testUpdate1() {
        // you have to add in a vehicle manager for the update function to work correctly
        ir.vehicleManager.addVehicles(vehListTwo());
        assertNotNull(ir.update());
        assertEquals(2, ir.getCurrentTimeStep());
        assertEquals(false, ir.update());
    }

    // Tests that the update will throw the appropriate errors
    @Test
    public void testUpdate2() {
        ir = new InterRep(0, tsi, null, tdcl);
        tsi.setThrowRemoteException(true); // the simulator interface throws a remote exception
        ee.expect(RuntimeException.class); // but interrep throws a runtime exception
        ir.update();
    }

    // Tests the addition of commands and injection requests
    @Test
    public void testUpdate3() {
        ir.vehicleManager.addVehicles(vehListTwo());
        ir.update();
        assertEquals(tsi.scommand, tdcl.sigCommands.get(0));
        assertEquals(tsi.vcommand, tdcl.vehCommands.get(0));
        assertEquals(tsi.request, tdcl.requests.get(0));
    }

    // Tests the
    @Test
    public void testUpdate4() {
        ir.vehicleManager.addVehicles(vehListTwo());
        ir.update();
        assertEquals(0.0, ir.vehicleManager.getVehicle("Simulation:0-Vehicle:1").getSpeed(), 0.0005);
        assertTrue(ir.vehicleManager.getVehicle("Simulation:0-Vehicle:2").isSpeedSet());
        assertTrue(ir.vehicleManager.getVehicle("Simulation:0-Vehicle:3").isAccelerationSet());
        DetectorManager dm = Whitebox.getInternalState(ir, "detectorManager");
        assertNull(dm.getDetector(4).getDetEvent());
        assertEquals(ir.signalManager.getSignalsByLaneId(1), sigList());

    }

    @Test
    public void testGetSimTime() {
        assertEquals(0.0, ir.getSimTime(), TOLERANCE);
    }

    @Test
    public void testGetTimeStepInterval() {
        assertEquals(0.5, ir.getTimeStepInterval(), TOLERANCE);
    }

    @Test
    public void testGetCurrentTimeStep() {
        assertEquals(1.0, ir.getCurrentTimeStep(), TOLERANCE);
    }

    private InterRep genInterRep() {
        InterRep ir = new InterRep(0, tsi, emd, tdcl);
        return ir;
    }

    private TestSimInt genTSI() {
        TestSimInt tsi = new TestSimInt();
        tsi.setStaticData(produceStaticData());
        tsi.setStepData(sn, genStepData());

        return tsi;
    }

    private TestInterRepCoordinatorDataLayer genTestInterRepCoordinatorDataLayer() {
        TestInterRepCoordinatorDataLayer tdcl = new TestInterRepCoordinatorDataLayer();
        tdcl.putInterRepModel(sn, 0, irim);
        VehicleSpeedCommand vscmd1 = new VehicleSpeedCommand(1, 3, 15.0);
        SignalCommand scmd1 = new SignalCommand(1, 0.0);
        VehicleInjectionRequest vir = new VehicleInjectionRequest(v4(), 1.0, 0);
        tdcl.vehCommands.add(0, vscmd1);
        tdcl.sigCommands.add(0, scmd1);
        tdcl.requests.add(0, vir);
        return tdcl;
    }

    private StaticData produceStaticData() {
        StaticData ret = new StaticData();
        ret.setMetaData(produceSimMetaData());
        ret.setDetectorManager(genDetMan());
        ret.setLaneManager(genLaneMan());
        ret.setSignalManager(genSigMan());
        return ret;
    }

    private StepData genStepData() {
        StepData std = new StepData();
        std.setSignalList(sigList());
        std.setVehicleList(vehList());
        return std;
    }

    private InterRepInfoModel produceInterRepInfoModel() {
        InterRepInfoModel irim = new InterRepInfoModel(genLaneMan(), genVehMan(), genSigMan(), genDetMan(), genRPA(), st, 0.0);
        return irim;
    }

    private LaneManager genLaneMan() {
        LaneManager lm = new LaneManager();
        lm.setLanes(laneList());
        return lm;
    }

    private VehicleManager genVehMan() {
        VehicleManager vm = new VehicleManager();
        vm.addVehicles(vehList());
        return vm;
    }

    private SignalManager genSigMan() {
        SignalManager sm = new SignalManager();
        sm.addSignals(sigList());
        return sm;
    }

    private DetectorManager genDetMan() {
        DetectorManager dm = new DetectorManager();
        dm.addDetector(detector().getDetectorID(), detector());
        return dm;
    }

    private ReferencePoint[] genRPA() {
        ReferencePoint rp = new ReferencePoint(1.0, 2.0);
        ReferencePoint[] rpa = (new ReferencePoint[] { rp });
        return rpa;

    }

    private ExecMetaData produceExecMetaData() {
        ExecMetaData emd = new ExecMetaData();
        emd.setReferencePoints(genRPA());
        emd.setGeoCalculatorType(3);
        emd.setExecDetectors(detList());
        return emd;
    }

    private SimMetaData produceSimMetaData() {
        SimMetaData smd = new SimMetaData();
        smd.setStepSize(0.5);
        smd.setFirstStep(1);
        smd.setMaxSteps(2);
        return smd;
    }

    private Vehicle v1() {
        Vehicle v1 = new Vehicle(1, 10.0, 10.0, 35.0, 35.0, 0.0);
        v1.setLaneID(1);

        return v1;
    }

    private Vehicle v2() {
        Vehicle v2 = new Vehicle(2, 10.0, 10.0, 15.0, 15.0, 0.0);
        v2.setLaneID(1);

        return v2;
    }

    private Vehicle v3() {
        Vehicle v3 = new Vehicle(3, 10.0, 10.0, 20.0, 20.0, 0.0);
        v3.setLaneID(1);

        v3.setSpeed(5);
        return v3;
    }

    private Vehicle v4() {
        Vehicle v4 = new Vehicle(4, 10.0, 10.0, 25.0, 25.0, 0.0);
        v4.setLaneID(1);

        v4.setSpeed(15.0);
        return v4;
    }

    private List<Vehicle> vehList() {
        List<Vehicle> vlist = new ArrayList<Vehicle>(3);
        vlist.add(v1());
        vlist.add(v2());
        vlist.add(v3());
        return vlist;
    }

    private List<Vehicle> vehListTwo() {
        List<Vehicle> vl2 = new ArrayList<Vehicle>(3);
        v2().setSpeed(10.0);
        v2().setX(400.0);
        v2().setY(400.0);
        v3().setX(550.0);
        v3().setY(550.0);
        vl2.add(v1());
        vl2.add(v2());
        vl2.add(v3());
        return vl2;
    }

    private Map<Integer, Lane> laneList() {
        Map<Integer, Lane> llist = new HashMap<Integer, Lane>(1);
        Lane l1 = new Lane();
        l1.setLaneId(1);
        l1.setType("INBOUND");
        LaneNode ln = new LaneNode();
        ln.setX(0.0);
        ln.setY(0.0);
        List<LaneNode> lst = new ArrayList<LaneNode>(1);
        lst.add(ln);
        l1.setLaneGeomList(lst);
        llist.put(0, l1);
        return llist;
    }

    private SignalIndication sigind() {
        SignalIndication si = new SignalIndication();
        si.setLaneId(1);
        si.setColorIndication(Color.GREEN);
        si.setTypeIndication(Type.BALL);
        si.setStateIndication(State.STEADY);
        si.setTimeToChange(1.0);
        return si;
    }

    private List<SignalIndication> sigList() {
        List<SignalIndication> sil = new ArrayList<SignalIndication>(1);
        sil.add(sigind());
        return sil;
    }

    private Detector detector() {
        List<Integer> lanes = new ArrayList<Integer>(1);
        lanes.add(1);
        Detector d = new Detector();
        d.setDetectorID(4);
        d.setLaneIDs(lanes);
        d.setDetEvent(new DetectorEvent());
        return d;
    }

    private List<Detector> detList() {
        List<Detector> dlist = new LinkedList<Detector>();
        dlist.add(detector());
        return dlist;
    }
}
