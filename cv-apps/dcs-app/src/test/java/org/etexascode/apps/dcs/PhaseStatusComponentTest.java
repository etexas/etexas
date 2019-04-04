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
package org.etexascode.apps.dcs;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.dcs.model.SignalController;
import org.etexascode.apps.dcs.model.VehicleDilemmaZoneData;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.reflect.Whitebox;

/**
 * Tests the phase status component class.
 * 
 * @author jrutherford
 */
public class PhaseStatusComponentTest {

    /** The phase status component. */
    private PhaseStatusComponent psc;

    /** The signal controller. */
    private SignalController controller;

    @Before
    public void setup() {
        List<LaneNode> node = new ArrayList<LaneNode>();
        node.add(new LaneNode(10, 10));
        Lane lane = new Lane();
        lane.setLaneGeomList(node);
        lane.setLaneId(17);

        Lane lane2 = new Lane();
        lane2.setLaneId(5);

        Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();
        lanes.put(17, lane);
        lanes.put(5, lane2);
        LaneManager laneMan = new LaneManager();
        laneMan.setLanes(lanes);
        laneMan.setElevation(100);
        laneMan.setIntersectionId(3);
        laneMan.setLatitude(24.3);
        laneMan.setLongitude(35.9);

        Vehicle veh = new Vehicle(34, 0.0, 0.0, 0.0, 0.0, 0.0);
        veh.setLaneID(17);
        veh.setSpeed(30.5);

        Vehicle veh2 = new Vehicle(90, 0.0, 0.0, 0.0, 0.0, 0.0);
        veh2.setLaneID(5);
        veh2.setSpeed(0.0);

        VehicleManager vehMan = new VehicleManager();
        vehMan.addVehicle(veh);
        vehMan.addVehicle(veh2);

        SignalIndication si1 = new SignalIndication();
        si1.setColorIndication(Color.GREEN);
        si1.setLaneId(17);
        si1.setStateIndication(State.STEADY);
        si1.setTimeToChange(30);
        si1.setTypeIndication(Type.BALL);
        List<SignalIndication> sigInds = new ArrayList<SignalIndication>();
        sigInds.add(si1);

        SignalIndication si2 = new SignalIndication();
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setLaneId(5);
        sigInds.add(si2);

        SignalManager sigMan = new SignalManager();
        sigMan.addSignals(sigInds);

        Detector det = new Detector();
        det.setDetectorID(30);
        det.setLengthDetectCap(true);
        DetectorManager detMan = new DetectorManager();
        detMan.addDetector(43, det);

        controller = PowerMockito.mock(SignalController.class);
        PowerMockito.when(controller.isHoldingGreen(new int[] { 17 })).thenReturn(false);
        psc = new PhaseStatusComponent(controller, laneMan, sigMan, vehMan, detMan, new int[] { 17 });
        psc.setStage1Timeout(10);
        psc.setStage2Timeout(5);
    }

    @Test
    public void testConstructor() {
        LaneManager lm = Whitebox.getInternalState(psc, "laneManager");
        VehicleManager vm = Whitebox.getInternalState(psc, "vehicleManager");
        SignalManager sm = Whitebox.getInternalState(psc, "signalManager");
        DetectorManager dm = Whitebox.getInternalState(psc, "detectorManager");

        assertTrue(lm.getLaneIds().size() == 2);
        assertTrue(vm.getAllVehicleIds().size() == 2);
        assertTrue(sm.getSignalsByLaneId(17).size() == 1);
        assertTrue(dm.getDetectorCollection().size() == 1);
    }

    @Test
    public void testUpdateManagers() {
        Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();
        lanes.put(1, new Lane());
        lanes.put(2, new Lane());
        LaneManager laneMan = new LaneManager();
        laneMan.setLanes(lanes);
        laneMan.setElevation(100);
        laneMan.setIntersectionId(3);
        laneMan.setLatitude(24.3);
        laneMan.setLongitude(35.9);

        Vehicle veh1 = new Vehicle(34, 0.0, 0.0, 0.0, 0.0, 0.0);
        veh1.setLaneID(17);
        Vehicle veh2 = new Vehicle(35, 0.0, 0.0, 0.0, 0.0, 0.0);
        veh2.setLaneID(20);
        VehicleManager vehMan = new VehicleManager();
        vehMan.addVehicle(veh1);
        vehMan.addVehicle(veh2);

        List<SignalIndication> sigInds = new ArrayList<SignalIndication>();
        sigInds.add(new SignalIndication());
        SignalManager sigMan = new SignalManager();
        sigMan.addSignals(sigInds);

        DetectorManager detMan = new DetectorManager();
        detMan.addDetector(43, new Detector());
        detMan.addDetector(34, new Detector());

        InterRepInfoModel irim = new InterRepInfoModel(laneMan, vehMan, sigMan, detMan, new ReferencePoint[] {}, 30.5, 0.5);
        psc.updateManagers(irim);

        assertTrue(irim.getLmi().getLaneIds().size() == 2);
        assertTrue(irim.getVmi().getAllVehicleIds().size() == 2);
        assertTrue(irim.getSmi().keysForSigMap().size() == 1);
        assertTrue(irim.getDmi().getDetectorCollection().size() == 2);
    }

    @Test
    public void testSetStage1Timeout() {
        psc.setStage1Timeout(100);
        assertTrue((Double)Whitebox.getInternalState(psc, "stage1Timeout") == 100);
    }

    @Test
    public void testSetStage2Timeout() {
        psc.setStage2Timeout(50);
        assertTrue((Double)Whitebox.getInternalState(psc, "stage2Timeout") == 50);
    }

    @Test
    public void testGetTimeInterval() {
        assertTrue(psc.getTimeInterval() == 0.5);
    }

    @Test
    public void testPerformControl1() {
        VehicleDilemmaZoneData dat = new VehicleDilemmaZoneData();
        dat.setLaneId(3);
        dat.setTimeOfArrivalToDilemmaZone(30.4);
        dat.setVehicleLength(100);
        dat.setVehicleSpeed(20.4);
        Collection<VehicleDilemmaZoneData> vdzm = new ArrayList<VehicleDilemmaZoneData>();
        vdzm.add(dat);

        Whitebox.setInternalState(psc, "stage", -1);
        psc.performControl(30.5, vdzm);
        assertTrue((Integer)Whitebox.getInternalState(psc, "stage") == 0);
        Whitebox.setInternalState(psc, "stage", 3);
        psc.performControl(30.5, vdzm);
        assertTrue((Integer)Whitebox.getInternalState(psc, "stage") == 0);
    }

    @Test
    public void testPerformControl2() {
        PowerMockito.when(controller.isHoldingGreen(new int[] { 17 })).thenReturn(true);
        VehicleDilemmaZoneData dat = new VehicleDilemmaZoneData();
        dat.setLaneId(3);
        dat.setTimeOfArrivalToDilemmaZone(30.4);
        dat.setVehicleLength(100);
        dat.setVehicleSpeed(20.4);
        Collection<VehicleDilemmaZoneData> vdzm = new ArrayList<VehicleDilemmaZoneData>();
        vdzm.add(dat);

        Whitebox.setInternalState(psc, "stage", -1);
        SignalManager sigMan = Whitebox.getInternalState(psc, "signalManager");
        sigMan.getSignalsByLaneId(17).get(0).setTimeToChange(1.0);
        psc.performControl(30.5, vdzm);
        assertTrue((Integer)Whitebox.getInternalState(psc, "stage") == 1);
        assertTrue((Double)Whitebox.getInternalState(psc, "greenHoldStartTime") == 30.5);

        Whitebox.setInternalState(psc, "greenHoldStartTime", 20);
        psc.performControl(30.5, vdzm);
        assertTrue((Integer)Whitebox.getInternalState(psc, "stage") == 2);

        Whitebox.setInternalState(psc, "greenHoldStartTime", 15);
        psc.performControl(30.5, vdzm);
        assertTrue((Integer)Whitebox.getInternalState(psc, "stage") == 3);
        Mockito.verify(controller, Mockito.times(1)).changePhase();
    }

    @Test
    public void testPerformControl3() {
        PowerMockito.when(controller.isHoldingGreen(new int[] { 17 })).thenReturn(true);
        VehicleDilemmaZoneData dat = new VehicleDilemmaZoneData();
        dat.setLaneId(3);
        dat.setTimeOfArrivalToDilemmaZone(30.4);
        dat.setVehicleLength(100);
        dat.setVehicleSpeed(20.4);
        Collection<VehicleDilemmaZoneData> vdzm = new ArrayList<VehicleDilemmaZoneData>();
        vdzm.add(dat);

        Whitebox.setInternalState(psc, "stage", -1);
        psc.performControl(30.5, vdzm);
        Mockito.verify(controller, Mockito.times(0)).changePhase();
        Mockito.verify(controller, Mockito.times(0)).holdPhase();

        SignalManager sigMan = Whitebox.getInternalState(psc, "signalManager");
        sigMan.getSignalsByLaneId(17).get(0).setTimeToChange(1.0);
        psc.performControl(30.5, vdzm);
        Mockito.verify(controller, Mockito.times(0)).changePhase();
        Mockito.verify(controller, Mockito.times(1)).holdPhase();
    }
}