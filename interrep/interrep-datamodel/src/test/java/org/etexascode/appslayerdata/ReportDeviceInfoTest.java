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
package org.etexascode.appslayerdata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jconnelly
 */

public class ReportDeviceInfoTest {

    Map<Integer, IVehicleManager> vehicleMap = null;

    Map<Integer, ISignalManager> signalMap = null;

    Map<Integer, ILaneManager> laneMap = null;

    Map<Integer, IDetectorManager> detectorMap = null;

    int intersection;

    String properId = null;

    IDistanceable loc = null;

    List<BasicMessage> messages = null;

    long devId = -1;

    ReportDeviceInfo rdi = null;

    @Before
    public void setup() {

        devId = 9876;
        ILaneManager laneManager = genLaneMan();
        intersection = laneManager.getIntersectionId();
        vehicleMap = new HashMap<Integer, IVehicleManager>();
        signalMap = new HashMap<Integer, ISignalManager>();
        laneMap = new HashMap<Integer, ILaneManager>();
        detectorMap = new HashMap<Integer, IDetectorManager>();
        vehicleMap.put(intersection, genVehMan());
        signalMap.put(intersection, genSigMan());
        laneMap.put(intersection, laneManager);
        detectorMap.put(intersection, genDetMan());
        messages = createMessageList();
        loc = v1();
        rdi = genReportDevInfo();
    }

    @After
    public void teardown() {

        devId = -1;
        loc = null;
        vehicleMap = null;
        signalMap = null;
        laneMap = null;
        detectorMap = null;
        messages = null;
        rdi = null;
    }

    @Test
    public void testConstructor() {
        assertNotNull(rdi);
    }

    @Test
    public void testEqualsIdTrue() {
        ReportDeviceInfo rdiTrue = new ReportDeviceInfo(vehicleMap, signalMap, laneMap, detectorMap, messages, messages, devId, loc);
        assertTrue(rdi.equalsId(rdiTrue));
    }

    @Test
    public void testEqualsIdFalse() {
        ReportDeviceInfo rdiFalse = new ReportDeviceInfo(vehicleMap, signalMap, laneMap, detectorMap, messages, messages, 3542, loc);
        assertFalse(rdi.equalsId(rdiFalse));
        OBUDeviceInfo test2 = new OBUDeviceInfo(v1(), createMessageList(), 1234);
        assertFalse(rdi.equalsId(test2));
    }

    @Test
    public void testGetProperId() {
        assertEquals(rdi.getDeviceId(), devId);
    }

    @Test
    public void testGetLocation() {
        assertEquals(rdi.getLocation(), loc);
    }

    @Test
    public void testGetVehicleManager() {
        assertEquals(rdi.getVehicleManagers(), vehicleMap);
        assertEquals(rdi.getVehicleManager(intersection), vehicleMap.get(intersection));
    }

    @Test
    public void testGetSignalManager() {
        assertEquals(rdi.getSignalManagers(), signalMap);
        assertEquals(rdi.getSignalManager(intersection), signalMap.get(intersection));
    }

    @Test
    public void testGetLaneManager() {
        assertEquals(rdi.getLaneManagers(), laneMap);
        assertEquals(rdi.getLaneManager(intersection), laneMap.get(intersection));
    }

    @Test
    public void testGetDetectorManager() {
        assertEquals(rdi.getDetectorManagers(), detectorMap);
        assertEquals(rdi.getDetectorManager(intersection), detectorMap.get(intersection));
    }

    private ReportDeviceInfo genReportDevInfo() {
        ReportDeviceInfo testrdi = new ReportDeviceInfo(vehicleMap, signalMap, laneMap, detectorMap, messages, messages, devId, loc);
        return testrdi;
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

    private List<BasicMessage> createMessageList() {
        List<BasicMessage> messageList = new ArrayList<BasicMessage>(2);
        BasicMessage message1 = new DSRCMessage(47, DSRCChannel.CH184, 4321);
        BasicMessage message2 = new DSRCMessage(53, DSRCChannel.CH184, 1234);
        messageList.add(0, message1);
        messageList.add(1, message2);
        return messageList;
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

    private List<Vehicle> vehList() {
        List<Vehicle> vlist = new ArrayList<Vehicle>(3);
        vlist.add(v1());
        vlist.add(v2());
        vlist.add(v3());
        return vlist;
    }

    private Map<Integer, Lane> laneList() {
        Map<Integer, Lane> llist = new HashMap<Integer, Lane>(1);
        Lane l1 = new Lane();
        l1.setLaneId(1);
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
}
