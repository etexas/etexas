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
import java.util.Arrays;
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
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jconnelly
 */

public class RSEDeviceInfoTest {

    long did = -1;

    Map<Integer, ISignalManager> signalMap = null;

    Map<Integer, ILaneManager> laneMap = null;

    Map<Integer, IDetectorManager> detectorMap = null;

    int intersection;

    ReferencePoint[] refs = null;

    List<BasicMessage> messages = null;

    String pid = null;

    IDistanceable loc = null;

    RSEDeviceInfo rseDevInf = null;

    @Before
    public void setup() {

        did = 9875;
        ILaneManager laneManager = genLaneMan();
        intersection = laneManager.getIntersectionId();
        signalMap = new HashMap<Integer, ISignalManager>();
        laneMap = new HashMap<Integer, ILaneManager>();
        detectorMap = new HashMap<Integer, IDetectorManager>();
        signalMap.put(intersection, genSigMan());
        laneMap.put(intersection, laneManager);
        detectorMap.put(intersection, genDetMan());
        refs = genRPA();
        messages = createMessageList();
        loc = v1();
        rseDevInf = genRSEDevInfo();
    }

    @After
    public void teardown() {
        did = -1;
        signalMap = null;
        laneMap = null;
        detectorMap = null;
        refs = null;
        messages = null;
        loc = null;
        rseDevInf = null;
    }

    @Test
    public void testConstructor() {
        assertNotNull(rseDevInf);
    }

    @Test
    public void testGetReferencePoints() {
        assertTrue(Arrays.equals(refs, rseDevInf.getReferencePoints()));
    }

    @Test
    public void testGetReferencePoints2() {
        RSEDeviceInfo rseDevInfo = new RSEDeviceInfo();
        assertTrue(null == rseDevInfo.getReferencePoints());
    }

    @Test
    public void testEqualsIdTrue() {
        RSEDeviceInfo entity = genRSEDevInfo();
        assertTrue(rseDevInf.equalsId(entity));
    }

    @Test
    public void testEqualsIdFalse() {
        RSEDeviceInfo testF = new RSEDeviceInfo(signalMap, laneMap, detectorMap, messages, refs, 22415, loc);
        assertFalse(rseDevInf.equalsId(testF));
        OBUDeviceInfo test2 = new OBUDeviceInfo(v1(), createMessageList(), 1234);
        assertFalse(rseDevInf.equalsId(test2));
    }

    @Test
    public void testGetProperId() {
        assertEquals(rseDevInf.getProperId(), ("RSEDevice:" + did));
    }

    @Test
    public void testGetLocation() {
        assertEquals(rseDevInf.getLocation(), loc);
    }

    @Test
    public void testGetSignalManager() {
        assertEquals(rseDevInf.getSignalManagers(), signalMap);
        assertEquals(rseDevInf.getSignalManager(intersection), signalMap.get(intersection));
    }

    @Test
    public void testGetLaneManager() {
        assertEquals(rseDevInf.getLaneManagers(), laneMap);
        assertEquals(rseDevInf.getLaneManager(intersection), laneMap.get(intersection));
    }

    @Test
    public void testGetDetectorManager() {
        assertEquals(rseDevInf.getDetectorManagers(), detectorMap);
        assertEquals(rseDevInf.getDetectorManager(intersection), detectorMap.get(intersection));
    }

    private RSEDeviceInfo genRSEDevInfo() {
        RSEDeviceInfo rdi = new RSEDeviceInfo(signalMap, laneMap, detectorMap, messages, refs, did, loc);
        return rdi;
    }

    private LaneManager genLaneMan() {
        LaneManager lm = new LaneManager();
        lm.setLanes(laneList());
        return lm;
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

    private ReferencePoint[] genRPA() {
        ReferencePoint rp = new ReferencePoint(1.0, 2.0);
        ReferencePoint[] rpa = (new ReferencePoint[] { rp });
        return rpa;
    }

    private Vehicle v1() {
        Vehicle v1 = new Vehicle(1, 10.0, 10.0, 35.0, 35.0, 0.0);
        v1.setLaneID(1);
        return v1;
    }

    private List<BasicMessage> createMessageList() {
        List<BasicMessage> messageList = new ArrayList<BasicMessage>(2);
        BasicMessage message1 = new DSRCMessage(47, DSRCChannel.CH184, 4321);
        BasicMessage message2 = new DSRCMessage(53, DSRCChannel.CH184, 1234);
        messageList.add(message1);
        messageList.add(message2);
        return messageList;
    }
}
