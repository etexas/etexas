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

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
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
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class RSEDeviceTest {

    RSEDeviceInfo rseDeviceInfo;

    List<BasicMessage> messages;

    BasicMessage rseMessage;

    long deviceInfoMac;

    ISignalManager signalManager;

    ILaneManager laneManager;

    IDetectorManager detectorManager;

    ReferencePoint refPoint;

    @Before
    public void setUp() throws Exception {

        messages = new ArrayList<BasicMessage>();
        rseMessage = new DSRCMessage("message", DSRCChannel.CH184, 321);
        messages.add(rseMessage);
        deviceInfoMac = 123;

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        signalManager = new SignalManager();
        SignalIndication signalIndication = new SignalIndication();
        signalIndication.setColorIndication(Color.GREEN);
        signalIndication.setLaneId(1);
        signalIndication.setStateIndication(State.FLASHING);
        signalIndication.setTimeToChange(5);
        signalIndication.setTypeIndication(Type.BALL);
        ((SignalManager)signalManager).addSignal(signalIndication);
        signalMap.put(0, signalManager);

        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        laneManager = new LaneManager();
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
        ((LaneManager)laneManager).setLanes(lanes);
        laneMap.put(0, laneManager);

        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        detectorManager = new DetectorManager();
        ((DetectorManager)detectorManager).addDetector(1, new Detector());
        detectorMap.put(0, detectorManager);

        ReferencePoint[] refPoints = new ReferencePoint[1];
        refPoint = new ReferencePoint(1, 1);
        refPoints[0] = refPoint;

        rseDeviceInfo = new RSEDeviceInfo(signalMap, laneMap, detectorMap, messages, refPoints, deviceInfoMac, null);
    }

    @After
    public void tearDown() throws Exception {

        messages = null;
        rseMessage = null;
        rseDeviceInfo = null;
        signalManager = null;
        laneManager = null;
        detectorManager = null;
        refPoint = null;
    }

    @Test
    public void testGetSignalManager() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);
        assertTrue(rseDevice.getSignalManager(0).equals(signalManager));
    }

    @Test
    public void testGetSignalManagers() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);

        Map<Integer, ISignalManager> signalMap = rseDevice.getSignalManagers();
        assertTrue(signalMap.get(0).equals(signalManager));
        assertTrue(signalMap.size() == 1);
    }

    @Test
    public void testGetLaneManager() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);
        assertTrue(rseDevice.getLaneManager(0).equals(laneManager));
    }

    @Test
    public void testGetLaneManagers() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);

        Map<Integer, ILaneManager> laneMap = rseDevice.getLaneManagers();
        assertTrue(laneMap.get(0).equals(laneManager));
        assertTrue(laneMap.size() == 1);
    }

    @Test
    public void testGetDetectorManager() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);
        assertTrue(rseDevice.getDetectorManager(0).equals(detectorManager));
    }

    @Test
    public void testGetDetectorManagers() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);

        Map<Integer, IDetectorManager> detectorMap = rseDevice.getDetectorManagers();
        assertTrue(detectorMap.get(0).equals(detectorManager));
        assertTrue(detectorMap.size() == 1);
    }

    @Test
    public void testGetReferencePoints() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);
        ReferencePoint[] refPoints = rseDevice.getReferencePoints();
        assertTrue(refPoints[0].equals(refPoint));
        assertTrue(refPoints.length == 1);
    }

    @Test
    public void testAddSignalCommmand() {

        SignalCommand signalCommand = SignalCommand.createChangeCommand(5);
        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);
        rseDevice.addSignalCommand(signalCommand);
        assertTrue(rseDevice.getSignalCommands().get(0).equals(signalCommand));
    }

    @Test
    public void testAddAppMessages() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);
        rseDevice.addAppMessages(messages);
        List<BasicMessage> appMessages = rseDevice.getAppMessages();

        assertTrue(appMessages.size() == 1);
        assertTrue(appMessages.get(0).equals(rseMessage));
    }

    @Test
    public void testResetAppMessages() {

        RSEDevice rseDevice = new RSEDevice(rseDeviceInfo);
        rseDevice.addAppMessages(messages);

        assertTrue(rseDevice.getAppMessages().size() == 1);

        rseDevice.resetAppMessages();

        assertTrue(rseDevice.getAppMessages().size() == 0);
    }
}