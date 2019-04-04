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
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.ReportDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.Detector;
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
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ReportDeviceTest {

    ReportDeviceInfo reportDeviceInfo;

    List<BasicMessage> txMessages;

    BasicMessage reportMessage;

    List<BasicMessage> rxMessages;

    long deviceInfoMac;

    IVehicleManager vehicleManager;

    ISignalManager signalManager;

    ILaneManager laneManager;

    IDetectorManager detectorManager;

    @Before
    public void setUp() throws Exception {

        txMessages = new ArrayList<BasicMessage>();
        reportMessage = new DSRCMessage("message", DSRCChannel.CH184, 321);
        txMessages.add(reportMessage);
        rxMessages = new ArrayList<BasicMessage>();
        deviceInfoMac = 123;

        Map<Integer, IVehicleManager> vehicleMap = new HashMap<Integer, IVehicleManager>();
        vehicleManager = new VehicleManager();
        ((VehicleManager)vehicleManager).addVehicle(new Vehicle(1, 500, 200, 0, 0, 0));
        vehicleMap.put(0, vehicleManager);

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

        reportDeviceInfo = new ReportDeviceInfo(vehicleMap, signalMap, laneMap, detectorMap, txMessages, rxMessages, deviceInfoMac, null);
    }

    @After
    public void tearDown() throws Exception {

        txMessages = null;
        reportMessage = null;
        rxMessages = null;
        reportDeviceInfo = null;
        signalManager = null;
        laneManager = null;
        detectorManager = null;
    }

    @Test
    public void testGetSignalManager() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);
        assertTrue(reportDevice.getSignalManager(0).equals(signalManager));
    }

    @Test
    public void testGetSignalManagers() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);

        Map<Integer, ISignalManager> signalMap = reportDevice.getSignalManagers();
        assertTrue(signalMap.get(0).equals(signalManager));
        assertTrue(signalMap.size() == 1);
    }

    @Test
    public void testGetLaneManager() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);
        assertTrue(reportDevice.getLaneManager(0).equals(laneManager));
    }

    @Test
    public void testGetLaneManagers() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);

        Map<Integer, ILaneManager> laneMap = reportDevice.getLaneManagers();
        assertTrue(laneMap.get(0).equals(laneManager));
        assertTrue(laneMap.size() == 1);
    }

    @Test
    public void testGetDetectorManager() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);
        assertTrue(reportDevice.getDetectorManager(0).equals(detectorManager));
    }

    @Test
    public void testGetDetectorManagers() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);

        Map<Integer, IDetectorManager> detectorMap = reportDevice.getDetectorManagers();
        assertTrue(detectorMap.get(0).equals(detectorManager));
        assertTrue(detectorMap.size() == 1);
    }

    @Test
    public void testGetVehicleManager() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);
        assertTrue(reportDevice.getVehicleManager(0).equals(vehicleManager));
    }

    @Test
    public void testGetVehicleManagers() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);

        Map<Integer, IVehicleManager> vehicleMap = reportDevice.getVehicleManagers();
        assertTrue(vehicleMap.get(0).equals(vehicleManager));
        assertTrue(vehicleMap.size() == 1);
    }

    @Test
    public void testGetTxMessages() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);
        Collection<BasicMessage> messages = reportDevice.getTxMessages();
        assertTrue(messages.size() == txMessages.size());
        Iterator<BasicMessage> iterator = messages.iterator();
        while (iterator.hasNext()) {
            assertTrue(txMessages.contains(iterator.next()));
        }
    }

    @Test
    public void testAddAppMessages() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);
        reportDevice.addAppMessages(txMessages);
        List<BasicMessage> appMessages = reportDevice.getAppMessages();

        assertTrue(appMessages.size() == 1);
        assertTrue(appMessages.get(0).equals(reportMessage));
    }

    @Test
    public void testResetAppMessages() {

        ReportDevice reportDevice = new ReportDevice(reportDeviceInfo);
        reportDevice.addAppMessages(txMessages);

        assertTrue(reportDevice.getAppMessages().size() == 1);

        reportDevice.resetAppMessages();

        assertTrue(reportDevice.getAppMessages().size() == 0);
    }
}