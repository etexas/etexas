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
package com.harmonia.apps;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.AppLoggerImpl;
import org.etexascode.apps.MapDataUtil;
import org.etexascode.apps.RSEDevice;
import org.etexascode.apps.UtilsEquals;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.FormattedDSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.j2735.MapData;
import org.etexascode.test.TestHarnessCase;
import org.etexascode.test.TestInterRep;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Test for the MapDataProducerApp class
 */
@RunWith(PowerMockRunner.class)
public class MapDataProducerAppTest {

    /** Static Logger. */
    private final static Logger LOGGER = LoggerFactory.getLogger(MapDataProducerAppTest.class);

    LaneManager lmanager = null;

    LaneMovement move = null;

    Short msgCount = null;

    /** Create lists of Device Apps. */
    List<IConnectedVehicleApp<?>> deviceApps = null;

    /** List of returned MapData messages. */
    List<BasicMessage> mapDataList = null;

    TestHarnessCase thc = null;

    IConnectedVehicleApp<RSEDevice> mapApp = null;

    /** The messages. */
    Object[] messages = null;

    @Before
    public void setUp() throws Exception {
        mapApp = new MapDataProducerApp();
        deviceApps = new LinkedList<IConnectedVehicleApp<?>>();
        mapDataList = new ArrayList<BasicMessage>();
        messages = new Object[] {};
        thc = new TestHarnessCase();
        msgCount = 1;

        Lane lane1 = new Lane();
        Lane lane2 = new Lane();
        double latitude = 0;
        double longitude = 0;
        double speedLimitInMetersPerSecond = 15.7;
        LaneNode node1 = new LaneNode(-146, 8432);
        LaneNode node2 = new LaneNode(-146, 9225);
        LaneNode node3 = new LaneNode(-466, 8679);
        LaneNode node4 = new LaneNode(-466, 9288);

        lmanager = new LaneManager();
        lmanager.setLatitude(latitude);
        lmanager.setLongitude(longitude);
        lmanager.setElevation(0.0);

        move = new LaneMovement();
        move.setMovementId(3);
        move.setMovement(Movement.STRAIGHT);

        Map<Integer, LaneMovement> lm = new HashMap<Integer, LaneMovement>();
        lm.put(1, move);

        lane1.setApproachId(4);
        lane1.setLaneId(1);
        lane1.setSpeedLimitInMetersPerSecond(speedLimitInMetersPerSecond);
        lane1.addLaneNode(node1);
        lane1.addLaneNode(node2);
        lane1.setType(Lane.INBOUND);

        lane2.setApproachId(4);
        lane2.setLaneId(2);
        lane2.setLaneMovements(lm);
        lane2.setSpeedLimitInMetersPerSecond(speedLimitInMetersPerSecond);
        lane2.addLaneNode(node3);
        lane2.addLaneNode(node4);
        lane2.setType(Lane.INBOUND);

        Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();
        lanes.put(1, lane1);
        lanes.put(2, lane2);

        lmanager.setLanes(lanes);
    }

    @After
    public void tearDown() throws Exception {
        lmanager = null;
        move = null;
        msgCount = null;
        deviceApps = null;
        mapDataList = null;
        thc = null;
        mapApp = null;
        messages = null;
    }

    @Test
    public void testLifecycle() {
        MapDataProducerApp app = (MapDataProducerApp)mapApp;
        Boolean hasFormatted = true;
        Double frequency = 2.5;
        String[] appConfigs = new String[] { hasFormatted.toString(), frequency.toString() };
        app.init(appConfigs);
        assertTrue(hasFormatted.equals(app.hasFormattedData));
        assertEquals(app.frequency, frequency, 0.001);
        app.appShutdown(null);
    }

    @Test
    public void testLifecycle2() {
        MapDataProducerApp app = (MapDataProducerApp)mapApp;
        Boolean hasFormatted = false;
        Double frequency = 2.5;
        String[] appConfigs = new String[] { hasFormatted.toString(), frequency.toString() };
        app.init(appConfigs);
        assertTrue(hasFormatted.equals(app.hasFormattedData));
        assertEquals(app.frequency, frequency, 0.001);
        app.appShutdown(null);
    }

    @Test
    public void testCreateMapDataMessage() {
        Lane lane = lmanager.getLaneById(1);
        MapData mapNew = MapDataProducerApp.createFormattedMapDataMessage(lmanager, msgCount, null, lmanager.getLatitude(), lmanager.getLongitude());
        List<Lane> newLanes = MapDataUtil.getLaneVectorsFromMapData(mapNew);

        /**
         * Lane movements aren't part of the map data J2735 standard. The lane we construct from the
         * map data should be the original lane from the lane manager, without the Lane Movement
         * map.
         */

        assertEquals(lane, newLanes.get(0));

    }

    @Test
    public void testGetVehicleLaneAttributesFromLaneMovement() {
        int laneAttribute = MapDataProducerApp.getVehicleLaneAttributesFromLaneMovement(move);
        assertEquals(2, laneAttribute);
    }

    @Test
    public void testClip() {
        double baseLat = 0.0;
        double baseLong = 0.0;
        double curLat = 0.003;
        double curLong = 0.003;
        int geoCalcType = 1;
        double z = 10;
        double w = 11;

        double[] offsets = UtilsLatLongConversion.convertLatLongToCentimeterOffset(baseLat, baseLong, curLat, curLong, geoCalcType);

        double cornerx = offsets[0] + Short.MIN_VALUE;
        double cornery = offsets[1] + Short.MAX_VALUE;
        double outputCornerx = Short.MIN_VALUE;
        double outputCornery = Short.MAX_VALUE;

        LaneNode input1 = new LaneNode(cornerx - 5, cornery - 5, z, w);
        LaneNode input2 = new LaneNode(cornerx + 5, cornery - 5, z, w);
        LaneNode input3 = new LaneNode(cornerx + 5, cornery + 5, z, w);

        LaneNode output1 = new LaneNode(outputCornerx, outputCornery - 5, z, w);
        LaneNode output2 = new LaneNode(outputCornerx + 5, outputCornery - 5, z, w);
        LaneNode output3 = new LaneNode(outputCornerx + 5, outputCornery, z, w);

        List<ILaneNode> input = new ArrayList<ILaneNode>();
        List<ILaneNode> output = new ArrayList<ILaneNode>();

        input.add(input1);
        input.add(input2);
        input.add(input3);

        output.add(output1);
        output.add(output2);
        output.add(output3);

        List<ILaneNode> actualOutput = MapDataProducerApp.clip(input, baseLat, baseLong, curLat, curLong, geoCalcType);
        assertEquals(output.size(), actualOutput.size());
        for (int k = 0; k < output.size(); ++k) {
            assertTrue(UtilsEquals.closelyEquals(output.get(k), actualOutput.get(k)));
        }
    }

    @Test
    public void testAppLoggerWarning() {
        LaneManager lm = new LaneManager();
        lm.setLatitude(0.0);
        lm.setLongitude(0.0);

        Lane l1 = new Lane();
        l1.setLaneId(1);
        l1.setApproachId(1);
        l1.addLaneNode(new LaneNode(32767 - 100, 0, 0, 1));
        l1.addLaneNode(new LaneNode(34023.69 + 100, 0, 0, 1));
        lm.getLanes().put(1, l1);

        ReferencePoint[] rps = new ReferencePoint[1];
        rps[0] = new ReferencePoint(40, 80);

        RSEDevice mockDevice = PowerMockito.mock(RSEDevice.class);
        AppLogger mockAppLogger = PowerMockito.mock(AppLogger.class);

        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        laneMap.put(lm.getIntersectionId(), lm);
        PowerMockito.when(mockDevice.getLaneManagers()).thenReturn(laneMap);
        PowerMockito.when(mockDevice.getReferencePoints()).thenReturn(rps);

        MapDataProducerApp app = new MapDataProducerApp();
        app.performUpdate(mockDevice, null, null, 5.5, mockAppLogger);

        Mockito.verify(mockAppLogger).log(Mockito.anyString(),
                Mockito.anyString());
    }

    @Test
    public void testPerformUpdate() throws Exception {

        // from setup
        // Add apps to list.
        String hasFormatted = "true";
        String frequency = "2.5";
        String[] appConfigs = new String[] { hasFormatted, frequency };
        if (mapApp instanceof IAppLifecycle) {
            ((IAppLifecycle)mapApp).init(appConfigs);
        }
        deviceApps.add(mapApp);

        // Create test InterRep.
        TestInterRep testInterRep = null;
        try {
            testInterRep = thc.getTestInterRep(true);
        }
        catch (Exception e) {
            LOGGER.debug("Can't generate test interrep", e);
            throw e;
        }

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        signalMap.put(testInterRep.getId(), testInterRep.getSignalManager());
        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        laneMap.put(testInterRep.getId(), testInterRep.getLaneManager());
        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        detectorMap.put(testInterRep.getId(), testInterRep.getDetectorManager());
        RSEDeviceInfo rdi = new RSEDeviceInfo(signalMap, laneMap, detectorMap, new ArrayList<BasicMessage>(0), new ReferencePoint[] {}, 2, new DistanceImpl());

        /**
         * Update and get new mapData messages for this timestep
         */
        AppLogger logger = new AppLoggerImpl(0, null, 0);
        if (mapApp instanceof IAppName) {
            logger = new AppLoggerImpl(2, ((IAppName)mapApp).getAppName(), testInterRep.getSimTime());
        }
        else {
            logger = new AppLoggerImpl(2, mapApp.getClass().getSimpleName(), testInterRep.getSimTime());
        }
        RSEDevice device = new RSEDevice(rdi);
        ((MapDataProducerApp)mapApp).performUpdate(device, messages, new ArrayList<BasicMessage>(0), 1.0, logger);
        mapDataList = device.getAppMessages();
        device.resetAppMessages();
        // actual test
        FormattedDSRCMessage message = (FormattedDSRCMessage)mapDataList.get(0);
        Object data = message.getData();
        Object formattedData = message.getFormattedData();
        assertTrue(data instanceof byte[]);
        assertTrue(formattedData instanceof MapData);

    }

    @Test
    public void testPerformUpdate2() throws Exception {

        // from setup
        // Add apps to list.
        String hasFormatted = "false";
        String frequency = "1.0";
        String[] appConfigs = new String[] { hasFormatted, frequency };
        if (mapApp instanceof IAppLifecycle) {
            ((IAppLifecycle)mapApp).init(appConfigs);
        }
        deviceApps.add(mapApp);

        // Create test InterRep.
        TestInterRep testInterRep = null;
        try {
            testInterRep = thc.getTestInterRep(true);
        }
        catch (Exception e) {
            LOGGER.debug("Can't generate test interrep", e);
            throw e;
        }

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        signalMap.put(testInterRep.getId(), testInterRep.getSignalManager());
        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        laneMap.put(testInterRep.getId(), testInterRep.getLaneManager());
        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        detectorMap.put(testInterRep.getId(), testInterRep.getDetectorManager());
        RSEDeviceInfo rdi = new RSEDeviceInfo(signalMap, laneMap, detectorMap, new ArrayList<BasicMessage>(0), new ReferencePoint[] {}, 2, new DistanceImpl());

        /**
         * Update and get new mapData messages for this timestep
         */
        AppLogger logger = new AppLoggerImpl(0, null, 0);
        if (mapApp instanceof IAppName) {
            logger = new AppLoggerImpl(2, ((IAppName)mapApp).getAppName(), testInterRep.getSimTime());
        }
        else {
            logger = new AppLoggerImpl(2, mapApp.getClass().getSimpleName(), testInterRep.getSimTime());
        }
        RSEDevice device = new RSEDevice(rdi);
        ((MapDataProducerApp)mapApp).performUpdate(device, messages, new ArrayList<BasicMessage>(0), 1.0, logger);
        mapDataList = device.getAppMessages();
        device.resetAppMessages();

        BasicMessage message = mapDataList.get(0);
        Object data = message.getData();
        assertTrue(data instanceof byte[]);

    }

    @Test
    public void testGetAppID() {
        assertEquals(((IAppName)mapApp).getAppName(), MapDataProducerApp.APP_NAME_MAP_DATA_PRODUCER_APP);
    }
}