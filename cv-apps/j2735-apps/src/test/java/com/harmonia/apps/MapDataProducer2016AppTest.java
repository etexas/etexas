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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.RSEDevice;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735_2016.elements.AllowedManeuvers;
import org.etexascode.j2735_2016.elements.ApproachID;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LaneDirection;
import org.etexascode.j2735_2016.elements.LaneSharing;
import org.etexascode.j2735_2016.elements.LayerType.Layer;
import org.etexascode.j2735_2016.elements.OffsetB10;
import org.etexascode.j2735_2016.elements.OffsetB11;
import org.etexascode.j2735_2016.elements.OffsetB12;
import org.etexascode.j2735_2016.elements.OffsetB13;
import org.etexascode.j2735_2016.elements.OffsetB14;
import org.etexascode.j2735_2016.elements.OffsetB16;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.etexascode.j2735_2016.frames.GenericLane;
import org.etexascode.j2735_2016.frames.IntersectionGeometry;
import org.etexascode.j2735_2016.frames.IntersectionReferenceID;
import org.etexascode.j2735_2016.frames.LaneAttributes;
import org.etexascode.j2735_2016.frames.LaneDataAttribute;
import org.etexascode.j2735_2016.frames.NodeAttributeSetXY;
import org.etexascode.j2735_2016.frames.NodeXY;
import org.etexascode.j2735_2016.frames.NodeXY24B;
import org.etexascode.j2735_2016.frames.NodeXY32B;
import org.etexascode.j2735_2016.frames.Position3D;
import org.etexascode.j2735_2016.frames.RegulatorySpeedLimit;
import org.etexascode.j2735_2016.messages.MapData;
import org.etexascode.j2735_2016.messages.MessageFrame;
import org.junit.Test;

/**
 * Tests for MapDataProducer2016App.
 * 
 * @author ttevendale
 */
public class MapDataProducer2016AppTest {

    @Test
    public void testGetAppName() {

        assertTrue(MapDataProducer2016App.APP_NAME_MAP_DATA_PRODUCER_2016_APP.equals(new MapDataProducer2016App().getAppName()));
    }

    @Test
    public void testInitFrequency() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);
        laneManagerMap.put(1, createSimpleLaneManager());
        MapDataProducer2016App mapDataProducer = new MapDataProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        mapDataProducer.init(new String[] { "1.0" });

        // The device will send a message.
        mapDataProducer.performUpdate(device, null, null, 0.0, null);
        assertFalse(device.getAppMessages().isEmpty());

        device.getAppMessages().clear();

        // The device will not send a message since 1 second has not elapsed since the last time.
        mapDataProducer.performUpdate(device, null, null, 0.5, null);
        assertTrue(device.getAppMessages().isEmpty());

        device.getAppMessages().clear();

        // The device will send a message since its been a second.
        mapDataProducer.performUpdate(device, null, null, 1.0, null);
        assertFalse(device.getAppMessages().isEmpty());
    }

    @Test
    public void testPerformUpdate() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);
        laneManagerMap.put(1, createSimpleLaneManager());
        MapDataProducer2016App mapProducer = new MapDataProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        mapProducer.performUpdate(device, null, null, 8000.0, null);

        MapData mapData = MessageFrame.decodeMapData(new String((byte[])device.getAppMessages().get(0).getData()));

        // 8000 (simTime) / 60 truncated = 133
        assertTrue(mapData.getTimeStamp().getValue() == 133);
        assertTrue(mapData.getMsgIssueRevision().getValue() == 0);
        assertTrue(mapData.getLayerType().getEnumeration().equals(Layer.INTERSECTION_DATA));

        IntersectionGeometry[] intersections = mapData.getIntersections().getIntersectionGeometryArray();
        assertTrue(intersections.length == 1);

        IntersectionGeometry intersection = intersections[0];

        IntersectionReferenceID id = intersection.getId();
        assertNull(id.getRegion());
        assertTrue(id.getId().getValue() == 1);

        assertTrue(intersection.getRevision().getValue() == 0);

        Position3D refPoint = intersection.getRefPoint();
        assertTrue(refPoint.getLatitude().getValue() == 100000000);
        assertTrue(refPoint.getLongitude().getValue() == 820000000);
        assertTrue(refPoint.getElevation().getValue() == -4);

        assertNull(intersection.getLaneWidth());
        assertNull(intersection.getSpeedLimits());

        GenericLane[] lanes = intersection.getLaneSet().getLaneArray();
        assertTrue(lanes.length == 2);

        GenericLane lane = lanes[0];
        assertTrue(lane.getLaneId().getValue() == 1);
        assertTrue(lane.getIngressApproach().getValue() == 1);
        assertNull(lane.getEgressApproach());

        LaneAttributes laneAttributes = lane.getLaneAttributes();

        LaneDirection laneDirection = laneAttributes.getDirectionalUse();
        assertTrue(laneDirection.isIngressPath());
        assertFalse(laneDirection.isEgressPath());

        LaneSharing LaneSharing = laneAttributes.getSharedWith();
        assertFalse(LaneSharing.isOverlappingLaneDescriptionProvided());
        assertFalse(LaneSharing.isMultipleLanesTreatedAsOneLane());
        assertFalse(LaneSharing.isOtherNonMotorizedTrafficTypes());
        assertTrue(LaneSharing.isIndividualMotorizedVehicleTraffic());
        assertTrue(LaneSharing.isBusVehicleTraffic());
        assertTrue(LaneSharing.isTaxiVehicleTraffic());
        assertFalse(LaneSharing.isPedestriansTraffic());
        assertFalse(LaneSharing.isCyclistVehicleTraffic());
        assertFalse(LaneSharing.isTrackedVehicleTraffic());
        assertFalse(LaneSharing.isPedestrianTraffic());

        LaneAttributesVehicle vehicleAttributes = laneAttributes.getLaneType().getVehicle();
        assertFalse(vehicleAttributes.isVehicleRevocableLane());
        assertFalse(vehicleAttributes.isVehicleFlyOverLane());
        assertFalse(vehicleAttributes.isHovLaneUseOnly());
        assertFalse(vehicleAttributes.isRestrictedToBusUse());
        assertFalse(vehicleAttributes.isRestrictedToTaxiUse());
        assertFalse(vehicleAttributes.isRestrictedFromPublicUse());
        assertFalse(vehicleAttributes.hasIRbeaconCoverage());
        assertFalse(vehicleAttributes.hasPermissionOnRequest());

        AllowedManeuvers maneuvers = lane.getManeuvers();
        assertFalse(maneuvers.isManeuverStraightAllowed());
        assertTrue(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertTrue(maneuvers.isManeuverUTurnAllowed());
        assertTrue(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertFalse(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());

        NodeXY[] nodes = lane.getNodeList().getNodes().getNodeArray();
        assertTrue(nodes.length == 2);

        NodeXY node = nodes[0];

        NodeXY24B delta24 = node.getDelta().getNodeXY24B();
        assertTrue(delta24.getX().getValue() == -182);
        assertTrue(delta24.getY().getValue() == 1341);

        NodeAttributeSetXY nodeAttributes = node.getAttributes();
        assertNull(nodeAttributes.getLocalNode());
        assertNull(nodeAttributes.getDisabled());
        assertNull(nodeAttributes.getEnabled());

        LaneDataAttribute[] data = nodeAttributes.getData().getAttributeArray();
        assertTrue(data.length == 1);

        RegulatorySpeedLimit[] speedLimits = data[0].getSpeedLimits().getSpeedLimitArray();
        assertTrue(speedLimits.length == 1);

        RegulatorySpeedLimit speedLimit = speedLimits[0];
        assertTrue(speedLimit.getType().getEnumeration().equals(SpeedLimit.VEHICLE_MAX_SPEED));
        assertTrue(speedLimit.getSpeed().getValue() == (int)UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(15));

        assertTrue(nodeAttributes.getDWidth().getValue() == 365);
        assertNull(nodeAttributes.getDElevation());

        node = nodes[1];

        NodeXY32B delta32 = node.getDelta().getNodeXY32B();
        assertTrue(delta32.getX().getValue() == -182);
        assertTrue(delta32.getY().getValue() == 25725);

        assertNull(node.getAttributes());

        lane = lanes[1];
        assertTrue(lane.getLaneId().getValue() == 2);
        assertNull(lane.getIngressApproach());
        assertTrue(lane.getEgressApproach().getValue() == 2);

        laneAttributes = lane.getLaneAttributes();

        laneDirection = laneAttributes.getDirectionalUse();
        assertFalse(laneDirection.isIngressPath());
        assertTrue(laneDirection.isEgressPath());

        LaneSharing = laneAttributes.getSharedWith();
        assertFalse(LaneSharing.isOverlappingLaneDescriptionProvided());
        assertFalse(LaneSharing.isMultipleLanesTreatedAsOneLane());
        assertFalse(LaneSharing.isOtherNonMotorizedTrafficTypes());
        assertTrue(LaneSharing.isIndividualMotorizedVehicleTraffic());
        assertTrue(LaneSharing.isBusVehicleTraffic());
        assertTrue(LaneSharing.isTaxiVehicleTraffic());
        assertFalse(LaneSharing.isPedestriansTraffic());
        assertFalse(LaneSharing.isCyclistVehicleTraffic());
        assertFalse(LaneSharing.isTrackedVehicleTraffic());
        assertFalse(LaneSharing.isPedestrianTraffic());

        vehicleAttributes = laneAttributes.getLaneType().getVehicle();
        assertFalse(vehicleAttributes.isVehicleRevocableLane());
        assertFalse(vehicleAttributes.isVehicleFlyOverLane());
        assertFalse(vehicleAttributes.isHovLaneUseOnly());
        assertFalse(vehicleAttributes.isRestrictedToBusUse());
        assertFalse(vehicleAttributes.isRestrictedToTaxiUse());
        assertFalse(vehicleAttributes.isRestrictedFromPublicUse());
        assertFalse(vehicleAttributes.hasIRbeaconCoverage());
        assertFalse(vehicleAttributes.hasPermissionOnRequest());

        maneuvers = lane.getManeuvers();
        assertFalse(maneuvers.isManeuverStraightAllowed());
        assertFalse(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertFalse(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());

        nodes = lane.getNodeList().getNodes().getNodeArray();
        assertTrue(nodes.length == 2);

        node = nodes[0];

        delta32 = node.getDelta().getNodeXY32B();
        assertTrue(delta32.getX().getValue() == -548);
        assertTrue(delta32.getY().getValue() == 25725);

        nodeAttributes = node.getAttributes();
        assertNull(nodeAttributes.getLocalNode());
        assertNull(nodeAttributes.getDisabled());
        assertNull(nodeAttributes.getEnabled());

        data = nodeAttributes.getData().getAttributeArray();
        assertTrue(data.length == 1);

        speedLimits = data[0].getSpeedLimits().getSpeedLimitArray();
        assertTrue(speedLimits.length == 1);

        speedLimit = speedLimits[0];
        assertTrue(speedLimit.getType().getEnumeration().equals(SpeedLimit.VEHICLE_MAX_SPEED));
        assertTrue(speedLimit.getSpeed().getValue() == (int)UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(15));

        assertTrue(nodeAttributes.getDWidth().getValue() == 365);
        assertNull(nodeAttributes.getDElevation());

        node = nodes[1];

        delta24 = node.getDelta().getNodeXY24B();
        assertTrue(delta24.getX().getValue() == -548);
        assertTrue(delta24.getY().getValue() == 1341);

        nodeAttributes = node.getAttributes();
        assertNull(nodeAttributes.getLocalNode());
        assertNull(nodeAttributes.getDisabled());
        assertNull(nodeAttributes.getEnabled());
        assertNull(nodeAttributes.getData());
        assertTrue(nodeAttributes.getDWidth().getValue() == 5);
    }

    @Test
    public void testPerformUpdateApproachIdAboveMax() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        Lane lane = laneManager.getLaneById(1);
        lane.setApproachId(ApproachID.MAX + 1);

        laneManagerMap.put(1, laneManager);
        MapDataProducer2016App mapProducer = new MapDataProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        mapProducer.performUpdate(device, null, null, 8000.0, null);

        MapData mapData = MessageFrame.decodeMapData(new String((byte[])device.getAppMessages().get(0).getData()));

        assertTrue(mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getIngressApproach().getValue() == ApproachID.UNKNOWN);
    }

    @Test
    public void testPerformUpdateApproachIdBelowMin() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        Lane lane = laneManager.getLaneById(1);
        lane.setApproachId(ApproachID.MIN - 1);

        laneManagerMap.put(1, laneManager);
        MapDataProducer2016App mapProducer = new MapDataProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        mapProducer.performUpdate(device, null, null, 8000.0, null);

        MapData mapData = MessageFrame.decodeMapData(new String((byte[])device.getAppMessages().get(0).getData()));

        assertTrue(mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getIngressApproach().getValue() == ApproachID.UNKNOWN);
    }

    @Test
    public void testPeformUpdateNodeXY20B() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        LaneNode node = (LaneNode)laneManager.getLaneById(1).getLaneGeomList().get(0);

        laneManagerMap.put(1, laneManager);

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        // x below threshold
        MapData mapData = testOffset(OffsetB10.MIN - 1, 0, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY22B().getX();

        // x above threshold
        mapData = testOffset(OffsetB10.MAX + 1, 0, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY22B().getX();

        // y below threshold
        mapData = testOffset(0, OffsetB10.MIN - 1, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY22B().getX();

        // y above threshold
        mapData = testOffset(0, OffsetB10.MAX + 1, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY22B().getX();

        // x and y within thresholds
        mapData = testOffset(OffsetB10.MAX, OffsetB10.MAX, device, node);

        // will throw a nullPointer if the NodeXY20B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY20B().getX();
    }

    @Test
    public void testPeformUpdateNodeXY22B() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        LaneNode node = (LaneNode)laneManager.getLaneById(1).getLaneGeomList().get(0);

        laneManagerMap.put(1, laneManager);

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        // x below threshold
        MapData mapData = testOffset(OffsetB11.MIN - 1, 0, device, node);

        // will throw a nullPointer if the NodeXY24B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY24B().getX();

        // x above threshold
        mapData = testOffset(OffsetB11.MAX + 1, 0, device, node);

        // will throw a nullPointer if the NodeXY24B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY24B().getX();

        // y below threshold
        mapData = testOffset(0, OffsetB11.MIN - 1, device, node);

        // will throw a nullPointer if the NodeXY24B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY24B().getX();

        // y above threshold
        mapData = testOffset(0, OffsetB11.MAX + 1, device, node);

        // will throw a nullPointer if the NodeXY24B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY24B().getX();

        // x and y within thresholds
        mapData = testOffset(OffsetB11.MAX, OffsetB11.MAX, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY22B().getX();
    }

    @Test
    public void testPeformUpdateNodeXY24B() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        LaneNode node = (LaneNode)laneManager.getLaneById(1).getLaneGeomList().get(0);

        laneManagerMap.put(1, laneManager);

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        // x below threshold
        MapData mapData = testOffset(OffsetB12.MIN - 1, 0, device, node);

        // will throw a nullPointer if the NodeXY26B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY26B().getX();

        // x above threshold
        mapData = testOffset(OffsetB12.MAX + 1, 0, device, node);

        // will throw a nullPointer if the NodeXY26B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY26B().getX();

        // y below threshold
        mapData = testOffset(0, OffsetB12.MIN - 1, device, node);

        // will throw a nullPointer if the NodeXY26B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY26B().getX();

        // y above threshold
        mapData = testOffset(0, OffsetB12.MAX + 1, device, node);

        // will throw a nullPointer if the NodeXY26B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY26B().getX();

        // x and y within thresholds
        mapData = testOffset(OffsetB12.MAX, OffsetB12.MAX, device, node);

        // will throw a nullPointer if the NodeXY24B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY24B().getX();
    }

    @Test
    public void testPeformUpdateNodeXY26B() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        LaneNode node = (LaneNode)laneManager.getLaneById(1).getLaneGeomList().get(0);

        laneManagerMap.put(1, laneManager);

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        // x below threshold
        MapData mapData = testOffset(OffsetB13.MIN - 1, 0, device, node);

        // will throw a nullPointer if the NodeXY28B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY28B().getX();

        // x above threshold
        mapData = testOffset(OffsetB13.MAX + 1, 0, device, node);

        // will throw a nullPointer if the NodeXY28B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY28B().getX();

        // y below threshold
        mapData = testOffset(0, OffsetB13.MIN - 1, device, node);

        // will throw a nullPointer if the NodeXY28B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY28B().getX();

        // y above threshold
        mapData = testOffset(0, OffsetB13.MAX + 1, device, node);

        // will throw a nullPointer if the NodeXY28B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY28B().getX();

        // x and y within thresholds
        mapData = testOffset(OffsetB13.MAX, OffsetB13.MAX, device, node);

        // will throw a nullPointer if the NodeXY26B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY26B().getX();
    }

    @Test
    public void testPeformUpdateNodeXY28B() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        LaneNode node = (LaneNode)laneManager.getLaneById(1).getLaneGeomList().get(0);

        laneManagerMap.put(1, laneManager);

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        // x below threshold
        MapData mapData = testOffset(OffsetB14.MIN - 1, 0, device, node);

        // will throw a nullPointer if the NodeXY32B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B().getX();

        // x above threshold
        mapData = testOffset(OffsetB14.MAX + 1, 0, device, node);

        // will throw a nullPointer if the NodeXY32B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B().getX();

        // y below threshold
        mapData = testOffset(0, OffsetB14.MIN - 1, device, node);

        // will throw a nullPointer if the NodeXY32B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B().getX();

        // y above threshold
        mapData = testOffset(0, OffsetB14.MAX + 1, device, node);

        // will throw a nullPointer if the NodeXY32B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B().getX();

        // x and y within thresholds
        mapData = testOffset(OffsetB14.MAX, OffsetB14.MAX, device, node);

        // will throw a nullPointer if the NodeXY28B was not used as the delta
        mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY28B().getX();
    }

    @Test
    public void testPeformUpdateNodeXY32B() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);

        ILaneManager laneManager = createSimpleLaneManager();
        LaneNode node = (LaneNode)laneManager.getLaneById(1).getLaneGeomList().get(0);

        laneManagerMap.put(1, laneManager);

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        // x below threshold
        MapData mapData = testOffset(OffsetB16.MIN - 1, 0, device, node);

        NodeXY32B nodeXY32b = mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B();
        assertTrue(nodeXY32b.getX().getValue() == OffsetB16.MIN);
        assertTrue(nodeXY32b.getY().getValue() == 0);

        // x above threshold
        mapData = testOffset(OffsetB16.MAX + 1, 0, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        nodeXY32b = mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B();
        assertTrue(nodeXY32b.getX().getValue() == OffsetB16.MAX);
        assertTrue(nodeXY32b.getY().getValue() == 0);

        // y below threshold
        mapData = testOffset(0, OffsetB16.MIN - 1, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        nodeXY32b = mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B();
        assertTrue(nodeXY32b.getX().getValue() == 0);
        assertTrue(nodeXY32b.getY().getValue() == OffsetB16.MIN);

        // y above threshold
        mapData = testOffset(0, OffsetB16.MAX + 1, device, node);

        // will throw a nullPointer if the NodeXY22B was not used as the delta
        nodeXY32b = mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B();
        assertTrue(nodeXY32b.getX().getValue() == 0);
        assertTrue(nodeXY32b.getY().getValue() == OffsetB16.MAX);

        // x and y within thresholds
        mapData = testOffset(25000, 25000, device, node);

        // will throw a nullPointer if the NodeXY20B was not used as the delta
        nodeXY32b = mapData.getIntersections().getIntersectionGeometryArray()[0].getLaneSet().getLaneArray()[0].getNodeList().getNodes().getNodeArray()[1].getDelta().getNodeXY32B();
        assertTrue(nodeXY32b.getX().getValue() == 25000);
        assertTrue(nodeXY32b.getY().getValue() == 25000);
    }

    @Test
    public void testPerformUpdateMultipleIntersections() {

        Map<Integer, ILaneManager> laneManagerMap = new HashMap<Integer, ILaneManager>(1);
        laneManagerMap.put(1, createSimpleLaneManager());
        laneManagerMap.put(2, createSimpleLaneManager2());
        MapDataProducer2016App mapProducer = new MapDataProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(new HashMap<Integer, ISignalManager>(), laneManagerMap, new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        mapProducer.performUpdate(device, null, null, 8000.0, null);

        MapData mapData = MessageFrame.decodeMapData(new String((byte[])device.getAppMessages().get(0).getData()));

        assertTrue(mapData.getIntersections().getIntersectionGeometryArray().length == 2);
    }

    /**
     * Helps test the nodeXY offsets by moving all the duplicated code to this method. This method
     * sets the node's x and y values, creates the mapdata producer, runs the mapdata producer's
     * performUpdate, decodes the message, and clears the device messages.
     * 
     * @param x The x value to set.
     * @param y The y value to set.
     * @param device The device that has the lane manager.
     * @param node The node that will be updated.
     * @return The mapdata message
     */
    private MapData testOffset(int x, int y, RSEDevice device, LaneNode node) {

        node.setX(x);
        node.setY(y);

        new MapDataProducer2016App().performUpdate(device, null, null, 8000.0, null);

        MapData mapData = MessageFrame.decodeMapData(new String((byte[])device.getAppMessages().get(0).getData()));
        device.getAppMessages().clear();

        return mapData;
    }

    /**
     * Creates a lane manager.
     * 
     * @return The created lane manager.
     */
    private ILaneManager createSimpleLaneManager() {

        LaneManager laneManager = new LaneManager();
        laneManager.setIntersectionId(1);
        laneManager.setLatitude(10);
        laneManager.setLongitude(82);
        laneManager.setElevation(-40);
        laneManager.setLanes(createSimpleLanes());

        return laneManager;
    }

    /**
     * Creates lanes.
     * 
     * @return The created lanes.
     */
    private Map<Integer, Lane> createSimpleLanes() {

        Map<Integer, Lane> laneMap = new HashMap<Integer, Lane>();

        List<LaneNode> nodes = new ArrayList<LaneNode>();
        nodes.add(new LaneNode(-182.88, 25725.12, 0, 365));
        nodes.add(new LaneNode(-182.88, 1341.52, 0, 365));
        laneMap.put(1, createLane(1, 1, 1, nodes, new Movement[] { Movement.LEFT_TURN, Movement.LEFT_TURN_ON_RED, Movement.U_TURN }, 15, Lane.INBOUND));

        nodes = new ArrayList<LaneNode>();
        nodes.add(new LaneNode(-548.88, 25725.12, 0, 365));
        nodes.add(new LaneNode(-548.88, 1341.52, 0, 370));
        laneMap.put(2, createLane(1, 2, 2, nodes, new Movement[] {}, 15, Lane.OUTBOUND));

        return laneMap;
    }

    /**
     * Creates a lane manager.
     * 
     * @return The created lane manager.
     */
    private ILaneManager createSimpleLaneManager2() {

        LaneManager laneManager = new LaneManager();
        laneManager.setIntersectionId(1);
        laneManager.setLatitude(10);
        laneManager.setLongitude(82);
        laneManager.setElevation(-40);
        laneManager.setLanes(createSimpleLanes2());

        return laneManager;
    }

    /**
     * Creates lanes.
     * 
     * @return The created lanes.
     */
    private Map<Integer, Lane> createSimpleLanes2() {

        Map<Integer, Lane> laneMap = new HashMap<Integer, Lane>();

        List<LaneNode> nodes = new ArrayList<LaneNode>();
        nodes.add(new LaneNode(-182.88, 25725.12, 0, 365));
        nodes.add(new LaneNode(-182.88, 1341.52, 0, 365));
        laneMap.put(1, createLane(1, 1, 1, nodes, new Movement[] { Movement.RIGHT_TURN, Movement.RIGHT_TURN_ON_RED, Movement.STRAIGHT }, 15, Lane.INBOUND));

        nodes = new ArrayList<LaneNode>();
        nodes.add(new LaneNode(-548.88, 25725.12, 0, 365));
        nodes.add(new LaneNode(-548.88, 1341.52, 0, 365));
        laneMap.put(2, createLane(1, 2, 2, nodes, new Movement[] {}, 15, Lane.OUTBOUND));

        return laneMap;
    }

    /**
     * Creates a lane.
     * 
     * @param intersectionId The intersection ID of the lane.
     * @param approachId The approach ID of the lane.
     * @param laneId The lane's ID.
     * @param nodes The nodes of the lane.
     * @param movements The movements of the lane.
     * @param speedLimit The speedLimit of the lane.
     * @param type The type of the lane.
     * @return The lane.
     */
    private Lane createLane(int intersectionId, int approachId, int laneId, List<LaneNode> nodes, Movement[] movements, double speedLimit, String type) {

        Map<Integer, LaneMovement> movementMap = new HashMap<Integer, LaneMovement>();
        for (int i = 0; i < movements.length; i++) {

            LaneMovement laneMovement = new LaneMovement();
            laneMovement.setMovementId(i);
            laneMovement.setMovement(movements[i]);
            movementMap.put(laneMovement.getMovementId(), laneMovement);
        }

        Lane lane = new Lane();
        lane.setIntersectionId(intersectionId);
        lane.setApproachId(approachId);
        lane.setLaneId(laneId);
        lane.setLaneGeomList(nodes);
        lane.setLaneMovements(movementMap);
        lane.setSpeedLimitInMetersPerSecond(speedLimit);
        lane.setType(type);

        return lane;
    }
}