/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.j2735_2016.messages;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.AdvisorySpeedType.Advisory;
import org.etexascode.j2735_2016.elements.AllowedManeuvers;
import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus.AntiLockBrake;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus.AuxiliaryBrake;
import org.etexascode.j2735_2016.elements.BrakeAppliedStatus;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied.BrakeBoost;
import org.etexascode.j2735_2016.elements.DSRCmsgID;
import org.etexascode.j2735_2016.elements.IntersectionStatusObject;
import org.etexascode.j2735_2016.elements.LaneAttributesBike;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LaneDirection;
import org.etexascode.j2735_2016.elements.LaneID;
import org.etexascode.j2735_2016.elements.LaneSharing;
import org.etexascode.j2735_2016.elements.LayerType.Layer;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.etexascode.j2735_2016.elements.NodeAttributeXY;
import org.etexascode.j2735_2016.elements.NodeAttributeXY.NodeAttribute;
import org.etexascode.j2735_2016.elements.RequestedItem;
import org.etexascode.j2735_2016.elements.RequestedItem.Item;
import org.etexascode.j2735_2016.elements.SegmentAttributeXY;
import org.etexascode.j2735_2016.elements.SegmentAttributeXY.SegmentAttribute;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.etexascode.j2735_2016.elements.StabilityControlStatus.StabilityControl;
import org.etexascode.j2735_2016.elements.TractionControlStatus.TractionControl;
import org.etexascode.j2735_2016.elements.TransmissionState.Transmission;
import org.etexascode.j2735_2016.frames.AccelerationSet4Way;
import org.etexascode.j2735_2016.frames.AdvisorySpeed;
import org.etexascode.j2735_2016.frames.BSMcoreData;
import org.etexascode.j2735_2016.frames.BrakeSystemStatus;
import org.etexascode.j2735_2016.frames.ComputedLane;
import org.etexascode.j2735_2016.frames.ConnectingLane;
import org.etexascode.j2735_2016.frames.Connection;
import org.etexascode.j2735_2016.frames.ConnectionManeuverAssist;
import org.etexascode.j2735_2016.frames.GenericLane;
import org.etexascode.j2735_2016.frames.IntersectionGeometry;
import org.etexascode.j2735_2016.frames.IntersectionReferenceID;
import org.etexascode.j2735_2016.frames.IntersectionState;
import org.etexascode.j2735_2016.frames.LaneAttributes;
import org.etexascode.j2735_2016.frames.LaneDataAttribute;
import org.etexascode.j2735_2016.frames.MovementEvent;
import org.etexascode.j2735_2016.frames.MovementState;
import org.etexascode.j2735_2016.frames.NodeAttributeSetXY;
import org.etexascode.j2735_2016.frames.NodeXY;
import org.etexascode.j2735_2016.frames.NodeXY20B;
import org.etexascode.j2735_2016.frames.Position3D;
import org.etexascode.j2735_2016.frames.PositionalAccuracy;
import org.etexascode.j2735_2016.frames.RegulatorySpeedLimit;
import org.etexascode.j2735_2016.frames.RoadSegment;
import org.etexascode.j2735_2016.frames.RoadSegmentReferenceID;
import org.etexascode.j2735_2016.frames.TimeChangeDetails;
import org.etexascode.j2735_2016.frames.VehicleSize;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the message frame.
 * 
 * @author ttevendale
 */
public class MessageFrameTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeHexUPER() {

        // small message
        String aByte = "00001111";
        // extension/msgId, messageSize, message
        String hexStr = "0014" + "01" + "0f";
        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, aByte);
        assertTrue(hexStr.equalsIgnoreCase(frame.encodeHexUPER()));

        // medium message
        String bytes128 = "";
        String hex128Str = "00158080";
        for (int i = 0; i < 128; i++) {

            bytes128 += aByte;
            hex128Str += "0f";
        }

        frame = new MessageFrame(DSRCmsgID.CSR, bytes128);
        assertTrue(hex128Str.equalsIgnoreCase(frame.encodeHexUPER()));

        // too large message
        String bytes2341 = "";
        for (int i = 0; i < 2341; i++) {

            bytes2341 += aByte;
        }

        frame = new MessageFrame(DSRCmsgID.SPAT, bytes2341);
        thrown.expect(IllegalStateException.class);
        frame.encodeHexUPER();
    }

    @Test
    public void testEncodeHexUPERPadding() {

        // small message that needs padding, should be 00001100 after padding is added
        String aByte = "000011";
        // extension/msgId, messageSize, message
        String hexStr = "0014" + "01" + "0c";
        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, aByte);
        assertTrue(hexStr.equalsIgnoreCase(frame.encodeHexUPER()));
    }

    @Test
    public void testGetDSRCMessageId() {

        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, "0010101011");
        String bytes = frame.encodeHexUPER();

        assertTrue(MessageFrame.getDSRCMessageId(bytes) == DSRCmsgID.BSM);

        frame = new MessageFrame(DSRCmsgID.MAP, "0011110101011");
        bytes = frame.encodeHexUPER();

        assertTrue(MessageFrame.getDSRCMessageId(bytes) == DSRCmsgID.MAP);

        int num = 121;
        frame = new MessageFrame(num, "001001010101011");
        bytes = frame.encodeHexUPER();

        assertTrue(MessageFrame.getDSRCMessageId(bytes) == num);
    }

    @Test
    public void testDecodeBSM1() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt BasicSafetyMessage 1 for source
        String bsmBytes = "0014251ffffffffff0005ad2748035a4e8ff8800021080322064012c92bb83e82f7c177b6c320960";

        BasicSafetyMessage bsm = MessageFrame.decodeBSM(bsmBytes);
        BSMcoreData coreData = bsm.getCoreData();
        assertTrue(coreData.getMessageCount().getValue() == 127);
        assertTrue(coreData.getId().getValue().equalsIgnoreCase("ffffffff"));
        assertTrue(coreData.getSecMark().getValue() == 49153);
        assertTrue(coreData.getLatitude().getValue() == 0);
        assertTrue(coreData.getLongitude().getValue() == 0);
        assertTrue(coreData.getElevation().getValue() == 0);

        PositionalAccuracy accuracy = coreData.getAccuracy();
        assertTrue(accuracy.getSemiMajor().getValue() == 4);
        assertTrue(accuracy.getSemiMinor().getValue() == 33);
        assertTrue(accuracy.getOrientation().getValue() == 100);

        assertTrue(coreData.getTransmission().getEnumeration().equals(Transmission.FORWARD_GEARS));
        assertTrue(coreData.getSpeed().getValue() == 200);
        assertTrue(coreData.getHeading().getValue() == 300);
        assertTrue(coreData.getAngle().getValue() == 20);

        AccelerationSet4Way accelerationSet = coreData.getAccelerationSet();
        assertTrue(accelerationSet.getLongitude().getValue() == 1000);
        assertTrue(accelerationSet.getLatitude().getValue() == -1000);
        assertTrue(accelerationSet.getVertical().getValue() == -80);
        assertTrue(accelerationSet.getYawRate().getValue() == -1000);

        BrakeSystemStatus brakes = coreData.getBrakes();
        BrakeAppliedStatus wheelBrakes = brakes.getWheelBrakes();
        assertTrue(wheelBrakes.isLeftFront());
        assertTrue(wheelBrakes.isLeftRear());
        assertTrue(wheelBrakes.isRightFront());
        assertTrue(wheelBrakes.isRightRear());

        assertTrue(brakes.getTraction().getEnumeration().equals(TractionControl.OFF));
        assertTrue(brakes.getAbs().getEnumeration().equals(AntiLockBrake.ON));
        assertTrue(brakes.getScs().getEnumeration().equals(StabilityControl.ENGAGED));
        assertTrue(brakes.getBrakeBoost().getEnumeration().equals(BrakeBoost.OFF));
        assertTrue(brakes.getAuxBrakes().getEnumeration().equals(AuxiliaryBrake.ON));

        VehicleSize size = coreData.getSize();
        assertTrue(size.getWidth().getValue() == 100);
        assertTrue(size.getLength().getValue() == 300);
    }

    @Test
    public void testDecodeBSM2() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt BasicSafetyMessage 2 for source
        String bsmBytes = "0014251ffffffffffffff5a4e900eb49d2007ffffffffffffffff080fdfa1fa1fefffe7ff7fffff8";

        BasicSafetyMessage bsm = MessageFrame.decodeBSM(bsmBytes);
        BSMcoreData coreData = bsm.getCoreData();
        assertTrue(coreData.getMessageCount().getValue() == 127);
        assertTrue(coreData.getId().getValue().equalsIgnoreCase("ffffffff"));
        assertTrue(coreData.getSecMark().getValue() == 65535);
        assertTrue(coreData.getLatitude().getValue() == 900000001);
        assertTrue(coreData.getLongitude().getValue() == 1800000001);
        assertTrue(coreData.getElevation().getValue() == 61439);

        PositionalAccuracy accuracy = coreData.getAccuracy();
        assertTrue(accuracy.getSemiMajor().getValue() == 255);
        assertTrue(accuracy.getSemiMinor().getValue() == 255);
        assertTrue(accuracy.getOrientation().getValue() == 65535);

        assertTrue(coreData.getTransmission().getEnumeration().equals(Transmission.UNAVAILABLE));
        assertTrue(coreData.getSpeed().getValue() == 8191);
        assertTrue(coreData.getHeading().getValue() == 28800);
        assertTrue(coreData.getAngle().getValue() == 127);

        AccelerationSet4Way accelerationSet = coreData.getAccelerationSet();
        assertTrue(accelerationSet.getLongitude().getValue() == 2001);
        assertTrue(accelerationSet.getLatitude().getValue() == 2001);
        assertTrue(accelerationSet.getVertical().getValue() == 127);
        assertTrue(accelerationSet.getYawRate().getValue() == 32767);

        BrakeSystemStatus brakes = coreData.getBrakes();
        BrakeAppliedStatus wheelBrakes = brakes.getWheelBrakes();
        assertTrue(wheelBrakes.isLeftFront());
        assertTrue(wheelBrakes.isLeftRear());
        assertTrue(wheelBrakes.isRightFront());
        assertTrue(wheelBrakes.isRightRear());

        assertTrue(brakes.getTraction().getEnumeration().equals(TractionControl.ENGAGED));
        assertTrue(brakes.getAbs().getEnumeration().equals(AntiLockBrake.ENGAGED));
        assertTrue(brakes.getScs().getEnumeration().equals(StabilityControl.ENGAGED));
        assertTrue(brakes.getBrakeBoost().getEnumeration().equals(BrakeBoost.ON));
        assertTrue(brakes.getAuxBrakes().getEnumeration().equals(AuxiliaryBrake.RESERVED));

        VehicleSize size = coreData.getSize();
        assertTrue(size.getWidth().getValue() == 1023);
        assertTrue(size.getLength().getValue() == 4095);
    }

    @Test
    public void testDecodeBSM3() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt BasicSafetyMessage 3 for source
        String bsmBytes = "00142500000000000000000000000000000000000000000000000000000000000000008000000000";

        BasicSafetyMessage bsm = MessageFrame.decodeBSM(bsmBytes);
        BSMcoreData coreData = bsm.getCoreData();
        assertTrue(coreData.getMessageCount().getValue() == 0);
        assertTrue(coreData.getId().getValue().equalsIgnoreCase("00000000"));
        assertTrue(coreData.getSecMark().getValue() == 0);
        assertTrue(coreData.getLatitude().getValue() == -900000000);
        assertTrue(coreData.getLongitude().getValue() == -1799999999);
        assertTrue(coreData.getElevation().getValue() == -4096);

        PositionalAccuracy accuracy = coreData.getAccuracy();
        assertTrue(accuracy.getSemiMajor().getValue() == 0);
        assertTrue(accuracy.getSemiMinor().getValue() == 0);
        assertTrue(accuracy.getOrientation().getValue() == 0);

        assertTrue(coreData.getTransmission().getEnumeration().equals(Transmission.NEUTRAL));
        assertTrue(coreData.getSpeed().getValue() == 0);
        assertTrue(coreData.getHeading().getValue() == 0);
        assertTrue(coreData.getAngle().getValue() == -126);

        AccelerationSet4Way accelerationSet = coreData.getAccelerationSet();
        assertTrue(accelerationSet.getLongitude().getValue() == -2000);
        assertTrue(accelerationSet.getLatitude().getValue() == -2000);
        assertTrue(accelerationSet.getVertical().getValue() == -127);
        assertTrue(accelerationSet.getYawRate().getValue() == -32767);

        BrakeSystemStatus brakes = coreData.getBrakes();
        BrakeAppliedStatus wheelBrakes = brakes.getWheelBrakes();
        assertFalse(wheelBrakes.isLeftFront());
        assertFalse(wheelBrakes.isLeftRear());
        assertFalse(wheelBrakes.isRightFront());
        assertFalse(wheelBrakes.isRightRear());

        assertTrue(brakes.getTraction().getEnumeration().equals(TractionControl.UNAVAILABLE));
        assertTrue(brakes.getAbs().getEnumeration().equals(AntiLockBrake.UNAVAILABLE));
        assertTrue(brakes.getScs().getEnumeration().equals(StabilityControl.UNAVAILABLE));
        assertTrue(brakes.getBrakeBoost().getEnumeration().equals(BrakeBoost.UNAVAILABLE));
        assertTrue(brakes.getAuxBrakes().getEnumeration().equals(AuxiliaryBrake.UNAVAILABLE));

        VehicleSize size = coreData.getSize();
        assertTrue(size.getWidth().getValue() == 0);
        assertTrue(size.getLength().getValue() == 0);
    }

    @Test
    public void testDecodeCSR1() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt CommonSafetyRequest 1 for source
        String csrBytes = "001503004730";

        CommonSafetyRequest csr = MessageFrame.decodeCSR(csrBytes);

        assertNull(csr.getTimeStamp());
        assertNull(csr.getMsgCount());
        assertNull(csr.getId());

        RequestedItem[] items = csr.getRequests().getRequestedItemArray();
        assertTrue(items.length == 2);
        assertTrue(items[0].getEnumeration().equals(Item.G));
        assertTrue(items[1].getEnumeration().equals(Item.M));
    }

    @Test
    public void testDecodeCSR2() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt CommonSafetyRequest 2 for source
        String csrBytes = "00150a73ffff8f0101010100e0";

        CommonSafetyRequest csr = MessageFrame.decodeCSR(csrBytes);

        assertTrue(csr.getTimeStamp().getValue() == 524287);
        assertTrue(csr.getMsgCount().getValue() == 15);
        assertTrue(csr.getId().getValue().equals("01010101"));

        RequestedItem[] items = csr.getRequests().getRequestedItemArray();
        assertTrue(items.length == 1);
        assertTrue(items[0].getEnumeration().equals(Item.G));
    }

    @Test
    public void testDecodeCSR3() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt CommonSafetyRequest 3 for source
        String csrBytes = "00150b73ffff8f0101010110e602";

        CommonSafetyRequest csr = MessageFrame.decodeCSR(csrBytes);

        assertTrue(csr.getTimeStamp().getValue() == 524287);
        assertTrue(csr.getMsgCount().getValue() == 15);
        assertTrue(csr.getId().getValue().equals("01010101"));

        RequestedItem[] items = csr.getRequests().getRequestedItemArray();
        assertTrue(items.length == 3);
        assertTrue(items[0].getEnumeration().equals(Item.G));
        assertTrue(items[1].getEnumeration().equals(Item.M));
        assertTrue(items[2].getEnumeration().equals(Item.A));
    }

    @Test
    public void testDecodeMapData1() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt MapData 1 for source
        String mapDataBytes = "001202000f";

        MapData mapData = MessageFrame.decodeMapData(mapDataBytes);

        assertNull(mapData.getTimeStamp());
        assertTrue(mapData.getMsgIssueRevision().getValue() == 15);
        assertNull(mapData.getLayerType());
        assertNull(mapData.getLayerId());
        assertNull(mapData.getIntersections());
        assertNull(mapData.getRoadSegments());
    }

    @Test
    public void testDecodeMapData2() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt MapData 2 for source
        String mapDataBytes = "00126e7c0003217164030001c4935a4e9646b49d213100503e8050fa001f00892700001800008852207e0b0340e00968160a05911007c0700000040a0228008060001204d693a569ad27482440c80c80141"
                + "90007c04425502f0008085c02fcf5f3800a8018020f81c04000040c0b200200";

        MapData mapData = MessageFrame.decodeMapData(mapDataBytes);

        assertTrue(mapData.getTimeStamp().getValue() == 100);
        assertTrue(mapData.getMsgIssueRevision().getValue() == 23);
        assertTrue(mapData.getLayerType().getEnumeration().equals(Layer.MIXED_CONTENT));
        assertTrue(mapData.getLayerId().getValue() == 50);

        IntersectionGeometry[] intersections = mapData.getIntersections().getIntersectionGeometryArray();
        assertTrue(intersections.length == 1);

        IntersectionGeometry intersection = intersections[0];

        IntersectionReferenceID id = intersection.getId();
        assertNull(id.getRegion());
        assertTrue(id.getId().getValue() == 14);

        assertTrue(intersection.getRevision().getValue() == 18);

        Position3D refPoint = intersection.getRefPoint();
        assertTrue(refPoint.getLatitude().getValue() == 100);
        assertTrue(refPoint.getLongitude().getValue() == 20);
        assertTrue(refPoint.getElevation().getValue() == 5);

        assertTrue(intersection.getLaneWidth().getValue() == 500);

        RegulatorySpeedLimit[] speedLimits = intersection.getSpeedLimits().getSpeedLimitArray();
        assertTrue(speedLimits.length == 1);

        RegulatorySpeedLimit speedLimit = speedLimits[0];
        assertTrue(speedLimit.getType().getEnumeration().equals(SpeedLimit.VEHICLE_MAX_SPEED));
        assertTrue(speedLimit.getSpeed().getValue() == 500);

        GenericLane[] lanes = intersection.getLaneSet().getLaneArray();
        assertTrue(lanes.length == 1);

        GenericLane lane = lanes[0];
        assertTrue(lane.getLaneId().getValue() == 1);
        assertTrue(lane.getIngressApproach().getValue() == 1);
        assertTrue(lane.getEgressApproach().getValue() == 2);

        LaneAttributes laneAttributes = lane.getLaneAttributes();

        LaneDirection laneDirection = laneAttributes.getDirectionalUse();
        assertTrue(laneDirection.isIngressPath());
        assertFalse(laneDirection.isEgressPath());

        LaneSharing LaneSharing = laneAttributes.getSharedWith();
        assertFalse(LaneSharing.isOverlappingLaneDescriptionProvided());
        assertTrue(LaneSharing.isMultipleLanesTreatedAsOneLane());
        assertTrue(LaneSharing.isOtherNonMotorizedTrafficTypes());
        assertTrue(LaneSharing.isIndividualMotorizedVehicleTraffic());
        assertFalse(LaneSharing.isBusVehicleTraffic());
        assertFalse(LaneSharing.isTaxiVehicleTraffic());
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
        assertTrue(maneuvers.isManeuverStraightAllowed());
        assertTrue(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertFalse(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());

        NodeXY[] nodes = lane.getNodeList().getNodes().getNodeArray();
        assertTrue(nodes.length == 2);

        NodeXY node = nodes[0];

        NodeXY20B delta = node.getDelta().getNodeXY20B();
        assertTrue(delta.getX().getValue() == 20);
        assertTrue(delta.getY().getValue() == 32);

        NodeAttributeSetXY nodeAttributes = node.getAttributes();

        NodeAttributeXY[] localNode = nodeAttributes.getLocalNode().getAttributeArray();
        assertTrue(localNode.length == 1);
        assertTrue(localNode[0].getEnumeration().equals(NodeAttribute.HYDRANT_PRESENT));

        SegmentAttributeXY[] disabled = nodeAttributes.getDisabled().getAttributeArray();
        assertTrue(disabled.length == 1);
        assertTrue(disabled[0].getEnumeration().equals(SegmentAttribute.ADJACENT_BIKE_LANE_ON_LEFT));

        SegmentAttributeXY[] enabled = nodeAttributes.getEnabled().getAttributeArray();
        assertTrue(enabled.length == 1);
        assertTrue(enabled[0].getEnumeration().equals(SegmentAttribute.ADJACENT_BIKE_LANE_ON_RIGHT));

        LaneDataAttribute[] data = nodeAttributes.getData().getAttributeArray();
        assertTrue(data.length == 1);
        assertTrue(data[0].getPathEndPointAngle().getValue() == 0);

        assertTrue(nodeAttributes.getDWidth().getValue() == 5);
        assertTrue(nodeAttributes.getDElevation().getValue() == 10);

        node = nodes[1];

        delta = node.getDelta().getNodeXY20B();
        assertTrue(delta.getX().getValue() == 200);
        assertTrue(delta.getY().getValue() == 32);

        assertNull(node.getAttributes());

        Connection[] connections = lane.getConnectsTo().getConnectionArray();
        assertTrue(connections.length == 1);

        Connection connection = connections[0];

        ConnectingLane connectingLane = connection.getConnectingLane();
        assertTrue(connectingLane.getLane().getValue() == 1);

        maneuvers = connectingLane.getManeuver();
        assertTrue(maneuvers.isManeuverStraightAllowed());
        assertTrue(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertFalse(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());

        IntersectionReferenceID remoteIntersectionId = connection.getRemoteIntersection();
        assertNull(remoteIntersectionId.getRegion());
        assertTrue(remoteIntersectionId.getId().getValue() == 2);

        assertTrue(connection.getSignalGroup().getValue() == 5);
        assertTrue(connection.getUserClass().getValue() == 1);
        assertTrue(connection.getConnectionId().getValue() == 20);

        LaneID[] overlays = lane.getOverlays().getLaneArray();
        assertTrue(overlays.length == 1);
        assertTrue(overlays[0].getValue() == 2);

        RoadSegment[] roadSegments = mapData.getRoadSegments().getRoadSegmentArray();
        assertTrue(roadSegments.length == 1);

        RoadSegment roadSegment = roadSegments[0];

        RoadSegmentReferenceID roadSegmentId = roadSegment.getId();
        assertNull(roadSegmentId.getRegion());
        assertTrue(roadSegmentId.getId().getValue() == 2);

        assertTrue(roadSegment.getRevision().getValue() == 32);

        refPoint = roadSegment.getRefPoint();
        assertTrue(refPoint.getLatitude().getValue() == 90);
        assertTrue(refPoint.getLongitude().getValue() == 10);
        assertTrue(refPoint.getElevation().getValue() == 50);

        assertTrue(roadSegment.getLaneWidth().getValue() == 400);

        speedLimits = roadSegment.getSpeedLimits().getSpeedLimitArray();
        assertTrue(speedLimits.length == 1);

        speedLimit = speedLimits[0];
        assertTrue(speedLimit.getType().getEnumeration().equals(SpeedLimit.VEHICLE_MAX_SPEED));
        assertTrue(speedLimit.getSpeed().getValue() == 200);

        lanes = roadSegment.getRoadLaneSet().getLaneArray();
        assertTrue(lanes.length == 1);

        lane = lanes[0];
        assertTrue(lane.getLaneId().getValue() == 2);
        assertTrue(lane.getIngressApproach().getValue() == 2);
        assertTrue(lane.getEgressApproach().getValue() == 1);

        laneAttributes = lane.getLaneAttributes();

        laneDirection = laneAttributes.getDirectionalUse();
        assertFalse(laneDirection.isIngressPath());
        assertTrue(laneDirection.isEgressPath());

        LaneSharing = laneAttributes.getSharedWith();
        assertFalse(LaneSharing.isOverlappingLaneDescriptionProvided());
        assertTrue(LaneSharing.isMultipleLanesTreatedAsOneLane());
        assertFalse(LaneSharing.isOtherNonMotorizedTrafficTypes());
        assertTrue(LaneSharing.isIndividualMotorizedVehicleTraffic());
        assertFalse(LaneSharing.isBusVehicleTraffic());
        assertTrue(LaneSharing.isTaxiVehicleTraffic());
        assertFalse(LaneSharing.isPedestriansTraffic());
        assertFalse(LaneSharing.isCyclistVehicleTraffic());
        assertFalse(LaneSharing.isTrackedVehicleTraffic());
        assertFalse(LaneSharing.isPedestrianTraffic());

        LaneAttributesBike bikeAttributes = laneAttributes.getLaneType().getBikeLane();
        assertTrue(bikeAttributes.isBikeRevocableLane());
        assertTrue(bikeAttributes.isPedestrianUseAllowed());
        assertTrue(bikeAttributes.isBikeFlyOverLane());
        assertTrue(bikeAttributes.isFixedCycleTime());
        assertFalse(bikeAttributes.isBiDirectionalCycleTimes());
        assertFalse(bikeAttributes.isIsolatedByBarrier());
        assertFalse(bikeAttributes.isUnsignalizedSegmentsPresent());

        maneuvers = lane.getManeuvers();
        assertTrue(maneuvers.isManeuverStraightAllowed());
        assertFalse(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertTrue(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());

        ComputedLane computedLane = lane.getNodeList().getComputed();
        assertTrue(computedLane.getReferenceLaneId().getValue() == 1);
        assertTrue(computedLane.getOffsetXAxis().getSmall().getValue() == 2000);
        assertTrue(computedLane.getOffsetYAxis().getSmall().getValue() == 1000);
        assertTrue(computedLane.getRotateXY().getValue() == 10);
        assertTrue(computedLane.getScaleXAxis().getValue() == 1);
        assertTrue(computedLane.getScaleYAxis().getValue() == 2);

        connections = lane.getConnectsTo().getConnectionArray();
        assertTrue(connections.length == 1);

        connection = connections[0];

        connectingLane = connection.getConnectingLane();
        assertTrue(connectingLane.getLane().getValue() == 3);

        maneuvers = connectingLane.getManeuver();
        assertTrue(maneuvers.isManeuverStraightAllowed());
        assertFalse(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertTrue(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());

        remoteIntersectionId = connection.getRemoteIntersection();
        assertNull(remoteIntersectionId.getRegion());
        assertTrue(remoteIntersectionId.getId().getValue() == 1);

        assertTrue(connection.getSignalGroup().getValue() == 3);
        assertTrue(connection.getUserClass().getValue() == 2);
        assertTrue(connection.getConnectionId().getValue() == 200);

        overlays = lane.getOverlays().getLaneArray();
        assertTrue(overlays.length == 1);
        assertTrue(overlays[0].getValue() == 4);
    }

    @Test
    public void testDecodeSPAT1() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt SignalPhaseAndTiming 1 for source
        String spatBytes = "00130b000000028ffcfc000ff000";

        SPAT spat = MessageFrame.decodeSPAT(spatBytes);

        assertNull(spat.getTimeStamp());

        IntersectionState[] intersections = spat.getIntersections().getIntersectionStateArray();
        assertTrue(intersections.length == 1);

        IntersectionState intersection = intersections[0];
        IntersectionReferenceID id = intersection.getId();
        assertNull(id.getRegion());
        assertTrue(id.getId().getValue() == 5);

        assertTrue(intersection.getRevision().getValue() == 15);

        IntersectionStatusObject status = intersection.getStatus();
        assertTrue(status.isManualControlEnabled());
        assertTrue(status.isStopTimeActivated());
        assertTrue(status.isFailureFlash());
        assertTrue(status.isPreemptActive());
        assertTrue(status.isSignalPriorityActive());
        assertTrue(status.isFixedTimeOperation());
        assertFalse(status.isTrafficDependentOperation());
        assertFalse(status.isStandbyOperation());
        assertTrue(status.isFailureMode());
        assertTrue(status.isOff());
        assertTrue(status.isRecentMapMessageUpdate());
        assertTrue(status.isRecentChangeInMapAssignedLaneIdsUsed());
        assertTrue(status.isNoValidMapAvailableAtThisTime());
        assertTrue(status.isNoValidSpatAvailableAtThisTime());

        assertNull(intersection.getMoy());
        assertNull(intersection.getTimeStamp());
        assertNull(intersection.getEnabledLanes());

        MovementState[] movements = intersection.getStates().getMovementArray();
        assertTrue(movements.length == 1);

        MovementState movement = movements[0];
        assertTrue(movement.getSignalGroup().getValue() == 255);

        MovementEvent[] events = movement.getStateTimeSpeed().getMovementEventArray();
        assertTrue(events.length == 1);

        MovementEvent event = events[0];
        assertTrue(event.getEventState().getEnumeration().equals(MovementPhase.UNAVAILABLE));
        assertNull(event.getTiming());
        assertNull(event.getSpeeds());

        assertNull(movement.getManeuverAssistList());

        assertNull(intersection.getManeuverAssistList());
    }

    @Test
    public void testDecodeSPAT2() {

        // check \\chimera\Products\eTEXAS\Research Material\SAE J2735 Documentation\SAE J2735 2016
        // - extra information\Example DSRC messages.txt SignalPhaseAndTiming 2 for source
        String spatBytes = "00133147ffff01e7fff80fffc00000ffff0ff002ff060f80028005000a00055001e03c1f400640507801fff0192078293880fa10";

        SPAT spat = MessageFrame.decodeSPAT(spatBytes);

        assertTrue(spat.getTimeStamp().getValue() == 524287);

        IntersectionState[] intersections = spat.getIntersections().getIntersectionStateArray();
        assertTrue(intersections.length == 1);

        IntersectionState intersection = intersections[0];
        IntersectionReferenceID id = intersection.getId();
        assertNull(id.getRegion());
        assertTrue(id.getId().getValue() == 65535);

        assertTrue(intersection.getRevision().getValue() == 0);

        IntersectionStatusObject status = intersection.getStatus();
        assertTrue(status.isManualControlEnabled());
        assertTrue(status.isStopTimeActivated());
        assertTrue(status.isFailureFlash());
        assertTrue(status.isPreemptActive());
        assertTrue(status.isSignalPriorityActive());
        assertTrue(status.isFixedTimeOperation());
        assertTrue(status.isTrafficDependentOperation());
        assertTrue(status.isStandbyOperation());
        assertTrue(status.isFailureMode());
        assertTrue(status.isOff());
        assertTrue(status.isRecentMapMessageUpdate());
        assertTrue(status.isRecentChangeInMapAssignedLaneIdsUsed());
        assertTrue(status.isNoValidMapAvailableAtThisTime());
        assertTrue(status.isNoValidSpatAvailableAtThisTime());

        assertTrue(intersection.getMoy().getValue() == 0);
        assertTrue(intersection.getTimeStamp().getValue() == 65535);

        LaneID[] enabledLanes = intersection.getEnabledLanes().getEnabledLaneArray();
        assertTrue(enabledLanes.length == 1);
        assertTrue(enabledLanes[0].getValue() == 255);

        MovementState[] movements = intersection.getStates().getMovementArray();
        assertTrue(movements.length == 1);

        MovementState movement = movements[0];
        assertTrue(movement.getSignalGroup().getValue() == 255);

        MovementEvent[] events = movement.getStateTimeSpeed().getMovementEventArray();
        assertTrue(events.length == 1);

        MovementEvent event = events[0];
        assertTrue(event.getEventState().getEnumeration().equals(MovementPhase.UNAVAILABLE));

        TimeChangeDetails timing = event.getTiming();
        assertTrue(timing.getStartTime().getValue() == 5);
        assertTrue(timing.getMinEndTime().getValue() == 10);
        assertTrue(timing.getMaxEndTime().getValue() == 20);
        assertTrue(timing.getLikelyTime().getValue() == 10);
        assertTrue(timing.getConfidence().getValue() == 10);
        assertTrue(timing.getNextTime().getValue() == 60);

        AdvisorySpeed[] speeds = event.getSpeeds().getAdvisorySpeedArray();
        assertTrue(speeds.length == 1);

        AdvisorySpeed speed = speeds[0];
        assertTrue(speed.getType().getEnumeration().equals(Advisory.NONE));
        assertTrue(speed.getSpeed().getValue() == 250);
        assertTrue(speed.getDistance().getValue() == 100);
        assertTrue(speed.getRestrictedClass().getValue() == 5);

        ConnectionManeuverAssist[] movementAssists = movement.getManeuverAssistList().getManeuverAssistArray();
        assertTrue(movementAssists.length == 1);

        ConnectionManeuverAssist movementAssist = movementAssists[0];
        assertTrue(movementAssist.getConnectionId().getValue() == 0);
        assertTrue(movementAssist.getQueueLength().getValue() == 8191);
        assertTrue(movementAssist.getAvailableStorageLength().getValue() == 100);
        assertTrue(movementAssist.getWaitOnStop().getValue());
        assertFalse(movementAssist.getPedBicycleDetect().getValue());

        ConnectionManeuverAssist[] intersectionAssists = intersection.getManeuverAssistList().getManeuverAssistArray();
        assertTrue(intersectionAssists.length == 1);

        ConnectionManeuverAssist intersectionAssist = intersectionAssists[0];
        assertTrue(intersectionAssist.getConnectionId().getValue() == 10);
        assertTrue(intersectionAssist.getQueueLength().getValue() == 5000);
        assertTrue(intersectionAssist.getAvailableStorageLength().getValue() == 1000);
        assertFalse(intersectionAssist.getWaitOnStop().getValue());
        assertTrue(intersectionAssist.getPedBicycleDetect().getValue());
    }

    @Test
    public void testDecodeMapDataLargeMessage() {

        String mapDataBytes = "00128341680000000300000e2009ad2748035a4e8ff8800119400921c0001000005f7c680b66710f05b06050280036d28022438000200000bef8d0448ce21e2240c0a050006da500648700005000017df1a"
                + "0e499c43c72418140a000db4601248e0000000006652535506050280036d2c94952370c02c91c000000000cd0026aa0c0a050006da59400a46e180692380000000019ab74d5418140a000db4b2add48dc500e6870000600"
                + "00164a486859994932aa18140a000db4a020d0e0000800002ca000d0b334006554302814001b694049a1c0001400005956e1a1666adccaa86050280036d180aa2380000000019c433e9418140a000db4b6c94fe94301744"
                + "7000000000338867770302814001b696d929f770603288e0000000006710ce3706050280036d2db253e371406ba1c0001000005083a7f4a60ef2fa506050280036d280e7438000200000a1074fbb8c1de5ddc0c0a050006"
                + "da501ee870000500001420e9f1b983bcb8dc18140a000db4604388e00000000061adccab06050280036d2b6b72dc90c08f11c000000000c30019560c0a050006da56c005b921812e238000000001854932ac18140a000db"
                + "4ad524b724502708700006000015b5bb97a986b74d5618140a000db4a05210e0000800002b60072f530c009aac302814001b6940ac21c00014000056a92e5ea6152535586050280036d1816f23800000000183bd416c181"
                + "40a000db4a936d016c302fe470000000003077a8890302814001b69526da08906063c8e00000000060ef51c906050280036d2a4db41c9140ca21c00018000030433d297f7a7a506050280036d281a443800020000060866"
                + "ee2fef4ddc0c0a050006da50368870000400000c10cc6e5fde98dc18140a000db4607308e0000000005fdea16c18140a000db4608685b0c0ee11c000000000bfbd4890302814001b68c10d224181ec2380000000017f7a9"
                + "c906050280036d1821a724503ea870000600000fef50b65821a16c18140a000db4a08150e0000800001fdea448b0434890302814001b69410aa1c0001000003fbd4e4960869c906050280036d1822d23800000000160867"
                + "a506050280036d1fde9e943047a470000000002c10cddc0c0a050006da3fbd3770609348e000000000582198dc18140a000db47f7a63700";
        // If no exception is thrown then the MapData message was properly decoded.
        MessageFrame.decodeMapData(mapDataBytes);
    }

    @Test
    public void testDecodeMapDataBadNumBytes() {

        String mapDataBytes = "00120341680000000300000e2009ad2748035a4e8ff8800119400921c0001000005f7c680b66710f05b06050280036d28022438000200000bef8d0448ce21e2240c0a050006da500648700005000017df1a"
                + "0e499c43c72418140a000db4601248e0000000006652535506050280036d2c94952370c02c91c000000000cd0026aa0c0a050006da59400a46e180692380000000019ab74d5418140a000db4b2add48dc500e6870000600"
                + "00164a486859994932aa18140a000db4a020d0e0000800002ca000d0b334006554302814001b694049a1c0001400005956e1a1666adccaa86050280036d180aa2380000000019c433e9418140a000db4b6c94fe94301744"
                + "7000000000338867770302814001b696d929f770603288e0000000006710ce3706050280036d2db253e371406ba1c0001000005083a7f4a60ef2fa506050280036d280e7438000200000a1074fbb8c1de5ddc0c0a050006"
                + "da501ee870000500001420e9f1b983bcb8dc18140a000db4604388e00000000061adccab06050280036d2b6b72dc90c08f11c000000000c30019560c0a050006da56c005b921812e238000000001854932ac18140a000db"
                + "4ad524b724502708700006000015b5bb97a986b74d5618140a000db4a05210e0000800002b60072f530c009aac302814001b6940ac21c00014000056a92e5ea6152535586050280036d1816f23800000000183bd416c181"
                + "40a000db4a936d016c302fe470000000003077a8890302814001b69526da08906063c8e00000000060ef51c906050280036d2a4db41c9140ca21c00018000030433d297f7a7a506050280036d281a443800020000060866"
                + "ee2fef4ddc0c0a050006da50368870000400000c10cc6e5fde98dc18140a000db4607308e0000000005fdea16c18140a000db4608685b0c0ee11c000000000bfbd4890302814001b68c10d224181ec2380000000017f7a9"
                + "c906050280036d1821a724503ea870000600000fef50b65821a16c18140a000db4a08150e0000800001fdea448b0434890302814001b69410aa1c0001000003fbd4e4960869c906050280036d1822d23800000000160867"
                + "a506050280036d1fde9e943047a470000000002c10cddc0c0a050006da3fbd3770609348e000000000582198dc18140a000db47f7a63700";
        thrown.expect(IllegalArgumentException.class);
        MessageFrame.decodeMapData(mapDataBytes);
    }

    @Test
    public void testDecodeSPATLargeMessage() {

        String spatBytes = "001380ae40008508800008080004e2012020045000068008228000340841140001a00408a0000d0030450000680382180014802010c000a40120860005200d043000188070218000c403c10c00062026086000"
                + "100140430000800a82180004006410c000200340860001001b0430000800f82300003407c1140001a20000402000138801c004100000000040800000000504000000003020000000024100000000140800000000d040000"
                + "000070200000000";
        // If no exception is thrown then the SPAT message was properly decoded.
        MessageFrame.decodeSPAT(spatBytes);
    }

    @Test
    public void testDecodeSPATBadNumBytes() {

        String spatBytes = "001300ae40008508800008080004e2012020045000068008228000340841140001a00408a0000d0030450000680382180014802010c000a40120860005200d043000188070218000c403c10c00062026086000"
                + "100140430000800a82180004006410c000200340860001001b0430000800f82300003407c1140001a20000402000138801c004100000000040800000000504000000003020000000024100000000140800000000d040000"
                + "000070200000000";
        thrown.expect(IllegalArgumentException.class);
        MessageFrame.decodeSPAT(spatBytes);
    }

    @Test
    public void testDecodeBSMTooManyBytes() {

        thrown.expect(IllegalStateException.class);
        MessageFrame.decodeBSM(createTooManyBytes());
    }

    @Test
    public void testDecodeCSRTooManyBytes() {

        thrown.expect(IllegalStateException.class);
        MessageFrame.decodeCSR(createTooManyBytes());
    }

    @Test
    public void testDecodeMapDataTooManyBytes() {

        thrown.expect(IllegalStateException.class);
        MessageFrame.decodeMapData(createTooManyBytes());
    }

    @Test
    public void testDecodeSPATTooManyBytes() {

        thrown.expect(IllegalStateException.class);
        MessageFrame.decodeSPAT(createTooManyBytes());
    }

    private String createTooManyBytes() {

        StringBuilder sb = new StringBuilder(2341);
        while (sb.length() < 2341 * 2) {

            sb.append('0');
        }
        return sb.toString();
    }
}
