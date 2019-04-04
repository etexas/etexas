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

import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.etexascode.j2735_2016.elements.LaneAttributesBike;
import org.etexascode.j2735_2016.elements.LaneAttributesTrackedVehicle;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LayerID;
import org.etexascode.j2735_2016.elements.LayerType;
import org.etexascode.j2735_2016.elements.LayerType.Layer;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.RestrictionAppliesTo.Restriction;
import org.etexascode.j2735_2016.frames.ComputedLane;
import org.etexascode.j2735_2016.frames.ComputedLaneOffsetXAxis;
import org.etexascode.j2735_2016.frames.ComputedLaneOffsetYAxis;
import org.etexascode.j2735_2016.frames.GenericLane;
import org.etexascode.j2735_2016.frames.IntersectionGeometry;
import org.etexascode.j2735_2016.frames.IntersectionGeometryList;
import org.etexascode.j2735_2016.frames.IntersectionReferenceID;
import org.etexascode.j2735_2016.frames.LaneAttributes;
import org.etexascode.j2735_2016.frames.LaneList;
import org.etexascode.j2735_2016.frames.LaneTypeAttributes;
import org.etexascode.j2735_2016.frames.NodeListXY;
import org.etexascode.j2735_2016.frames.Position3D;
import org.etexascode.j2735_2016.frames.RestrictionClassAssignment;
import org.etexascode.j2735_2016.frames.RestrictionClassList;
import org.etexascode.j2735_2016.frames.RestrictionUserType;
import org.etexascode.j2735_2016.frames.RestrictionUserTypeList;
import org.etexascode.j2735_2016.frames.RoadLaneSetList;
import org.etexascode.j2735_2016.frames.RoadSegment;
import org.etexascode.j2735_2016.frames.RoadSegmentList;
import org.etexascode.j2735_2016.frames.RoadSegmentReferenceID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the map data message.
 * 
 * @author ttevendale
 */
public class MapDataTest {

    MapData mapData;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void init() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(5154);
        MsgCount msgIssueRevision = new MsgCount(95);
        LayerType layerType = new LayerType(Layer.MIXED_CONTENT);
        LayerID layerId = new LayerID(3);

        IntersectionGeometryList intersections = new IntersectionGeometryList(1);

        LaneList laneSet = new LaneList(1);

        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesVehicle()));

        NodeListXY nodeList = new NodeListXY(new ComputedLane(0, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3888)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(38))));
        laneSet.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        intersections.getIntersectionGeometryArray()[0] = new IntersectionGeometry(new IntersectionReferenceID(23), new MsgCount(45), new Position3D(2343, 34234), laneSet);

        RoadSegmentList roadSegments = new RoadSegmentList(1);

        RoadLaneSetList roadLaneSet = new RoadLaneSetList(1);

        LaneAttributes attributes2 = new LaneAttributes();
        attributes2.setLaneType(new LaneTypeAttributes(new LaneAttributesBike()));

        NodeListXY nodeList2 = new NodeListXY(new ComputedLane(3, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(5678)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-50))));

        roadLaneSet.getLaneArray()[0] = new GenericLane(2, attributes2, nodeList2);

        roadSegments.getRoadSegmentArray()[0] = new RoadSegment(new RoadSegmentReferenceID(123), new MsgCount(49), new Position3D(123, 321), roadLaneSet);

        RestrictionClassList restrictionList = new RestrictionClassList(1);

        RestrictionUserTypeList users = new RestrictionUserTypeList(1);
        users.getRestrictionArray()[0] = new RestrictionUserType(Restriction.OTHER_UNKNOWN_DISABILITIES);

        restrictionList.getRestrictionArray()[0] = new RestrictionClassAssignment(88, users);

        mapData = new MapData(msgIssueRevision);
        mapData.setTimeStamp(timeStamp);
        mapData.setLayerType(layerType);
        mapData.setLayerId(layerId);
        mapData.setIntersections(intersections);
        mapData.setRoadSegments(roadSegments);
        mapData.setRestrictionList(restrictionList);
    }

    @Test
    public void testConstructor() {

        MsgCount msgIssueRevision = new MsgCount(15);

        MapData mapData = new MapData(msgIssueRevision);

        assertTrue(msgIssueRevision.equals(mapData.getMsgIssueRevision()));

        thrown.expect(NullPointerException.class);
        new MapData(null);
    }

    @Test
    public void testConstructorPrimitive() {

        int msgIssueRevision = 45;

        MapData mapData = new MapData(msgIssueRevision);

        assertTrue(msgIssueRevision == mapData.getMsgIssueRevision().getValue());
    }

    @Test
    public void testSetTimeStampPrimitive() {

        MapData mapData = new MapData();

        int timeStamp = 20;
        mapData.setTimeStamp(timeStamp);
        assertTrue(timeStamp == mapData.getTimeStamp().getValue());

        timeStamp = 15;
        mapData.setTimeStamp(timeStamp);
        assertTrue(timeStamp == mapData.getTimeStamp().getValue());
    }

    @Test
    public void testSetMsgCount() {

        MsgCount msgIssueRevision = new MsgCount(99);

        MapData mapData = new MapData();
        mapData.setMsgIssueRevision(msgIssueRevision);

        assertTrue(msgIssueRevision.equals(mapData.getMsgIssueRevision()));

        thrown.expect(NullPointerException.class);
        mapData.setMsgIssueRevision(null);
    }

    @Test
    public void testSetMsgCountPrimitive() {

        MapData mapData = new MapData();

        int msgCount = 10;
        mapData.setMsgIssueRevision(msgCount);
        assertTrue(msgCount == mapData.getMsgIssueRevision().getValue());

        msgCount = 111;
        mapData.setMsgIssueRevision(msgCount);
        assertTrue(msgCount == mapData.getMsgIssueRevision().getValue());
    }

    @Test
    public void testSetLayerTypePrimitive() {

        MapData mapData = new MapData();

        Layer layerType = Layer.NONE;
        mapData.setLayerType(layerType);
        assertTrue(layerType.equals(mapData.getLayerType().getEnumeration()));

        layerType = Layer.PARKING_AREA_DATA;
        mapData.setLayerType(layerType);
        assertTrue(layerType.equals(mapData.getLayerType().getEnumeration()));
    }

    @Test
    public void testSetLayerIdPrimitive() {

        MapData mapData = new MapData();

        int layerId = 1;
        mapData.setLayerId(layerId);
        assertTrue(layerId == mapData.getLayerId().getValue());

        layerId = 2;
        mapData.setLayerId(layerId);
        assertTrue(layerId == mapData.getLayerId().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        MsgCount msgIssueRevision = new MsgCount(77);

        MapData mapData = new MapData(msgIssueRevision);

        String mapDataOptionals = "000000000";
        String remainingBits = msgIssueRevision.encodeUPER();
        assertTrue((mapDataOptionals + remainingBits).equals(mapData.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(1234);
        MsgCount msgIssueRevision = new MsgCount(126);
        LayerType layerType = new LayerType(Layer.MIXED_CONTENT);
        LayerID layerId = new LayerID(10);

        IntersectionGeometryList intersections = new IntersectionGeometryList(1);

        LaneList laneSet = new LaneList(1);

        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesTrackedVehicle()));

        NodeListXY nodeList = new NodeListXY(new ComputedLane(10, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(123)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(4321))));
        laneSet.getLaneArray()[0] = new GenericLane(2, attributes, nodeList);

        intersections.getIntersectionGeometryArray()[0] = new IntersectionGeometry(new IntersectionReferenceID(1), new MsgCount(5), new Position3D(0, 35000), laneSet);

        RoadSegmentList roadSegments = new RoadSegmentList(1);

        RoadLaneSetList roadLaneSet = new RoadLaneSetList(1);

        LaneAttributes attributes2 = new LaneAttributes();
        attributes2.setLaneType(new LaneTypeAttributes(new LaneAttributesBike()));

        NodeListXY nodeList2 = new NodeListXY(new ComputedLane(3, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(5555)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-123))));

        roadLaneSet.getLaneArray()[0] = new GenericLane(1, attributes2, nodeList2);

        roadSegments.getRoadSegmentArray()[0] = new RoadSegment(new RoadSegmentReferenceID(10), new MsgCount(1), new Position3D(123, 321), roadLaneSet);

        RestrictionClassList restrictionList = new RestrictionClassList(1);

        RestrictionUserTypeList users = new RestrictionUserTypeList(1);
        users.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EMISSION_COMPLIANT);

        restrictionList.getRestrictionArray()[0] = new RestrictionClassAssignment(10, users);

        MapData mapData = new MapData(msgIssueRevision);
        mapData.setTimeStamp(timeStamp);
        mapData.setLayerType(layerType);
        mapData.setLayerId(layerId);
        mapData.setIntersections(intersections);
        mapData.setRoadSegments(roadSegments);
        mapData.setRestrictionList(restrictionList);

        String mapDataOptionals = "011111010";
        String remainingBits = timeStamp.encodeUPER() + msgIssueRevision.encodeUPER() + layerType.encodeUPER() + layerId.encodeUPER() + intersections.encodeUPER() + roadSegments.encodeUPER()
                + restrictionList.encodeUPER();
        assertTrue((mapDataOptionals + remainingBits).equals(mapData.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        MsgCount msgIssueRevision = new MsgCount(5);

        String mapDataOptionals = "000000000";

        MapData mapData = new MapData();
        String remainingBits = mapData.decodeUPER(mapDataOptionals + msgIssueRevision.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(mapData.getTimeStamp());
        assertNull(mapData.getLayerType());
        assertNull(mapData.getLayerId());
        assertNull(mapData.getIntersections());
        assertNull(mapData.getRoadSegments());
        assertNull(mapData.getRestrictionList());

        assertTrue(msgIssueRevision.equals(mapData.getMsgIssueRevision()));
    }

    @Test
    public void testDecodeUPERMax() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(1234);
        MsgCount msgIssueRevision = new MsgCount(126);
        LayerType layerType = new LayerType(Layer.MIXED_CONTENT);
        LayerID layerId = new LayerID(10);

        IntersectionGeometryList intersections = new IntersectionGeometryList(1);

        LaneList laneSet = new LaneList(1);

        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesTrackedVehicle()));

        NodeListXY nodeList = new NodeListXY(new ComputedLane(10, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(123)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(4321))));
        laneSet.getLaneArray()[0] = new GenericLane(2, attributes, nodeList);

        intersections.getIntersectionGeometryArray()[0] = new IntersectionGeometry(new IntersectionReferenceID(1), new MsgCount(5), new Position3D(0, 35000), laneSet);

        RoadSegmentList roadSegments = new RoadSegmentList(1);

        RoadLaneSetList roadLaneSet = new RoadLaneSetList(1);

        LaneAttributes attributes2 = new LaneAttributes();
        attributes2.setLaneType(new LaneTypeAttributes(new LaneAttributesBike()));

        NodeListXY nodeList2 = new NodeListXY(new ComputedLane(3, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(5555)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-123))));

        roadLaneSet.getLaneArray()[0] = new GenericLane(1, attributes2, nodeList2);

        roadSegments.getRoadSegmentArray()[0] = new RoadSegment(new RoadSegmentReferenceID(10), new MsgCount(1), new Position3D(123, 321), roadLaneSet);

        RestrictionClassList restrictionList = new RestrictionClassList(1);

        RestrictionUserTypeList users = new RestrictionUserTypeList(1);
        users.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EMISSION_COMPLIANT);

        restrictionList.getRestrictionArray()[0] = new RestrictionClassAssignment(10, users);

        String mapDataOptionals = "011111010";

        MapData mapData = new MapData();
        String remainingBits = mapData.decodeUPER(mapDataOptionals + timeStamp.encodeUPER() + msgIssueRevision.encodeUPER() + layerType.encodeUPER() + layerId.encodeUPER() + intersections.encodeUPER()
                + roadSegments.encodeUPER() + restrictionList.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(timeStamp.equals(mapData.getTimeStamp()));
        assertTrue(msgIssueRevision.equals(mapData.getMsgIssueRevision()));
        assertTrue(layerType.equals(mapData.getLayerType()));
        assertTrue(layerId.equals(mapData.getLayerId()));
        assertTrue(intersections.equals(mapData.getIntersections()));
        assertTrue(roadSegments.equals(mapData.getRoadSegments()));
        assertTrue(restrictionList.equals(mapData.getRestrictionList()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String mapDataOptionals = "100000000";

        MapData mapData = new MapData();
        thrown.expect(IllegalArgumentException.class);
        mapData.decodeUPER(mapDataOptionals);
    }

    @Test
    public void testDecodeUPERDataParameters() {

        String mapDataOptionals = "000000100";

        MapData mapData = new MapData();
        thrown.expect(IllegalArgumentException.class);
        mapData.decodeUPER(mapDataOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String mapDataOptionals = "000000001";

        MapData mapData = new MapData();
        thrown.expect(IllegalArgumentException.class);
        mapData.decodeUPER(mapDataOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String mapDataOptionals = "00001000";

        MapData mapData = new MapData();
        thrown.expect(IllegalArgumentException.class);
        mapData.decodeUPER(mapDataOptionals);
    }

    @Test
    public void testHashCode() {

        int timeStamp = mapData.getTimeStamp().getValue();
        int msgIssueRevision = mapData.getMsgIssueRevision().getValue();
        Layer layerType = mapData.getLayerType().getEnumeration();
        int layerId = mapData.getLayerId().getValue();
        IntersectionGeometryList intersections = mapData.getIntersections();
        RoadSegmentList roadSegments = mapData.getRoadSegments();
        RestrictionClassList restrictionList = mapData.getRestrictionList();

        IntersectionGeometryList diffIntersections = new IntersectionGeometryList(intersections.getIntersectionGeometryArray().length + 1);
        RoadSegmentList diffRoadSegments = new RoadSegmentList(roadSegments.getRoadSegmentArray().length + 1);
        RestrictionClassList diffRestrictionList = new RestrictionClassList(restrictionList.getRestrictionArray().length + 1);

        MapData mapData2 = new MapData(msgIssueRevision + 1);
        mapData2.setTimeStamp(timeStamp + 1);
        mapData2.setLayerType(Layer.CURVE_DATA);
        mapData2.setLayerId(layerId + 1);
        mapData2.setIntersections(diffIntersections);
        mapData2.setRoadSegments(diffRoadSegments);
        mapData2.setRestrictionList(diffRestrictionList);

        assertFalse(mapData.hashCode() == mapData2.hashCode());
        assertTrue(mapData.hashCode() == mapData.hashCode());
        assertTrue(mapData2.hashCode() == mapData2.hashCode());

        MapData mapData3 = new MapData(msgIssueRevision);
        mapData3.setTimeStamp(timeStamp);
        mapData3.setLayerType(layerType);
        mapData3.setLayerId(layerId);
        mapData3.setIntersections(intersections);
        mapData3.setRoadSegments(roadSegments);
        mapData3.setRestrictionList(restrictionList);

        assertTrue(mapData.hashCode() == mapData3.hashCode());
        assertFalse(mapData2.hashCode() == mapData3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(mapData.equals(mapData));
        assertFalse(mapData.equals(null));
        assertFalse(mapData.equals(new String()));

        int timeStamp = mapData.getTimeStamp().getValue();
        int msgIssueRevision = mapData.getMsgIssueRevision().getValue();
        Layer layerType = mapData.getLayerType().getEnumeration();
        int layerId = mapData.getLayerId().getValue();
        IntersectionGeometryList intersections = mapData.getIntersections();
        RoadSegmentList roadSegments = mapData.getRoadSegments();
        RestrictionClassList restrictionList = mapData.getRestrictionList();

        IntersectionGeometryList diffIntersections = new IntersectionGeometryList(intersections.getIntersectionGeometryArray().length + 1);
        RoadSegmentList diffRoadSegments = new RoadSegmentList(roadSegments.getRoadSegmentArray().length + 1);
        RestrictionClassList diffRestrictionList = new RestrictionClassList(restrictionList.getRestrictionArray().length + 1);

        // different
        MapData mapData2 = new MapData(msgIssueRevision + 1);
        mapData2.setTimeStamp(timeStamp + 1);
        mapData2.setLayerType(Layer.CURVE_DATA);
        mapData2.setLayerId(layerId + 1);
        mapData2.setIntersections(diffIntersections);
        mapData2.setRoadSegments(diffRoadSegments);
        mapData2.setRestrictionList(diffRestrictionList);

        assertFalse(mapData.equals(mapData2));

        // different message issue revision
        mapData2 = new MapData(msgIssueRevision + 1);
        mapData2.setTimeStamp(timeStamp);
        mapData2.setLayerType(layerType);
        mapData2.setLayerId(layerId);
        mapData2.setIntersections(intersections);
        mapData2.setRoadSegments(roadSegments);
        mapData2.setRestrictionList(restrictionList);

        assertFalse(mapData.equals(mapData2));

        // different time stamp
        mapData2 = new MapData(msgIssueRevision);
        mapData2.setTimeStamp(timeStamp + 1);
        mapData2.setLayerType(layerType);
        mapData2.setLayerId(layerId);
        mapData2.setIntersections(intersections);
        mapData2.setRoadSegments(roadSegments);
        mapData2.setRestrictionList(restrictionList);

        assertFalse(mapData.equals(mapData2));

        // different layer type
        mapData2 = new MapData(msgIssueRevision);
        mapData2.setTimeStamp(timeStamp);
        mapData2.setLayerType(Layer.CURVE_DATA);
        mapData2.setLayerId(layerId);
        mapData2.setIntersections(intersections);
        mapData2.setRoadSegments(roadSegments);
        mapData2.setRestrictionList(restrictionList);

        assertFalse(mapData.equals(mapData2));

        // different layer ID
        mapData2 = new MapData(msgIssueRevision);
        mapData2.setTimeStamp(timeStamp);
        mapData2.setLayerType(layerType);
        mapData2.setLayerId(layerId + 1);
        mapData2.setIntersections(intersections);
        mapData2.setRoadSegments(roadSegments);
        mapData2.setRestrictionList(restrictionList);

        assertFalse(mapData.equals(mapData2));

        // different intersections
        mapData2 = new MapData(msgIssueRevision);
        mapData2.setTimeStamp(timeStamp);
        mapData2.setLayerType(layerType);
        mapData2.setLayerId(layerId);
        mapData2.setIntersections(diffIntersections);
        mapData2.setRoadSegments(roadSegments);
        mapData2.setRestrictionList(restrictionList);

        assertFalse(mapData.equals(mapData2));

        // different road segments
        mapData2 = new MapData(msgIssueRevision);
        mapData2.setTimeStamp(timeStamp);
        mapData2.setLayerType(layerType);
        mapData2.setLayerId(layerId);
        mapData2.setIntersections(intersections);
        mapData2.setRoadSegments(diffRoadSegments);
        mapData2.setRestrictionList(restrictionList);

        assertFalse(mapData.equals(mapData2));

        // different restriction list
        mapData2 = new MapData(msgIssueRevision);
        mapData2.setTimeStamp(timeStamp);
        mapData2.setLayerType(layerType);
        mapData2.setLayerId(layerId);
        mapData2.setIntersections(intersections);
        mapData2.setRoadSegments(roadSegments);
        mapData2.setRestrictionList(diffRestrictionList);

        assertFalse(mapData.equals(mapData2));

        // same
        mapData2 = new MapData(msgIssueRevision);
        mapData2.setTimeStamp(timeStamp);
        mapData2.setLayerType(layerType);
        mapData2.setLayerId(layerId);
        mapData2.setIntersections(intersections);
        mapData2.setRoadSegments(roadSegments);
        mapData2.setRestrictionList(restrictionList);

        assertTrue(mapData.equals(mapData2));
    }
}
