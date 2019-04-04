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
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LaneWidth;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the road segment frame.
 * 
 * @author ttevendale
 */
public class RoadSegmentTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    RoadSegmentReferenceID id;

    MsgCount revision;

    Position3D refPoint;

    LaneWidth laneWidth;

    SpeedLimitList speedLimits;

    RoadLaneSetList laneSet;

    @Before
    public void init() {

        id = new RoadSegmentReferenceID(1);
        revision = new MsgCount(120);
        refPoint = new Position3D(1232, 43234);
        laneWidth = new LaneWidth(30);
        speedLimits = new SpeedLimitList(1);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.VEHICLE_MAX_SPEED, 50);
        laneSet = new RoadLaneSetList(1);
        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesVehicle()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(0, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3888)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(38))));
        laneSet.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);
    }

    @Test
    public void testConstructor() {

        RoadSegment roadSegment = new RoadSegment(id, revision, refPoint, laneSet);

        assertNull(roadSegment.getLaneWidth());
        assertNull(roadSegment.getSpeedLimits());
        assertTrue(id.equals(roadSegment.getId()));
        assertTrue(revision.equals(roadSegment.getRevision()));
        assertTrue(refPoint.equals(roadSegment.getRefPoint()));
        assertTrue(laneSet.equals(roadSegment.getRoadLaneSet()));
    }

    @Test
    public void testConstructorNullId() {

        thrown.expect(NullPointerException.class);
        new RoadSegment(null, new MsgCount(), new Position3D(), new RoadLaneSetList());
    }

    @Test
    public void testConstructorNullRevision() {

        thrown.expect(NullPointerException.class);
        new RoadSegment(new RoadSegmentReferenceID(), null, new Position3D(), new RoadLaneSetList());
    }

    @Test
    public void testConstructorNullRefPoint() {

        thrown.expect(NullPointerException.class);
        new RoadSegment(new RoadSegmentReferenceID(), new MsgCount(), null, new RoadLaneSetList());
    }

    @Test
    public void testConstructorNullRoadLaneSet() {

        thrown.expect(NullPointerException.class);
        new RoadSegment(new RoadSegmentReferenceID(), new MsgCount(), new Position3D(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int revision = 11;

        RoadSegment roadSegment = new RoadSegment(id, revision, refPoint, laneSet);

        assertNull(roadSegment.getLaneWidth());
        assertNull(roadSegment.getSpeedLimits());
        assertTrue(id.equals(roadSegment.getId()));
        assertTrue(revision == roadSegment.getRevision().getValue());
        assertTrue(refPoint.equals(roadSegment.getRefPoint()));
        assertTrue(laneSet.equals(roadSegment.getRoadLaneSet()));
    }

    @Test
    public void testConstructorPrimitiveNullId() {

        thrown.expect(NullPointerException.class);
        new RoadSegment(null, 0, new Position3D(), new RoadLaneSetList());
    }

    @Test
    public void testConstructorPrimitiveNullRefPoint() {

        thrown.expect(NullPointerException.class);
        new RoadSegment(new RoadSegmentReferenceID(), 0, null, new RoadLaneSetList());
    }

    @Test
    public void testConstructorPrimitiveNullRoadLaneSet() {

        thrown.expect(NullPointerException.class);
        new RoadSegment(new RoadSegmentReferenceID(), 0, new Position3D(), null);
    }

    @Test
    public void testSetId() {

        RoadSegment roadSegment = new RoadSegment();
        roadSegment.setId(id);

        assertTrue(id.equals(roadSegment.getId()));

        thrown.expect(NullPointerException.class);
        roadSegment.setId(null);
    }

    @Test
    public void testSetRevision() {

        RoadSegment roadSegment = new RoadSegment();
        roadSegment.setRevision(revision);

        assertTrue(revision.equals(roadSegment.getRevision()));

        thrown.expect(NullPointerException.class);
        roadSegment.setRevision(null);
    }

    @Test
    public void testSetRevisionPrimitive() {

        int revision = 83;

        RoadSegment roadSegment = new RoadSegment();
        roadSegment.setRevision(revision);

        assertTrue(revision == roadSegment.getRevision().getValue());
    }

    @Test
    public void testSetRefPoint() {

        RoadSegment roadSegment = new RoadSegment();
        roadSegment.setRefPoint(refPoint);

        assertTrue(refPoint.equals(roadSegment.getRefPoint()));

        thrown.expect(NullPointerException.class);
        roadSegment.setRefPoint(null);
    }

    @Test
    public void testSetLaneWidthPrimitive() {

        int laneWidth = 202;

        RoadSegment roadSegment = new RoadSegment();
        roadSegment.setLaneWidth(laneWidth);

        assertTrue(laneWidth == roadSegment.getLaneWidth().getValue());

        laneWidth = 19;

        roadSegment.setLaneWidth(laneWidth);

        assertTrue(laneWidth == roadSegment.getLaneWidth().getValue());
    }

    @Test
    public void testSetRoadLaneSet() {

        RoadSegment roadSegment = new RoadSegment();
        roadSegment.setRoadLaneSet(laneSet);

        assertTrue(laneSet.equals(roadSegment.getRoadLaneSet()));

        thrown.expect(NullPointerException.class);
        roadSegment.setRoadLaneSet(null);
    }

    @Test
    public void testEncodeUPERMin() {

        RoadSegment roadSegment = new RoadSegment(id, revision, refPoint, laneSet);

        String roadSegmentOptionals = "00000";
        String remainingBits = id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneSet.encodeUPER();
        assertTrue((roadSegmentOptionals + remainingBits).equals(roadSegment.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        RoadSegment roadSegment = new RoadSegment(id, revision, refPoint, laneSet);
        roadSegment.setLaneWidth(laneWidth);
        roadSegment.setSpeedLimits(speedLimits);

        String roadSegmentOptionals = "00110";
        String remainingBits = id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneWidth.encodeUPER() + speedLimits.encodeUPER() + laneSet.encodeUPER();
        assertTrue((roadSegmentOptionals + remainingBits).equals(roadSegment.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        String roadSegmentOptionals = "00000";

        RoadSegment roadSegment = new RoadSegment();
        String remainingBits = roadSegment.decodeUPER(roadSegmentOptionals + id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneSet.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(roadSegment.getLaneWidth());
        assertNull(roadSegment.getSpeedLimits());
        assertTrue(id.equals(roadSegment.getId()));
        assertTrue(revision.equals(roadSegment.getRevision()));
        assertTrue(refPoint.equals(roadSegment.getRefPoint()));
        assertTrue(laneSet.equals(roadSegment.getRoadLaneSet()));
    }

    @Test
    public void testDecodeUPERMax() {

        String roadSegmentOptionals = "00110";

        RoadSegment roadSegment = new RoadSegment();
        String remainingBits = roadSegment
                .decodeUPER(roadSegmentOptionals + id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneWidth.encodeUPER() + speedLimits.encodeUPER() + laneSet.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(id.equals(roadSegment.getId()));
        assertTrue(revision.equals(roadSegment.getRevision()));
        assertTrue(refPoint.equals(roadSegment.getRefPoint()));
        assertTrue(laneWidth.equals(roadSegment.getLaneWidth()));
        assertTrue(speedLimits.equals(roadSegment.getSpeedLimits()));
        assertTrue(laneSet.equals(roadSegment.getRoadLaneSet()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String roadSegmentOptionals = "10000";

        RoadSegment roadSegment = new RoadSegment();
        thrown.expect(IllegalArgumentException.class);
        roadSegment.decodeUPER(roadSegmentOptionals);
    }

    @Test
    public void testDecodeUPERDescriptiveName() {

        String roadSegmentOptionals = "01000";

        RoadSegment roadSegment = new RoadSegment();
        thrown.expect(IllegalArgumentException.class);
        roadSegment.decodeUPER(roadSegmentOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String roadSegmentOptionals = "00001";

        RoadSegment roadSegment = new RoadSegment();
        thrown.expect(IllegalArgumentException.class);
        roadSegment.decodeUPER(roadSegmentOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String roadSegmentOptionals = "0000";

        RoadSegment roadSegment = new RoadSegment();
        thrown.expect(IllegalArgumentException.class);
        roadSegment.decodeUPER(roadSegmentOptionals);
    }

    @Test
    public void testHashCode() {

        RoadSegment roadSegment = new RoadSegment(id, revision, refPoint, laneSet);
        roadSegment.setLaneWidth(laneWidth);
        roadSegment.setSpeedLimits(speedLimits);

        int revision = roadSegment.getRevision().getValue();
        int laneWidth = roadSegment.getLaneWidth().getValue();

        RoadSegmentReferenceID diffId = new RoadSegmentReferenceID(id.getId().getValue() + 1);
        Position3D diffRefPoint = new Position3D(refPoint.getLatitude().getValue() + 1, refPoint.getLongitude().getValue() + 1);
        RoadLaneSetList diffRoadLaneSet = new RoadLaneSetList(laneSet.getLaneArray().length + 1);
        SpeedLimitList diffSpeedLimits = new SpeedLimitList(speedLimits.getSpeedLimitArray().length + 1);

        RoadSegment roadSegment2 = new RoadSegment(diffId, revision + 1, diffRefPoint, diffRoadLaneSet);
        roadSegment2.setLaneWidth(laneWidth + 1);
        roadSegment2.setSpeedLimits(diffSpeedLimits);

        assertFalse(roadSegment.hashCode() == roadSegment2.hashCode());
        assertTrue(roadSegment.hashCode() == roadSegment.hashCode());
        assertTrue(roadSegment2.hashCode() == roadSegment2.hashCode());

        RoadSegment roadSegment3 = new RoadSegment(id, revision, refPoint, laneSet);
        roadSegment3.setLaneWidth(laneWidth);
        roadSegment3.setSpeedLimits(speedLimits);

        assertTrue(roadSegment.hashCode() == roadSegment3.hashCode());
        assertFalse(roadSegment2.hashCode() == roadSegment3.hashCode());
    }

    @Test
    public void testEquals() {

        RoadSegment roadSegment = new RoadSegment(id, revision, refPoint, laneSet);
        roadSegment.setLaneWidth(laneWidth);
        roadSegment.setSpeedLimits(speedLimits);

        assertTrue(roadSegment.equals(roadSegment));
        assertFalse(roadSegment.equals(null));
        assertFalse(roadSegment.equals(new String()));

        int revision = roadSegment.getRevision().getValue();
        int laneWidth = roadSegment.getLaneWidth().getValue();

        RoadSegmentReferenceID diffId = new RoadSegmentReferenceID(id.getId().getValue() + 1);
        Position3D diffRefPoint = new Position3D(refPoint.getLatitude().getValue() + 1, refPoint.getLongitude().getValue() + 1);
        RoadLaneSetList diffRoadLaneSet = new RoadLaneSetList(laneSet.getLaneArray().length + 1);
        SpeedLimitList diffSpeedLimits = new SpeedLimitList(speedLimits.getSpeedLimitArray().length + 1);

        // different
        RoadSegment roadSegment2 = new RoadSegment(diffId, revision + 1, diffRefPoint, diffRoadLaneSet);
        roadSegment2.setLaneWidth(laneWidth + 1);
        roadSegment2.setSpeedLimits(diffSpeedLimits);

        assertFalse(roadSegment.equals(roadSegment2));

        // different id
        roadSegment2 = new RoadSegment(diffId, revision, refPoint, laneSet);
        roadSegment2.setLaneWidth(laneWidth);
        roadSegment2.setSpeedLimits(speedLimits);

        assertFalse(roadSegment.equals(roadSegment2));

        // different revision
        roadSegment2 = new RoadSegment(id, revision + 1, refPoint, laneSet);
        roadSegment2.setLaneWidth(laneWidth);
        roadSegment2.setSpeedLimits(speedLimits);

        assertFalse(roadSegment.equals(roadSegment2));

        // different ref point
        roadSegment2 = new RoadSegment(id, revision, diffRefPoint, laneSet);
        roadSegment2.setLaneWidth(laneWidth);
        roadSegment2.setSpeedLimits(speedLimits);

        assertFalse(roadSegment.equals(roadSegment2));

        // different road lane set
        roadSegment2 = new RoadSegment(id, revision, refPoint, diffRoadLaneSet);
        roadSegment2.setLaneWidth(laneWidth);
        roadSegment2.setSpeedLimits(speedLimits);

        assertFalse(roadSegment.equals(roadSegment2));

        // different lane width
        roadSegment2 = new RoadSegment(id, revision, refPoint, laneSet);
        roadSegment2.setLaneWidth(laneWidth + 1);
        roadSegment2.setSpeedLimits(speedLimits);

        assertFalse(roadSegment.equals(roadSegment2));

        // different speed limits
        roadSegment2 = new RoadSegment(id, revision, refPoint, laneSet);
        roadSegment2.setLaneWidth(laneWidth);
        roadSegment2.setSpeedLimits(diffSpeedLimits);

        assertFalse(roadSegment.equals(roadSegment2));

        // same
        roadSegment2 = new RoadSegment(id, revision, refPoint, laneSet);
        roadSegment2.setLaneWidth(laneWidth);
        roadSegment2.setSpeedLimits(speedLimits);

        assertTrue(roadSegment.equals(roadSegment2));
    }
}
