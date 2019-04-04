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
 * Unit tests for the intersection geometry frame.
 * 
 * @author ttevendale
 */
public class IntersectionGeometryTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    IntersectionReferenceID id;

    MsgCount revision;

    Position3D refPoint;

    LaneWidth laneWidth;

    SpeedLimitList speedLimits;

    LaneList laneSet;

    @Before
    public void init() {

        id = new IntersectionReferenceID(1);
        revision = new MsgCount(120);
        refPoint = new Position3D(1232, 43234);
        laneWidth = new LaneWidth(30);
        speedLimits = new SpeedLimitList(1);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.VEHICLE_MAX_SPEED, 50);
        laneSet = new LaneList(1);
        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesVehicle()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(0, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3888)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(38))));
        laneSet.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);
    }

    @Test
    public void testConstructor() {

        IntersectionGeometry intersection = new IntersectionGeometry(id, revision, refPoint, laneSet);

        assertNull(intersection.getLaneWidth());
        assertNull(intersection.getSpeedLimits());
        assertTrue(id.equals(intersection.getId()));
        assertTrue(revision.equals(intersection.getRevision()));
        assertTrue(refPoint.equals(intersection.getRefPoint()));
        assertTrue(laneSet.equals(intersection.getLaneSet()));
    }

    @Test
    public void testConstructorNullId() {

        thrown.expect(NullPointerException.class);
        new IntersectionGeometry(null, new MsgCount(), new Position3D(), new LaneList());
    }

    @Test
    public void testConstructorNullRevision() {

        thrown.expect(NullPointerException.class);
        new IntersectionGeometry(new IntersectionReferenceID(), null, new Position3D(), new LaneList());
    }

    @Test
    public void testConstructorNullRefPoint() {

        thrown.expect(NullPointerException.class);
        new IntersectionGeometry(new IntersectionReferenceID(), new MsgCount(), null, new LaneList());
    }

    @Test
    public void testConstructorNullLaneSet() {

        thrown.expect(NullPointerException.class);
        new IntersectionGeometry(new IntersectionReferenceID(), new MsgCount(), new Position3D(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int revision = 11;

        IntersectionGeometry intersection = new IntersectionGeometry(id, revision, refPoint, laneSet);

        assertNull(intersection.getLaneWidth());
        assertNull(intersection.getSpeedLimits());
        assertTrue(id.equals(intersection.getId()));
        assertTrue(revision == intersection.getRevision().getValue());
        assertTrue(refPoint.equals(intersection.getRefPoint()));
        assertTrue(laneSet.equals(intersection.getLaneSet()));
    }

    @Test
    public void testConstructorPrimitiveNullId() {

        thrown.expect(NullPointerException.class);
        new IntersectionGeometry(null, 0, new Position3D(), new LaneList());
    }

    @Test
    public void testConstructorPrimitiveNullRefPoint() {

        thrown.expect(NullPointerException.class);
        new IntersectionGeometry(new IntersectionReferenceID(), 0, null, new LaneList());
    }

    @Test
    public void testConstructorPrimitiveNullLaneSet() {

        thrown.expect(NullPointerException.class);
        new IntersectionGeometry(new IntersectionReferenceID(), 0, new Position3D(), null);
    }

    @Test
    public void testSetId() {

        IntersectionGeometry intersection = new IntersectionGeometry();
        intersection.setId(id);

        assertTrue(id.equals(intersection.getId()));

        thrown.expect(NullPointerException.class);
        intersection.setId(null);
    }

    @Test
    public void testSetRevision() {

        IntersectionGeometry intersection = new IntersectionGeometry();
        intersection.setRevision(revision);

        assertTrue(revision.equals(intersection.getRevision()));

        thrown.expect(NullPointerException.class);
        intersection.setRevision(null);
    }

    @Test
    public void testSetRevisionPrimitive() {

        int revision = 83;

        IntersectionGeometry intersection = new IntersectionGeometry();
        intersection.setRevision(revision);

        assertTrue(revision == intersection.getRevision().getValue());
    }

    @Test
    public void testSetRefPoint() {

        IntersectionGeometry intersection = new IntersectionGeometry();
        intersection.setRefPoint(refPoint);

        assertTrue(refPoint.equals(intersection.getRefPoint()));

        thrown.expect(NullPointerException.class);
        intersection.setRefPoint(null);
    }

    @Test
    public void testSetLaneWidthPrimitive() {

        int laneWidth = 202;

        IntersectionGeometry intersection = new IntersectionGeometry();
        intersection.setLaneWidth(laneWidth);

        assertTrue(laneWidth == intersection.getLaneWidth().getValue());

        laneWidth = 19;

        intersection.setLaneWidth(laneWidth);

        assertTrue(laneWidth == intersection.getLaneWidth().getValue());
    }

    @Test
    public void testSetLaneSet() {

        IntersectionGeometry intersection = new IntersectionGeometry();
        intersection.setLaneSet(laneSet);

        assertTrue(laneSet.equals(intersection.getLaneSet()));

        thrown.expect(NullPointerException.class);
        intersection.setLaneSet(null);
    }

    @Test
    public void testEncodeUPERMin() {

        IntersectionGeometry intersection = new IntersectionGeometry(id, revision, refPoint, laneSet);

        String intersectionGeometryOptionals = "000000";
        String remainingBits = id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneSet.encodeUPER();
        assertTrue((intersectionGeometryOptionals + remainingBits).equals(intersection.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        IntersectionGeometry intersection = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection.setLaneWidth(laneWidth);
        intersection.setSpeedLimits(speedLimits);

        String intersectionGeometryOptionals = "001100";
        String remainingBits = id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneWidth.encodeUPER() + speedLimits.encodeUPER() + laneSet.encodeUPER();
        assertTrue((intersectionGeometryOptionals + remainingBits).equals(intersection.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        String intersectionGeometryOptionals = "000000";

        IntersectionGeometry intersection = new IntersectionGeometry();
        String remainingBits = intersection.decodeUPER(intersectionGeometryOptionals + id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneSet.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(intersection.getLaneWidth());
        assertNull(intersection.getSpeedLimits());
        assertTrue(id.equals(intersection.getId()));
        assertTrue(revision.equals(intersection.getRevision()));
        assertTrue(refPoint.equals(intersection.getRefPoint()));
        assertTrue(laneSet.equals(intersection.getLaneSet()));
    }

    @Test
    public void testDecodeUPERMax() {

        String intersectionGeometryOptionals = "001100";

        IntersectionGeometry intersection = new IntersectionGeometry();
        String remainingBits = intersection
                .decodeUPER(intersectionGeometryOptionals + id.encodeUPER() + revision.encodeUPER() + refPoint.encodeUPER() + laneWidth.encodeUPER() + speedLimits.encodeUPER() + laneSet.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(id.equals(intersection.getId()));
        assertTrue(revision.equals(intersection.getRevision()));
        assertTrue(refPoint.equals(intersection.getRefPoint()));
        assertTrue(laneWidth.equals(intersection.getLaneWidth()));
        assertTrue(speedLimits.equals(intersection.getSpeedLimits()));
        assertTrue(laneSet.equals(intersection.getLaneSet()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String intersectionGeometryOptionals = "100000";

        IntersectionGeometry intersection = new IntersectionGeometry();
        thrown.expect(IllegalArgumentException.class);
        intersection.decodeUPER(intersectionGeometryOptionals);
    }

    @Test
    public void testDecodeUPERDescriptiveName() {

        String intersectionGeometryOptionals = "010000";

        IntersectionGeometry intersection = new IntersectionGeometry();
        thrown.expect(IllegalArgumentException.class);
        intersection.decodeUPER(intersectionGeometryOptionals);
    }

    @Test
    public void testDecodeUPERPreemptList() {

        String intersectionGeometryOptionals = "000010";

        IntersectionGeometry intersection = new IntersectionGeometry();
        thrown.expect(IllegalArgumentException.class);
        intersection.decodeUPER(intersectionGeometryOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String intersectionGeometryOptionals = "000001";

        IntersectionGeometry intersection = new IntersectionGeometry();
        thrown.expect(IllegalArgumentException.class);
        intersection.decodeUPER(intersectionGeometryOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String intersectionGeometryOptionals = "00000";

        IntersectionGeometry intersection = new IntersectionGeometry();
        thrown.expect(IllegalArgumentException.class);
        intersection.decodeUPER(intersectionGeometryOptionals);
    }

    @Test
    public void testHashCode() {

        IntersectionGeometry intersection = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection.setLaneWidth(laneWidth);
        intersection.setSpeedLimits(speedLimits);

        int revision = intersection.getRevision().getValue();
        int laneWidth = intersection.getLaneWidth().getValue();

        IntersectionReferenceID diffId = new IntersectionReferenceID(id.getId().getValue() + 1);
        Position3D diffRefPoint = new Position3D(refPoint.getLatitude().getValue() + 1, refPoint.getLongitude().getValue() + 1);
        LaneList diffLaneSet = new LaneList(laneSet.getLaneArray().length + 1);
        SpeedLimitList diffSpeedLimits = new SpeedLimitList(speedLimits.getSpeedLimitArray().length + 1);

        IntersectionGeometry intersection2 = new IntersectionGeometry(diffId, revision + 1, diffRefPoint, diffLaneSet);
        intersection2.setLaneWidth(laneWidth + 1);
        intersection2.setSpeedLimits(diffSpeedLimits);

        assertFalse(intersection.hashCode() == intersection2.hashCode());
        assertTrue(intersection.hashCode() == intersection.hashCode());
        assertTrue(intersection2.hashCode() == intersection2.hashCode());

        IntersectionGeometry intersection3 = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection3.setLaneWidth(laneWidth);
        intersection3.setSpeedLimits(speedLimits);

        assertTrue(intersection.hashCode() == intersection3.hashCode());
        assertFalse(intersection2.hashCode() == intersection3.hashCode());
    }

    @Test
    public void testEquals() {

        IntersectionGeometry intersection = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection.setLaneWidth(laneWidth);
        intersection.setSpeedLimits(speedLimits);

        assertTrue(intersection.equals(intersection));
        assertFalse(intersection.equals(null));
        assertFalse(intersection.equals(new String()));

        int revision = intersection.getRevision().getValue();
        int laneWidth = intersection.getLaneWidth().getValue();

        IntersectionReferenceID diffId = new IntersectionReferenceID(id.getId().getValue() + 1);
        Position3D diffRefPoint = new Position3D(refPoint.getLatitude().getValue() + 1, refPoint.getLongitude().getValue() + 1);
        LaneList diffLaneSet = new LaneList(laneSet.getLaneArray().length + 1);
        SpeedLimitList diffSpeedLimits = new SpeedLimitList(speedLimits.getSpeedLimitArray().length + 1);

        // different
        IntersectionGeometry intersection2 = new IntersectionGeometry(diffId, revision + 1, diffRefPoint, diffLaneSet);
        intersection2.setLaneWidth(laneWidth + 1);
        intersection2.setSpeedLimits(diffSpeedLimits);

        assertFalse(intersection.equals(intersection2));

        // different id
        intersection2 = new IntersectionGeometry(diffId, revision, refPoint, laneSet);
        intersection2.setLaneWidth(laneWidth);
        intersection2.setSpeedLimits(speedLimits);

        assertFalse(intersection.equals(intersection2));

        // different revision
        intersection2 = new IntersectionGeometry(id, revision + 1, refPoint, laneSet);
        intersection2.setLaneWidth(laneWidth);
        intersection2.setSpeedLimits(speedLimits);

        assertFalse(intersection.equals(intersection2));

        // different ref point
        intersection2 = new IntersectionGeometry(id, revision, diffRefPoint, laneSet);
        intersection2.setLaneWidth(laneWidth);
        intersection2.setSpeedLimits(speedLimits);

        assertFalse(intersection.equals(intersection2));

        // different lane set
        intersection2 = new IntersectionGeometry(id, revision, refPoint, diffLaneSet);
        intersection2.setLaneWidth(laneWidth);
        intersection2.setSpeedLimits(speedLimits);

        assertFalse(intersection.equals(intersection2));

        // different lane width
        intersection2 = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection2.setLaneWidth(laneWidth + 1);
        intersection2.setSpeedLimits(speedLimits);

        assertFalse(intersection.equals(intersection2));

        // different speed limits
        intersection2 = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection2.setLaneWidth(laneWidth);
        intersection2.setSpeedLimits(diffSpeedLimits);

        assertFalse(intersection.equals(intersection2));

        // same
        intersection2 = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection2.setLaneWidth(laneWidth);
        intersection2.setSpeedLimits(speedLimits);

        assertTrue(intersection.equals(intersection2));
    }
}
