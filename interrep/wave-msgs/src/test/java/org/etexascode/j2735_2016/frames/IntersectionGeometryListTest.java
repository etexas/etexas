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
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.etexascode.j2735_2016.elements.LaneAttributesBike;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LaneWidth;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the intersection geometry list frame.
 * 
 * @author ttevendale
 */
public class IntersectionGeometryListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        IntersectionGeometryList intersections = new IntersectionGeometryList(IntersectionGeometryList.MIN_LIST_SIZE);
        assertTrue(intersections.getIntersectionGeometryArray().length == IntersectionGeometryList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        intersections = new IntersectionGeometryList(IntersectionGeometryList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        IntersectionGeometryList intersections = new IntersectionGeometryList(IntersectionGeometryList.MAX_LIST_SIZE);
        assertTrue(intersections.getIntersectionGeometryArray().length == IntersectionGeometryList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        intersections = new IntersectionGeometryList(IntersectionGeometryList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numIntersections = 12;
        IntersectionGeometryList intersections = new IntersectionGeometryList(numIntersections);
        assertTrue(intersections.getIntersectionGeometryArray().length == numIntersections);
    }

    @Test
    public void testEncodeUPERMin() {

        IntersectionGeometryList intersections = new IntersectionGeometryList(IntersectionGeometryList.MIN_LIST_SIZE);
        IntersectionGeometry intersection = createIntersectionGeometry();
        intersections.getIntersectionGeometryArray()[0] = intersection;

        String listSize = "00000";
        String remainingBits = intersection.encodeUPER();

        assertTrue((listSize + remainingBits).equals(intersections.encodeUPER()));

        intersections = new IntersectionGeometryList(IntersectionGeometryList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        intersections.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111";
        String remainingBits = "";

        IntersectionGeometryList intersections = new IntersectionGeometryList(IntersectionGeometryList.MAX_LIST_SIZE);

        IntersectionGeometry[] intersectionArray = intersections.getIntersectionGeometryArray();
        for (int i = 0; i < intersectionArray.length; i++) {

            IntersectionGeometry intersection = createIntersectionGeometry();
            intersectionArray[i] = intersection;
            remainingBits += intersection.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(intersections.encodeUPER()));

        intersections = new IntersectionGeometryList(IntersectionGeometryList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        intersections.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        IntersectionGeometryList intersections = new IntersectionGeometryList();
        thrown.expect(IllegalStateException.class);
        intersections.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        IntersectionGeometry intersection = createIntersectionGeometry();
        String listSize = "00000";
        String remainingBits = intersection.encodeUPER();

        IntersectionGeometryList intersections = new IntersectionGeometryList();
        intersections.decodeUPER(listSize + remainingBits);
        IntersectionGeometry[] intersectionArray = intersections.getIntersectionGeometryArray();
        assertTrue(IntersectionGeometryList.MIN_LIST_SIZE == intersectionArray.length);
        assertTrue(intersection.equals(intersectionArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        IntersectionGeometry intersection = createIntersectionGeometry();
        IntersectionGeometry intersection2 = createIntersectionGeometry2();

        String listSize = "11111";
        String remainingBits = intersection.encodeUPER();

        for (int i = 0; i < IntersectionGeometryList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += intersection2.encodeUPER();
        }

        IntersectionGeometryList intersections = new IntersectionGeometryList();
        intersections.decodeUPER(listSize + remainingBits);

        IntersectionGeometry[] intersectionArray = intersections.getIntersectionGeometryArray();
        assertTrue(IntersectionGeometryList.MAX_LIST_SIZE == intersectionArray.length);
        assertTrue(intersection.equals(intersectionArray[0]));
        for (int i = 1; i < IntersectionGeometryList.MAX_LIST_SIZE; i++) {

            assertTrue(intersection2.equals(intersectionArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        IntersectionGeometryList intersections = new IntersectionGeometryList();
        thrown.expect(IllegalArgumentException.class);
        intersections.decodeUPER("0101");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        IntersectionGeometryList intersections = new IntersectionGeometryList();
        thrown.expect(IllegalArgumentException.class);
        // 11111 = 32 objects, but there's none
        intersections.decodeUPER("11111010100");
    }

    @Test
    public void testHashCode() {

        IntersectionGeometryList intersections = new IntersectionGeometryList(1);
        intersections.getIntersectionGeometryArray()[0] = createIntersectionGeometry();

        assertTrue(intersections.hashCode() == intersections.hashCode());

        IntersectionGeometryList intersections2 = new IntersectionGeometryList(2);

        assertFalse(intersections.hashCode() == intersections2.hashCode());

        intersections2 = new IntersectionGeometryList(1);
        intersections2.getIntersectionGeometryArray()[0] = createIntersectionGeometry2();

        assertFalse(intersections.hashCode() == intersections2.hashCode());

        intersections2.getIntersectionGeometryArray()[0] = createIntersectionGeometry();

        assertTrue(intersections.hashCode() == intersections2.hashCode());
    }

    @Test
    public void testEquals() {

        IntersectionGeometryList intersections = new IntersectionGeometryList(1);
        intersections.getIntersectionGeometryArray()[0] = createIntersectionGeometry();

        assertTrue(intersections.equals(intersections));
        assertFalse(intersections.equals(null));
        assertFalse(intersections.equals(new String()));

        IntersectionGeometryList intersections2 = new IntersectionGeometryList(2);

        assertFalse(intersections.equals(intersections2));

        intersections2 = new IntersectionGeometryList(1);
        intersections2.getIntersectionGeometryArray()[0] = createIntersectionGeometry2();

        assertFalse(intersections.equals(intersections2));

        intersections2.getIntersectionGeometryArray()[0] = createIntersectionGeometry();

        assertTrue(intersections.equals(intersections2));
    }

    private IntersectionGeometry createIntersectionGeometry() {

        IntersectionReferenceID id = new IntersectionReferenceID(1);
        MsgCount revision = new MsgCount(15);
        Position3D refPoint = new Position3D(1232, 43234);
        LaneWidth laneWidth = new LaneWidth(30);
        SpeedLimitList speedLimits = new SpeedLimitList(1);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.VEHICLE_MAX_SPEED, 50);
        LaneList laneSet = new LaneList(1);
        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesVehicle()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(0, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3888)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(38))));
        laneSet.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        IntersectionGeometry intersection = new IntersectionGeometry(id, revision, refPoint, laneSet);
        intersection.setLaneWidth(laneWidth);
        intersection.setSpeedLimits(speedLimits);

        return intersection;
    }

    private IntersectionGeometry createIntersectionGeometry2() {

        IntersectionReferenceID id = new IntersectionReferenceID(4);
        MsgCount revision = new MsgCount(10);
        Position3D refPoint = new Position3D(122, -234);
        LaneList laneSet = new LaneList(1);
        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesBike()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(2, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(9871)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(151))));
        laneSet.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        return new IntersectionGeometry(id, revision, refPoint, laneSet);
    }
}
