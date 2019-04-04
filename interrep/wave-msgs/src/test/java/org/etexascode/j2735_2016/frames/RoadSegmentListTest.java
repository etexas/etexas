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
 * Unit tests for the road segment list frame.
 * 
 * @author ttevendale
 */
public class RoadSegmentListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        RoadSegmentList roadSegments = new RoadSegmentList(RoadSegmentList.MIN_LIST_SIZE);
        assertTrue(roadSegments.getRoadSegmentArray().length == RoadSegmentList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        roadSegments = new RoadSegmentList(RoadSegmentList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        RoadSegmentList roadSegments = new RoadSegmentList(RoadSegmentList.MAX_LIST_SIZE);
        assertTrue(roadSegments.getRoadSegmentArray().length == RoadSegmentList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        roadSegments = new RoadSegmentList(RoadSegmentList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numSegments = 12;
        RoadSegmentList roadSegments = new RoadSegmentList(numSegments);
        assertTrue(roadSegments.getRoadSegmentArray().length == numSegments);
    }

    @Test
    public void testEncodeUPERMin() {

        RoadSegmentList roadSegments = new RoadSegmentList(RoadSegmentList.MIN_LIST_SIZE);
        RoadSegment roadSegment = createRoadSegement();
        roadSegments.getRoadSegmentArray()[0] = roadSegment;

        String listSize = "00000";
        String remainingBits = roadSegment.encodeUPER();

        assertTrue((listSize + remainingBits).equals(roadSegments.encodeUPER()));

        roadSegments = new RoadSegmentList(RoadSegmentList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        roadSegments.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111";
        String remainingBits = "";

        RoadSegmentList roadSegments = new RoadSegmentList(RoadSegmentList.MAX_LIST_SIZE);

        RoadSegment[] roadSegmentArray = roadSegments.getRoadSegmentArray();
        for (int i = 0; i < roadSegmentArray.length; i++) {

            RoadSegment roadSegment = createRoadSegement();
            roadSegmentArray[i] = roadSegment;
            remainingBits += roadSegment.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(roadSegments.encodeUPER()));

        roadSegments = new RoadSegmentList(RoadSegmentList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        roadSegments.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        RoadSegmentList roadSegments = new RoadSegmentList();
        thrown.expect(IllegalStateException.class);
        roadSegments.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        RoadSegment roadSegment = createRoadSegement();
        String listSize = "00000";
        String remainingBits = roadSegment.encodeUPER();

        RoadSegmentList roadSegments = new RoadSegmentList();
        roadSegments.decodeUPER(listSize + remainingBits);
        RoadSegment[] roadSegmentArray = roadSegments.getRoadSegmentArray();
        assertTrue(RoadSegmentList.MIN_LIST_SIZE == roadSegmentArray.length);
        assertTrue(roadSegment.equals(roadSegmentArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        RoadSegment roadSegment = createRoadSegement();
        RoadSegment roadSegment2 = createRoadSegement2();

        String listSize = "11111";
        String remainingBits = roadSegment.encodeUPER();

        for (int i = 0; i < RoadSegmentList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += roadSegment2.encodeUPER();
        }

        RoadSegmentList roadSegments = new RoadSegmentList();
        roadSegments.decodeUPER(listSize + remainingBits);

        RoadSegment[] roadSegmentArray = roadSegments.getRoadSegmentArray();
        assertTrue(RoadSegmentList.MAX_LIST_SIZE == roadSegmentArray.length);
        assertTrue(roadSegment.equals(roadSegmentArray[0]));
        for (int i = 1; i < RoadSegmentList.MAX_LIST_SIZE; i++) {

            assertTrue(roadSegment2.equals(roadSegmentArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        RoadSegmentList roadSegments = new RoadSegmentList();
        thrown.expect(IllegalArgumentException.class);
        roadSegments.decodeUPER("0101");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        RoadSegmentList roadSegments = new RoadSegmentList();
        thrown.expect(IllegalArgumentException.class);
        // 11111 = 32 objects, but there's none
        roadSegments.decodeUPER("11111010100");
    }

    @Test
    public void testHashCode() {

        RoadSegmentList roadSegments = new RoadSegmentList(1);
        roadSegments.getRoadSegmentArray()[0] = createRoadSegement();

        assertTrue(roadSegments.hashCode() == roadSegments.hashCode());

        RoadSegmentList roadSegments2 = new RoadSegmentList(2);

        assertFalse(roadSegments.hashCode() == roadSegments2.hashCode());

        roadSegments2 = new RoadSegmentList(1);
        roadSegments2.getRoadSegmentArray()[0] = createRoadSegement2();

        assertFalse(roadSegments.hashCode() == roadSegments2.hashCode());

        roadSegments2.getRoadSegmentArray()[0] = createRoadSegement();

        assertTrue(roadSegments.hashCode() == roadSegments2.hashCode());
    }

    @Test
    public void testEquals() {

        RoadSegmentList roadSegments = new RoadSegmentList(1);
        roadSegments.getRoadSegmentArray()[0] = createRoadSegement();

        assertTrue(roadSegments.equals(roadSegments));
        assertFalse(roadSegments.equals(null));
        assertFalse(roadSegments.equals(new String()));

        RoadSegmentList roadSegments2 = new RoadSegmentList(2);

        assertFalse(roadSegments.equals(roadSegments2));

        roadSegments2 = new RoadSegmentList(1);
        roadSegments2.getRoadSegmentArray()[0] = createRoadSegement2();

        assertFalse(roadSegments.equals(roadSegments2));

        roadSegments2.getRoadSegmentArray()[0] = createRoadSegement();

        assertTrue(roadSegments.equals(roadSegments2));
    }

    private RoadSegment createRoadSegement() {

        RoadSegmentReferenceID id = new RoadSegmentReferenceID(1);
        MsgCount revision = new MsgCount(15);
        Position3D refPoint = new Position3D(1232, 43234);
        LaneWidth laneWidth = new LaneWidth(30);
        SpeedLimitList speedLimits = new SpeedLimitList(1);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.VEHICLE_MAX_SPEED, 50);
        RoadLaneSetList roadRoadLaneSet = new RoadLaneSetList(1);
        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesVehicle()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(0, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3888)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(38))));
        roadRoadLaneSet.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        RoadSegment roadSegment = new RoadSegment(id, revision, refPoint, roadRoadLaneSet);
        roadSegment.setLaneWidth(laneWidth);
        roadSegment.setSpeedLimits(speedLimits);

        return roadSegment;
    }

    private RoadSegment createRoadSegement2() {

        RoadSegmentReferenceID id = new RoadSegmentReferenceID(4);
        MsgCount revision = new MsgCount(10);
        Position3D refPoint = new Position3D(122, -234);
        RoadLaneSetList roadRoadLaneSet = new RoadLaneSetList(1);
        LaneAttributes attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesBike()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(2, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(9871)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(151))));
        roadRoadLaneSet.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        return new RoadSegment(id, revision, refPoint, roadRoadLaneSet);
    }
}
