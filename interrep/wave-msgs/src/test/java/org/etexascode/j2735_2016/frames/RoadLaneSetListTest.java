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
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the road lane set list frame.
 * 
 * @author ttevendale
 */
public class RoadLaneSetListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    LaneAttributes attributes;

    NodeListXY nodeList;

    @Before
    public void init() {

        attributes = new LaneAttributes();
        attributes.setLaneType(new LaneTypeAttributes(new LaneAttributesVehicle()));
        nodeList = new NodeListXY(new ComputedLane(0, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3888)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(38))));
    }

    @Test
    public void testConstructorMin() {

        RoadLaneSetList lanes = new RoadLaneSetList(RoadLaneSetList.MIN_LIST_SIZE);
        assertTrue(lanes.getLaneArray().length == RoadLaneSetList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        lanes = new RoadLaneSetList(RoadLaneSetList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        RoadLaneSetList lanes = new RoadLaneSetList(RoadLaneSetList.MAX_LIST_SIZE);
        assertTrue(lanes.getLaneArray().length == RoadLaneSetList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        lanes = new RoadLaneSetList(RoadLaneSetList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numLanes = 12;
        RoadLaneSetList lanes = new RoadLaneSetList(numLanes);
        assertTrue(lanes.getLaneArray().length == numLanes);
    }

    @Test
    public void testEncodeUPERMin() {

        RoadLaneSetList lanes = new RoadLaneSetList(RoadLaneSetList.MIN_LIST_SIZE);
        GenericLane lane = new GenericLane(1, attributes, nodeList);
        GenericLane[] laneArray = lanes.getLaneArray();
        laneArray[0] = lane;

        String listSize = "00000000";
        String remainingBits = lane.encodeUPER();

        assertTrue((listSize + remainingBits).equals(lanes.encodeUPER()));

        lanes = new RoadLaneSetList(RoadLaneSetList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111110";
        String remainingBits = "";

        RoadLaneSetList lanes = new RoadLaneSetList(RoadLaneSetList.MAX_LIST_SIZE);

        GenericLane[] laneArray = lanes.getLaneArray();
        for (int i = 0; i < laneArray.length; i++) {

            GenericLane lane = new GenericLane(i, attributes, nodeList);
            laneArray[i] = lane;
            remainingBits += lane.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(lanes.encodeUPER()));

        lanes = new RoadLaneSetList(RoadLaneSetList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        RoadLaneSetList lanes = new RoadLaneSetList();
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        GenericLane lane = new GenericLane(3, attributes, nodeList);
        String listSize = "00000000";
        String remainingBits = lane.encodeUPER();

        RoadLaneSetList lanes = new RoadLaneSetList();
        lanes.decodeUPER(listSize + remainingBits);
        GenericLane[] laneArray = lanes.getLaneArray();
        assertTrue(RoadLaneSetList.MIN_LIST_SIZE == laneArray.length);
        assertTrue(lane.equals(laneArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        GenericLane lane = new GenericLane(4, attributes, nodeList);
        GenericLane lane2 = new GenericLane(5, attributes, nodeList);

        String listSize = "11111110";
        String remainingBits = lane.encodeUPER();

        for (int i = 0; i < RoadLaneSetList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += lane2.encodeUPER();
        }

        RoadLaneSetList lanes = new RoadLaneSetList();
        lanes.decodeUPER(listSize + remainingBits);

        GenericLane[] laneArray = lanes.getLaneArray();
        assertTrue(RoadLaneSetList.MAX_LIST_SIZE == laneArray.length);
        assertTrue(lane.equals(laneArray[0]));
        for (int i = 1; i < RoadLaneSetList.MAX_LIST_SIZE; i++) {

            assertTrue(lane2.equals(laneArray[i]));
        }
    }

    @Test
    public void testDecodeUPERAboveMax() {

        String listSize = "11111111";
        RoadLaneSetList lanes = new RoadLaneSetList();
        thrown.expect(IllegalArgumentException.class);
        lanes.decodeUPER(listSize);
    }

    @Test
    public void testDecodeUPERLessBits() {

        RoadLaneSetList lanes = new RoadLaneSetList();
        thrown.expect(IllegalArgumentException.class);
        lanes.decodeUPER("0100001");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        RoadLaneSetList lanes = new RoadLaneSetList();
        thrown.expect(IllegalArgumentException.class);
        // 00101100 = 39 objects, but there's none
        lanes.decodeUPER("00101100100");
    }

    @Test
    public void testHashCode() {

        RoadLaneSetList lanes = new RoadLaneSetList(1);
        lanes.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.hashCode() == lanes.hashCode());

        RoadLaneSetList lanes2 = new RoadLaneSetList(2);

        assertFalse(lanes.hashCode() == lanes2.hashCode());

        lanes2 = new RoadLaneSetList(1);
        lanes2.getLaneArray()[0] = new GenericLane(2, attributes, nodeList);

        assertFalse(lanes.hashCode() == lanes2.hashCode());

        lanes2.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.hashCode() == lanes2.hashCode());
    }

    @Test
    public void testEquals() {

        RoadLaneSetList lanes = new RoadLaneSetList(1);
        lanes.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.equals(lanes));
        assertFalse(lanes.equals(null));
        assertFalse(lanes.equals(new String()));

        RoadLaneSetList lanes2 = new RoadLaneSetList(2);

        assertFalse(lanes.equals(lanes2));

        lanes2 = new RoadLaneSetList(1);
        lanes2.getLaneArray()[0] = new GenericLane(2, attributes, nodeList);

        assertFalse(lanes.equals(lanes2));

        lanes2.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.equals(lanes2));
    }
}
