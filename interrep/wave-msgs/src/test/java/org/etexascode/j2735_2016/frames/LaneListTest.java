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
 * Unit tests for the lane list frame.
 * 
 * @author ttevendale
 */
public class LaneListTest {

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

        LaneList lanes = new LaneList(LaneList.MIN_LIST_SIZE);
        assertTrue(lanes.getLaneArray().length == LaneList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        lanes = new LaneList(LaneList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        LaneList lanes = new LaneList(LaneList.MAX_LIST_SIZE);
        assertTrue(lanes.getLaneArray().length == LaneList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        lanes = new LaneList(LaneList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numLanes = 12;
        LaneList lanes = new LaneList(numLanes);
        assertTrue(lanes.getLaneArray().length == numLanes);
    }

    @Test
    public void testEncodeUPERMin() {

        LaneList lanes = new LaneList(LaneList.MIN_LIST_SIZE);
        GenericLane lane = new GenericLane(1, attributes, nodeList);
        GenericLane[] laneArray = lanes.getLaneArray();
        laneArray[0] = lane;

        String listSize = "00000000";
        String remainingBits = lane.encodeUPER();

        assertTrue((listSize + remainingBits).equals(lanes.encodeUPER()));

        lanes = new LaneList(LaneList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111110";
        String remainingBits = "";

        LaneList lanes = new LaneList(LaneList.MAX_LIST_SIZE);

        GenericLane[] laneArray = lanes.getLaneArray();
        for (int i = 0; i < laneArray.length; i++) {

            GenericLane lane = new GenericLane(i, attributes, nodeList);
            laneArray[i] = lane;
            remainingBits += lane.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(lanes.encodeUPER()));

        lanes = new LaneList(LaneList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        LaneList lanes = new LaneList();
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        GenericLane lane = new GenericLane(3, attributes, nodeList);
        String listSize = "00000000";
        String remainingBits = lane.encodeUPER();

        LaneList lanes = new LaneList();
        lanes.decodeUPER(listSize + remainingBits);
        GenericLane[] laneArray = lanes.getLaneArray();
        assertTrue(LaneList.MIN_LIST_SIZE == laneArray.length);
        assertTrue(lane.equals(laneArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        GenericLane lane = new GenericLane(4, attributes, nodeList);
        GenericLane lane2 = new GenericLane(5, attributes, nodeList);

        String listSize = "11111110";
        String remainingBits = lane.encodeUPER();

        for (int i = 0; i < LaneList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += lane2.encodeUPER();
        }

        LaneList lanes = new LaneList();
        lanes.decodeUPER(listSize + remainingBits);

        GenericLane[] laneArray = lanes.getLaneArray();
        assertTrue(LaneList.MAX_LIST_SIZE == laneArray.length);
        assertTrue(lane.equals(laneArray[0]));
        for (int i = 1; i < LaneList.MAX_LIST_SIZE; i++) {

            assertTrue(lane2.equals(laneArray[i]));
        }
    }

    @Test
    public void testDecodeUPERAboveMax() {

        String listSize = "11111111";
        LaneList lanes = new LaneList();
        thrown.expect(IllegalArgumentException.class);
        lanes.decodeUPER(listSize);
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneList lanes = new LaneList();
        thrown.expect(IllegalArgumentException.class);
        lanes.decodeUPER("0100001");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        LaneList lanes = new LaneList();
        thrown.expect(IllegalArgumentException.class);
        // 00101100 = 39 objects, but there's none
        lanes.decodeUPER("00101100100");
    }

    @Test
    public void testHashCode() {

        LaneList lanes = new LaneList(1);
        lanes.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.hashCode() == lanes.hashCode());

        LaneList lanes2 = new LaneList(2);

        assertFalse(lanes.hashCode() == lanes2.hashCode());

        lanes2 = new LaneList(1);
        lanes2.getLaneArray()[0] = new GenericLane(2, attributes, nodeList);

        assertFalse(lanes.hashCode() == lanes2.hashCode());

        lanes2.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.hashCode() == lanes2.hashCode());
    }

    @Test
    public void testEquals() {

        LaneList lanes = new LaneList(1);
        lanes.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.equals(lanes));
        assertFalse(lanes.equals(null));
        assertFalse(lanes.equals(new String()));

        LaneList lanes2 = new LaneList(2);

        assertFalse(lanes.equals(lanes2));

        lanes2 = new LaneList(1);
        lanes2.getLaneArray()[0] = new GenericLane(2, attributes, nodeList);

        assertFalse(lanes.equals(lanes2));

        lanes2.getLaneArray()[0] = new GenericLane(1, attributes, nodeList);

        assertTrue(lanes.equals(lanes2));
    }
}
