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

import org.etexascode.j2735_2016.elements.LaneID;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the overlay lane list frame.
 * 
 * @author ttevendale
 */
public class OverlayLaneListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        OverlayLaneList lanes = new OverlayLaneList(OverlayLaneList.MIN_LIST_SIZE);
        assertTrue(lanes.getLaneArray().length == OverlayLaneList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        lanes = new OverlayLaneList(OverlayLaneList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        OverlayLaneList lanes = new OverlayLaneList(OverlayLaneList.MAX_LIST_SIZE);
        assertTrue(lanes.getLaneArray().length == OverlayLaneList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        lanes = new OverlayLaneList(OverlayLaneList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numLanes = 3;
        OverlayLaneList lanes = new OverlayLaneList(numLanes);
        assertTrue(lanes.getLaneArray().length == numLanes);
    }

    @Test
    public void testEncodeUPERMin() {

        OverlayLaneList lanes = new OverlayLaneList(OverlayLaneList.MIN_LIST_SIZE);
        LaneID lane = new LaneID(1);
        LaneID[] laneArray = lanes.getLaneArray();
        laneArray[0] = lane;

        String listSize = "000";
        String remainingBits = lane.encodeUPER();

        assertTrue((listSize + remainingBits).equals(lanes.encodeUPER()));

        lanes = new OverlayLaneList(OverlayLaneList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "100";
        String remainingBits = "";

        OverlayLaneList lanes = new OverlayLaneList(OverlayLaneList.MAX_LIST_SIZE);

        LaneID[] laneArray = lanes.getLaneArray();
        for (int i = 0; i < laneArray.length; i++) {

            LaneID lane = new LaneID(i);
            laneArray[i] = lane;
            remainingBits += lane.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(lanes.encodeUPER()));

        lanes = new OverlayLaneList(OverlayLaneList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        OverlayLaneList lanes = new OverlayLaneList();
        thrown.expect(IllegalStateException.class);
        lanes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        LaneID lane = new LaneID(2);
        String listSize = "000";
        String remainingBits = lane.encodeUPER();

        OverlayLaneList lanes = new OverlayLaneList();
        lanes.decodeUPER(listSize + remainingBits);
        LaneID[] laneArray = lanes.getLaneArray();
        assertTrue(OverlayLaneList.MIN_LIST_SIZE == laneArray.length);
        assertTrue(lane.equals(laneArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        LaneID lane = new LaneID(3);
        LaneID lane2 = new LaneID(4);

        String listSize = "100";
        String remainingBits = lane.encodeUPER();

        for (int i = 0; i < OverlayLaneList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += lane2.encodeUPER();
        }

        OverlayLaneList lanes = new OverlayLaneList();
        lanes.decodeUPER(listSize + remainingBits);

        LaneID[] laneArray = lanes.getLaneArray();
        assertTrue(OverlayLaneList.MAX_LIST_SIZE == laneArray.length);
        assertTrue(lane.equals(laneArray[0]));
        for (int i = 1; i < OverlayLaneList.MAX_LIST_SIZE; i++) {

            assertTrue(lane2.equals(laneArray[i]));
        }
    }

    @Test
    public void testDecodeUPERAboveMax() {

        String listSize = "111";
        OverlayLaneList lanes = new OverlayLaneList();
        thrown.expect(IllegalArgumentException.class);
        lanes.decodeUPER(listSize);
    }

    @Test
    public void testDecodeUPERLessBits() {

        OverlayLaneList lanes = new OverlayLaneList();
        thrown.expect(IllegalArgumentException.class);
        lanes.decodeUPER("11");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        OverlayLaneList lanes = new OverlayLaneList();
        thrown.expect(IllegalArgumentException.class);
        // 100 = 5 objects, but there's none
        lanes.decodeUPER("10011010100");
    }

    @Test
    public void testHashCode() {

        OverlayLaneList lanes = new OverlayLaneList(1);
        lanes.getLaneArray()[0] = new LaneID(5);

        assertTrue(lanes.hashCode() == lanes.hashCode());

        OverlayLaneList lanes2 = new OverlayLaneList(2);

        assertFalse(lanes.hashCode() == lanes2.hashCode());

        lanes2 = new OverlayLaneList(1);
        lanes2.getLaneArray()[0] = new LaneID(6);

        assertFalse(lanes.hashCode() == lanes2.hashCode());

        lanes2.getLaneArray()[0] = new LaneID(5);

        assertTrue(lanes.hashCode() == lanes2.hashCode());
    }

    @Test
    public void testEquals() {

        OverlayLaneList lanes = new OverlayLaneList(1);
        lanes.getLaneArray()[0] = new LaneID(5);

        assertTrue(lanes.equals(lanes));
        assertFalse(lanes.equals(null));
        assertFalse(lanes.equals(new String()));

        OverlayLaneList lanes2 = new OverlayLaneList(2);

        assertFalse(lanes.equals(lanes2));

        lanes2 = new OverlayLaneList(1);
        lanes2.getLaneArray()[0] = new LaneID(6);

        assertFalse(lanes.equals(lanes2));

        lanes2.getLaneArray()[0] = new LaneID(5);

        assertTrue(lanes.equals(lanes2));
    }
}
