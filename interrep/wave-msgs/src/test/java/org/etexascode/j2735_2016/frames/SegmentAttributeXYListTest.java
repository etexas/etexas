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

import org.etexascode.j2735_2016.elements.SegmentAttributeXY;
import org.etexascode.j2735_2016.elements.SegmentAttributeXY.SegmentAttribute;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the segment attribute xy list frame.
 * 
 * @author ttevendale
 */
public class SegmentAttributeXYListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MIN_LIST_SIZE);
        assertTrue(segmentAttributes.getAttributeArray().length == SegmentAttributeXYList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MAX_LIST_SIZE);
        assertTrue(segmentAttributes.getAttributeArray().length == SegmentAttributeXYList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numAttributes = 4;
        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList(numAttributes);
        assertTrue(segmentAttributes.getAttributeArray().length == numAttributes);
    }

    @Test
    public void testEncodeUPERMin() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MIN_LIST_SIZE);
        SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.ADAPTIVE_TIMING_PRESENT);
        SegmentAttributeXY[] attributeArray = segmentAttributes.getAttributeArray();
        attributeArray[0] = attribute;

        String listSize = "000";
        String remainingBits = attribute.encodeUPER();

        assertTrue((listSize + remainingBits).equals(segmentAttributes.encodeUPER()));

        segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        segmentAttributes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "111";
        String remainingBits = "";

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MAX_LIST_SIZE);

        SegmentAttributeXY[] attributeArray = segmentAttributes.getAttributeArray();
        for (int i = 0; i < attributeArray.length; i++) {

            SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.ADJACENT_BIKE_LANE_ON_RIGHT);
            attributeArray[i] = attribute;
            remainingBits += attribute.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(segmentAttributes.encodeUPER()));

        segmentAttributes = new SegmentAttributeXYList(SegmentAttributeXYList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        segmentAttributes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList();
        thrown.expect(IllegalStateException.class);
        segmentAttributes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.BIKE_BOX_IN_FRONT);
        String listSize = "000";
        String remainingBits = attribute.encodeUPER();

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList();
        segmentAttributes.decodeUPER(listSize + remainingBits);
        SegmentAttributeXY[] attributeArray = segmentAttributes.getAttributeArray();
        assertTrue(SegmentAttributeXYList.MIN_LIST_SIZE == attributeArray.length);
        assertTrue(attribute.equals(attributeArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.TURN_OUT_POINT_ON_RIGHT);
        SegmentAttributeXY attribute2 = new SegmentAttributeXY(SegmentAttribute.WHITE_LINE);

        String listSize = "111";
        String remainingBits = attribute.encodeUPER();

        for (int i = 0; i < SegmentAttributeXYList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += attribute2.encodeUPER();
        }

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList();
        segmentAttributes.decodeUPER(listSize + remainingBits);

        SegmentAttributeXY[] attributeArray = segmentAttributes.getAttributeArray();
        assertTrue(SegmentAttributeXYList.MAX_LIST_SIZE == attributeArray.length);
        assertTrue(attribute.equals(attributeArray[0]));
        for (int i = 1; i < SegmentAttributeXYList.MAX_LIST_SIZE; i++) {

            assertTrue(attribute2.equals(attributeArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList();
        thrown.expect(IllegalArgumentException.class);
        segmentAttributes.decodeUPER("01");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList();
        thrown.expect(IllegalArgumentException.class);
        // 111 = 8 objects, but there's none
        segmentAttributes.decodeUPER("11111010100");
    }

    @Test
    public void testHashCode() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList(1);
        segmentAttributes.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_LEFT);

        assertTrue(segmentAttributes.hashCode() == segmentAttributes.hashCode());

        SegmentAttributeXYList segmentAttributes2 = new SegmentAttributeXYList(2);

        assertFalse(segmentAttributes.hashCode() == segmentAttributes2.hashCode());

        segmentAttributes2 = new SegmentAttributeXYList(1);
        segmentAttributes2.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_RIGHT);

        assertFalse(segmentAttributes.hashCode() == segmentAttributes2.hashCode());

        segmentAttributes2.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_LEFT);

        assertTrue(segmentAttributes.hashCode() == segmentAttributes2.hashCode());
    }

    @Test
    public void testEquals() {

        SegmentAttributeXYList segmentAttributes = new SegmentAttributeXYList(1);
        segmentAttributes.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_LEFT);

        assertTrue(segmentAttributes.equals(segmentAttributes));
        assertFalse(segmentAttributes.equals(null));
        assertFalse(segmentAttributes.equals(new String()));

        SegmentAttributeXYList segmentAttributes2 = new SegmentAttributeXYList(2);

        assertFalse(segmentAttributes.equals(segmentAttributes2));

        segmentAttributes2 = new SegmentAttributeXYList(1);
        segmentAttributes2.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_RIGHT);

        assertFalse(segmentAttributes.equals(segmentAttributes2));

        segmentAttributes2.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_LEFT);

        assertTrue(segmentAttributes.equals(segmentAttributes2));
    }
}
