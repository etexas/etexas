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

import org.etexascode.j2735_2016.elements.DeltaAngle;
import org.etexascode.j2735_2016.elements.RoadwayCrownAngle;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the lane data attribute list frame.
 * 
 * @author ttevendale
 */
public class LaneDataAttributeListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MIN_LIST_SIZE);
        assertTrue(laneAttributes.getAttributeArray().length == LaneDataAttributeList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MAX_LIST_SIZE);
        assertTrue(laneAttributes.getAttributeArray().length == LaneDataAttributeList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numAttributes = 7;
        LaneDataAttributeList laneAttributes = new LaneDataAttributeList(numAttributes);
        assertTrue(laneAttributes.getAttributeArray().length == numAttributes);
    }

    @Test
    public void testEncodeUPERMin() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MIN_LIST_SIZE);
        LaneDataAttribute laneAttribute = new LaneDataAttribute(new DeltaAngle(-111));
        LaneDataAttribute[] laneAttributeArray = laneAttributes.getAttributeArray();
        laneAttributeArray[0] = laneAttribute;

        String listSize = "000";
        String remainingBits = laneAttribute.encodeUPER();

        assertTrue((listSize + remainingBits).equals(laneAttributes.encodeUPER()));

        laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        laneAttributes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "111";
        String remainingBits = "";

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MAX_LIST_SIZE);

        LaneDataAttribute[] laneAttributeArray = laneAttributes.getAttributeArray();
        for (int i = 0; i < laneAttributeArray.length; i++) {

            LaneDataAttribute laneAttribute = new LaneDataAttribute(new RoadwayCrownAngle(38), 0);
            laneAttributeArray[i] = laneAttribute;
            remainingBits += laneAttribute.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(laneAttributes.encodeUPER()));

        laneAttributes = new LaneDataAttributeList(LaneDataAttributeList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        laneAttributes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList();
        thrown.expect(IllegalStateException.class);
        laneAttributes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        LaneDataAttribute laneAttribute = new LaneDataAttribute(new RoadwayCrownAngle(98), 1);
        String listSize = "000";
        String remainingBits = laneAttribute.encodeUPER();

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList();
        laneAttributes.decodeUPER(listSize + remainingBits);
        LaneDataAttribute[] laneAttributeArray = laneAttributes.getAttributeArray();
        assertTrue(LaneDataAttributeList.MIN_LIST_SIZE == laneAttributeArray.length);
        assertTrue(laneAttribute.equals(laneAttributeArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        LaneDataAttribute laneAttribute = new LaneDataAttribute(new RoadwayCrownAngle(1), 0);
        LaneDataAttribute laneAttribute2 = new LaneDataAttribute(new RoadwayCrownAngle(2), 2);

        String listSize = "111";
        String remainingBits = laneAttribute.encodeUPER();

        for (int i = 0; i < LaneDataAttributeList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += laneAttribute2.encodeUPER();
        }

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList();
        laneAttributes.decodeUPER(listSize + remainingBits);

        LaneDataAttribute[] laneAttributeArray = laneAttributes.getAttributeArray();
        assertTrue(LaneDataAttributeList.MAX_LIST_SIZE == laneAttributeArray.length);
        assertTrue(laneAttribute.equals(laneAttributeArray[0]));
        for (int i = 1; i < LaneDataAttributeList.MAX_LIST_SIZE; i++) {

            assertTrue(laneAttribute2.equals(laneAttributeArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList();
        thrown.expect(IllegalArgumentException.class);
        laneAttributes.decodeUPER("11");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList();
        thrown.expect(IllegalArgumentException.class);
        // 111 = 8 objects, but there's none
        laneAttributes.decodeUPER("1110011010100");
    }

    @Test
    public void testHashCode() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList(1);
        laneAttributes.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(1));

        assertTrue(laneAttributes.hashCode() == laneAttributes.hashCode());

        LaneDataAttributeList laneAttributes2 = new LaneDataAttributeList(2);

        assertFalse(laneAttributes.hashCode() == laneAttributes2.hashCode());

        laneAttributes2 = new LaneDataAttributeList(1);
        laneAttributes2.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(2));

        assertFalse(laneAttributes.hashCode() == laneAttributes2.hashCode());

        laneAttributes2.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(1));

        assertTrue(laneAttributes.hashCode() == laneAttributes2.hashCode());
    }

    @Test
    public void testEquals() {

        LaneDataAttributeList laneAttributes = new LaneDataAttributeList(1);
        laneAttributes.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(1));

        assertTrue(laneAttributes.equals(laneAttributes));
        assertFalse(laneAttributes.equals(null));
        assertFalse(laneAttributes.equals(new String()));

        LaneDataAttributeList laneAttributes2 = new LaneDataAttributeList(2);

        assertFalse(laneAttributes.equals(laneAttributes2));

        laneAttributes2 = new LaneDataAttributeList(1);
        laneAttributes2.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(2));

        assertFalse(laneAttributes.equals(laneAttributes2));

        laneAttributes2.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(1));

        assertTrue(laneAttributes.equals(laneAttributes2));
    }
}
