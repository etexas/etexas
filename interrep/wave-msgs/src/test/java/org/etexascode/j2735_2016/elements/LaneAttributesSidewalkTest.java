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
package org.etexascode.j2735_2016.elements;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the lane attributes sidewalk element.
 * 
 * @author ttevendale
 */
public class LaneAttributesSidewalkTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();

        // min
        assertTrue("0000000000000000".equals(attributes.encodeUPER()));

        attributes.setSidewalkFlyOverLane(true);

        // some turned on
        assertTrue("0010000000000000".equals(attributes.encodeUPER()));

        attributes.setSidewalkRevocableLane(true);
        attributes.setBicycleUseAllowed(true);
        attributes.setWalkBikes(true);

        // max
        assertTrue("1111000000000000".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();

        // min
        String remainingBits = attributes.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isSidewalkRevocableLane());
        assertFalse(attributes.isBicycleUseAllowed());
        assertFalse(attributes.isSidewalkFlyOverLane());
        assertFalse(attributes.isWalkBikes());

        // some turned on
        remainingBits = attributes.decodeUPER("0100000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isSidewalkRevocableLane());
        assertTrue(attributes.isBicycleUseAllowed());
        assertFalse(attributes.isSidewalkFlyOverLane());
        assertFalse(attributes.isWalkBikes());

        // max
        remainingBits = attributes.decodeUPER("1111000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isSidewalkRevocableLane());
        assertTrue(attributes.isBicycleUseAllowed());
        assertTrue(attributes.isSidewalkFlyOverLane());
        assertTrue(attributes.isWalkBikes());
    }

    @Test
    public void testDecodeUPERRservedBit4() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000100000000000");
    }

    @Test
    public void testDecodeUPERRservedBit5() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000010000000000");
    }

    @Test
    public void testDecodeUPERRservedBit6() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000001000000000");
    }

    @Test
    public void testDecodeUPERRservedBit7() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000100000000");
    }

    @Test
    public void testDecodeUPERRservedBit8() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000010000000");
    }

    @Test
    public void testDecodeUPERRservedBit9() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000001000000");
    }

    @Test
    public void testDecodeUPERRservedBit10() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000100000");
    }

    @Test
    public void testDecodeUPERRservedBit11() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000010000");
    }

    @Test
    public void testDecodeUPERRservedBit12() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000001000");
    }

    @Test
    public void testDecodeUPERRservedBit13() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000100");
    }

    @Test
    public void testDecodeUPERRservedBit14() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERRservedBit15() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        String remainingBits = attributes.decodeUPER("0101000000000000111");
        assertTrue("111".equals(remainingBits));
        assertFalse(attributes.isSidewalkRevocableLane());
        assertTrue(attributes.isBicycleUseAllowed());
        assertFalse(attributes.isSidewalkFlyOverLane());
        assertTrue(attributes.isWalkBikes());
    }

    @Test
    public void testHashCode() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        attributes.setSidewalkRevocableLane(true);

        LaneAttributesSidewalk attributes2 = new LaneAttributesSidewalk();
        attributes2.setSidewalkRevocableLane(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesSidewalk attributes3 = new LaneAttributesSidewalk();
        attributes3.setSidewalkRevocableLane(attributes.isSidewalkRevocableLane());
        attributes3.setBicycleUseAllowed(attributes.isBicycleUseAllowed());
        attributes3.setSidewalkFlyOverLane(attributes.isSidewalkFlyOverLane());
        attributes3.setWalkBikes(attributes.isWalkBikes());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesSidewalk attributes = new LaneAttributesSidewalk();
        attributes.setSidewalkRevocableLane(true);
        attributes.setBicycleUseAllowed(true);
        attributes.setSidewalkFlyOverLane(true);
        attributes.setWalkBikes(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesSidewalk attributes2 = new LaneAttributesSidewalk();
        attributes2.setSidewalkRevocableLane(false);
        attributes2.setBicycleUseAllowed(false);
        attributes2.setSidewalkFlyOverLane(false);
        attributes2.setWalkBikes(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setSidewalkRevocableLane(attributes.isSidewalkRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setBicycleUseAllowed(attributes.isBicycleUseAllowed());
        assertFalse(attributes.equals(attributes2));

        attributes2.setSidewalkFlyOverLane(attributes.isSidewalkFlyOverLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setWalkBikes(attributes.isWalkBikes());
        assertTrue(attributes.equals(attributes2));
    }
}
