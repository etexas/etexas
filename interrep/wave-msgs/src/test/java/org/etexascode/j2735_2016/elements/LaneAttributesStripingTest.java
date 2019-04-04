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
 * Unit tests for the lane attributes striping element.
 * 
 * @author ttevendale
 */
public class LaneAttributesStripingTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();

        // min
        assertTrue("0000000000000000".equals(attributes.encodeUPER()));

        attributes.setStripeDrawOnRight(true);
        attributes.setStripeToConnectingLanesLeft(true);

        // some turned on
        assertTrue("0011000000000000".equals(attributes.encodeUPER()));

        attributes.setStripeToConnectingLanesRevocableLane(true);
        attributes.setStripeDrawOnLeft(true);
        attributes.setStripeToConnectingLanesRight(true);
        attributes.setStripeToConnectingLanesAhead(true);

        // max
        assertTrue("1111110000000000".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();

        // min
        String remainingBits = attributes.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isStripeToConnectingLanesRevocableLane());
        assertFalse(attributes.isStripeDrawOnLeft());
        assertFalse(attributes.isStripeDrawOnRight());
        assertFalse(attributes.isStripeToConnectingLanesLeft());
        assertFalse(attributes.isStripeToConnectingLanesRight());
        assertFalse(attributes.isStripeToConnectingLanesAhead());

        // some turned on
        remainingBits = attributes.decodeUPER("1100000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isStripeToConnectingLanesRevocableLane());
        assertTrue(attributes.isStripeDrawOnLeft());
        assertFalse(attributes.isStripeDrawOnRight());
        assertFalse(attributes.isStripeToConnectingLanesLeft());
        assertFalse(attributes.isStripeToConnectingLanesRight());
        assertFalse(attributes.isStripeToConnectingLanesAhead());

        // max
        remainingBits = attributes.decodeUPER("1111110000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isStripeToConnectingLanesRevocableLane());
        assertTrue(attributes.isStripeDrawOnLeft());
        assertTrue(attributes.isStripeDrawOnRight());
        assertTrue(attributes.isStripeToConnectingLanesLeft());
        assertTrue(attributes.isStripeToConnectingLanesRight());
        assertTrue(attributes.isStripeToConnectingLanesAhead());
    }

    @Test
    public void testDecodeUPERRservedBit6() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000001000000000");
    }

    @Test
    public void testDecodeUPERRservedBit7() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000100000000");
    }

    @Test
    public void testDecodeUPERRservedBit8() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000010000000");
    }

    @Test
    public void testDecodeUPERRservedBit9() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000001000000");
    }

    @Test
    public void testDecodeUPERRservedBit10() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000100000");
    }

    @Test
    public void testDecodeUPERRservedBit11() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000010000");
    }

    @Test
    public void testDecodeUPERRservedBit12() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000001000");
    }

    @Test
    public void testDecodeUPERRservedBit13() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000100");
    }

    @Test
    public void testDecodeUPERRservedBit14() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERRservedBit15() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        String remainingBits = attributes.decodeUPER("0101000000000000101");
        assertTrue("101".equals(remainingBits));
        assertFalse(attributes.isStripeToConnectingLanesRevocableLane());
        assertTrue(attributes.isStripeDrawOnLeft());
        assertFalse(attributes.isStripeDrawOnRight());
        assertTrue(attributes.isStripeToConnectingLanesLeft());
        assertFalse(attributes.isStripeToConnectingLanesRight());
        assertFalse(attributes.isStripeToConnectingLanesAhead());
    }

    @Test
    public void testHashCode() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        attributes.setStripeToConnectingLanesLeft(true);

        LaneAttributesStriping attributes2 = new LaneAttributesStriping();
        attributes2.setStripeToConnectingLanesLeft(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesStriping attributes3 = new LaneAttributesStriping();
        attributes3.setStripeToConnectingLanesRevocableLane(attributes.isStripeToConnectingLanesRevocableLane());
        attributes3.setStripeDrawOnLeft(attributes.isStripeDrawOnLeft());
        attributes3.setStripeDrawOnRight(attributes.isStripeDrawOnRight());
        attributes3.setStripeToConnectingLanesLeft(attributes.isStripeToConnectingLanesLeft());
        attributes3.setStripeToConnectingLanesRight(attributes.isStripeToConnectingLanesRight());
        attributes3.setStripeToConnectingLanesAhead(attributes.isStripeToConnectingLanesAhead());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesStriping attributes = new LaneAttributesStriping();
        attributes.setStripeToConnectingLanesRevocableLane(true);
        attributes.setStripeDrawOnLeft(true);
        attributes.setStripeDrawOnRight(true);
        attributes.setStripeToConnectingLanesLeft(true);
        attributes.setStripeToConnectingLanesRight(true);
        attributes.setStripeToConnectingLanesAhead(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesStriping attributes2 = new LaneAttributesStriping();
        attributes2.setStripeToConnectingLanesRevocableLane(false);
        attributes2.setStripeDrawOnLeft(false);
        attributes2.setStripeDrawOnRight(false);
        attributes2.setStripeToConnectingLanesLeft(false);
        attributes2.setStripeToConnectingLanesRight(false);
        attributes2.setStripeToConnectingLanesAhead(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setStripeToConnectingLanesRevocableLane(attributes.isStripeToConnectingLanesRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setStripeDrawOnLeft(attributes.isStripeDrawOnLeft());
        assertFalse(attributes.equals(attributes2));

        attributes2.setStripeDrawOnRight(attributes.isStripeDrawOnRight());
        assertFalse(attributes.equals(attributes2));

        attributes2.setStripeToConnectingLanesLeft(attributes.isStripeToConnectingLanesLeft());
        assertFalse(attributes.equals(attributes2));

        attributes2.setStripeToConnectingLanesRight(attributes.isStripeToConnectingLanesRight());
        assertFalse(attributes.equals(attributes2));

        attributes2.setStripeToConnectingLanesAhead(attributes.isStripeToConnectingLanesAhead());
        assertTrue(attributes.equals(attributes2));
    }
}
