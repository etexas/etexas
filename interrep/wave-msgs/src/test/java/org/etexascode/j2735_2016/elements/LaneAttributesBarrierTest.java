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
 * Unit tests for the lane attributes barrier element.
 * 
 * @author ttevendale
 */
public class LaneAttributesBarrierTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();

        // min
        assertTrue("0000000000000000".equals(attributes.encodeUPER()));

        attributes.setWhiteLineHashing(true);
        attributes.setDoubleStripedLines(true);
        attributes.setConstructionBarrier(true);
        attributes.setTrafficChannels(true);

        // some turned on
        assertTrue("0010101100000000".equals(attributes.encodeUPER()));

        attributes.setMedianRevocableLane(true);
        attributes.setMedian(true);
        attributes.setStripedLines(true);
        attributes.setTrafficCones(true);
        attributes.setLowCurbs(true);
        attributes.setHighCurbs(true);

        // max
        assertTrue("1111111111000000".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();

        // min
        String remainingBits = attributes.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isMedianRevocableLane());
        assertFalse(attributes.isMedian());
        assertFalse(attributes.isWhiteLineHashing());
        assertFalse(attributes.isStripedLines());
        assertFalse(attributes.isDoubleStripedLines());
        assertFalse(attributes.isTrafficCones());
        assertFalse(attributes.isConstructionBarrier());
        assertFalse(attributes.isTrafficChannels());
        assertFalse(attributes.isLowCurbs());
        assertFalse(attributes.isHighCurbs());

        attributes.setMedianRevocableLane(true);
        attributes.setDoubleStripedLines(true);
        attributes.setConstructionBarrier(true);

        // some turned on
        remainingBits = attributes.decodeUPER("1000101000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isMedianRevocableLane());
        assertFalse(attributes.isMedian());
        assertFalse(attributes.isWhiteLineHashing());
        assertFalse(attributes.isStripedLines());
        assertTrue(attributes.isDoubleStripedLines());
        assertFalse(attributes.isTrafficCones());
        assertTrue(attributes.isConstructionBarrier());
        assertFalse(attributes.isTrafficChannels());
        assertFalse(attributes.isLowCurbs());
        assertFalse(attributes.isHighCurbs());

        attributes.setMedian(true);
        attributes.setWhiteLineHashing(true);
        attributes.setStripedLines(true);
        attributes.setTrafficCones(true);
        attributes.setTrafficChannels(true);
        attributes.setLowCurbs(true);
        attributes.setHighCurbs(true);

        // max
        remainingBits = attributes.decodeUPER("1111111111000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isMedianRevocableLane());
        assertTrue(attributes.isMedian());
        assertTrue(attributes.isWhiteLineHashing());
        assertTrue(attributes.isStripedLines());
        assertTrue(attributes.isDoubleStripedLines());
        assertTrue(attributes.isTrafficCones());
        assertTrue(attributes.isConstructionBarrier());
        assertTrue(attributes.isTrafficChannels());
        assertTrue(attributes.isLowCurbs());
        assertTrue(attributes.isHighCurbs());
    }

    @Test
    public void testDecodeUPERRservedBit10() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000100000");
    }

    @Test
    public void testDecodeUPERRservedBit11() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000010000");
    }

    @Test
    public void testDecodeUPERRservedBit12() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000001000");
    }

    @Test
    public void testDecodeUPERRservedBit13() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000100");
    }

    @Test
    public void testDecodeUPERRservedBit14() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERRservedBit15() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        String remainingBits = attributes.decodeUPER("010100001000000000001");
        assertTrue("00001".equals(remainingBits));
        assertFalse(attributes.isMedianRevocableLane());
        assertTrue(attributes.isMedian());
        assertFalse(attributes.isWhiteLineHashing());
        assertTrue(attributes.isStripedLines());
        assertFalse(attributes.isDoubleStripedLines());
        assertFalse(attributes.isTrafficCones());
        assertFalse(attributes.isConstructionBarrier());
        assertFalse(attributes.isTrafficChannels());
        assertTrue(attributes.isLowCurbs());
        assertFalse(attributes.isHighCurbs());
    }

    @Test
    public void testHashCode() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        attributes.setMedianRevocableLane(true);
        attributes.setTrafficChannels(true);

        LaneAttributesBarrier attributes2 = new LaneAttributesBarrier();
        attributes2.setMedianRevocableLane(false);
        attributes2.setTrafficChannels(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesBarrier attributes3 = new LaneAttributesBarrier();
        attributes3.setMedianRevocableLane(attributes.isMedianRevocableLane());
        attributes3.setMedian(attributes.isMedian());
        attributes3.setWhiteLineHashing(attributes.isWhiteLineHashing());
        attributes3.setStripedLines(attributes.isStripedLines());
        attributes3.setDoubleStripedLines(attributes.isDoubleStripedLines());
        attributes3.setTrafficCones(attributes.isTrafficCones());
        attributes3.setConstructionBarrier(attributes.isConstructionBarrier());
        attributes3.setTrafficChannels(attributes.isTrafficChannels());
        attributes3.setLowCurbs(attributes.isLowCurbs());
        attributes3.setHighCurbs(attributes.isHighCurbs());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesBarrier attributes = new LaneAttributesBarrier();
        attributes.setMedianRevocableLane(true);
        attributes.setMedian(true);
        attributes.setWhiteLineHashing(true);
        attributes.setStripedLines(true);
        attributes.setDoubleStripedLines(true);
        attributes.setTrafficCones(true);
        attributes.setConstructionBarrier(true);
        attributes.setTrafficChannels(true);
        attributes.setLowCurbs(true);
        attributes.setHighCurbs(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesBarrier attributes2 = new LaneAttributesBarrier();
        attributes2.setMedianRevocableLane(false);
        attributes2.setMedian(false);
        attributes2.setWhiteLineHashing(false);
        attributes2.setStripedLines(false);
        attributes2.setDoubleStripedLines(false);
        attributes2.setTrafficCones(false);
        attributes2.setConstructionBarrier(false);
        attributes2.setTrafficChannels(false);
        attributes2.setLowCurbs(false);
        attributes2.setHighCurbs(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setMedianRevocableLane(attributes.isMedianRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setMedian(attributes.isMedian());
        assertFalse(attributes.equals(attributes2));

        attributes2.setWhiteLineHashing(attributes.isWhiteLineHashing());
        assertFalse(attributes.equals(attributes2));

        attributes2.setStripedLines(attributes.isStripedLines());
        assertFalse(attributes.equals(attributes2));

        attributes2.setDoubleStripedLines(attributes.isDoubleStripedLines());
        assertFalse(attributes.equals(attributes2));

        attributes2.setTrafficCones(attributes.isTrafficCones());
        assertFalse(attributes.equals(attributes2));

        attributes2.setConstructionBarrier(attributes.isConstructionBarrier());
        assertFalse(attributes.equals(attributes2));

        attributes2.setTrafficChannels(attributes.isTrafficChannels());
        assertFalse(attributes.equals(attributes2));

        attributes2.setLowCurbs(attributes.isLowCurbs());
        assertFalse(attributes.equals(attributes2));

        attributes2.setHighCurbs(attributes.isHighCurbs());
        assertTrue(attributes.equals(attributes2));
    }
}
