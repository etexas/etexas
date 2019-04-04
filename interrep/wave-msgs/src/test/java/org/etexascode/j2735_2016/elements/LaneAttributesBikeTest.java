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
 * Unit tests for the lane attributes bike element.
 * 
 * @author ttevendale
 */
public class LaneAttributesBikeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesBike attributes = new LaneAttributesBike();

        // min
        assertTrue("0000000000000000".equals(attributes.encodeUPER()));

        attributes.setBikeFlyOverLane(true);
        attributes.setBiDirectionalCycleTimes(true);
        attributes.setUnsignalizedSegmentsPresent(true);

        // some turned on
        assertTrue("0010101000000000".equals(attributes.encodeUPER()));

        attributes.setBikeRevocableLane(true);
        attributes.setPedestrianUseAllowed(true);
        attributes.setFixedCycleTime(true);
        attributes.setIsolatedByBarrier(true);

        // max
        assertTrue("1111111000000000".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesBike attributes = new LaneAttributesBike();

        // min
        String remainingBits = attributes.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isBikeRevocableLane());
        assertFalse(attributes.isPedestrianUseAllowed());
        assertFalse(attributes.isBikeFlyOverLane());
        assertFalse(attributes.isFixedCycleTime());
        assertFalse(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.isIsolatedByBarrier());
        assertFalse(attributes.isUnsignalizedSegmentsPresent());

        attributes.setBikeRevocableLane(true);
        attributes.setBiDirectionalCycleTimes(true);
        attributes.setUnsignalizedSegmentsPresent(true);

        // some turned on
        remainingBits = attributes.decodeUPER("1000101000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isBikeRevocableLane());
        assertFalse(attributes.isPedestrianUseAllowed());
        assertFalse(attributes.isBikeFlyOverLane());
        assertFalse(attributes.isFixedCycleTime());
        assertTrue(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.isIsolatedByBarrier());
        assertTrue(attributes.isUnsignalizedSegmentsPresent());

        attributes.setPedestrianUseAllowed(true);
        attributes.setBikeFlyOverLane(true);
        attributes.setFixedCycleTime(true);
        attributes.setIsolatedByBarrier(true);

        // max
        remainingBits = attributes.decodeUPER("1111111000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isBikeRevocableLane());
        assertTrue(attributes.isPedestrianUseAllowed());
        assertTrue(attributes.isBikeFlyOverLane());
        assertTrue(attributes.isFixedCycleTime());
        assertTrue(attributes.isBiDirectionalCycleTimes());
        assertTrue(attributes.isIsolatedByBarrier());
        assertTrue(attributes.isUnsignalizedSegmentsPresent());
    }

    @Test
    public void testDecodeUPERRservedBit7() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000100000000");
    }

    @Test
    public void testDecodeUPERRservedBit8() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000010000000");
    }

    @Test
    public void testDecodeUPERRservedBit9() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000001000000");
    }

    @Test
    public void testDecodeUPERRservedBit10() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000100000");
    }

    @Test
    public void testDecodeUPERRservedBit11() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000010000");
    }

    @Test
    public void testDecodeUPERRservedBit12() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000001000");
    }

    @Test
    public void testDecodeUPERRservedBit13() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000100");
    }

    @Test
    public void testDecodeUPERRservedBit14() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERRservedBit15() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        String remainingBits = attributes.decodeUPER("1110001000000000110");
        assertTrue("110".equals(remainingBits));
        assertTrue(attributes.isBikeRevocableLane());
        assertTrue(attributes.isPedestrianUseAllowed());
        assertTrue(attributes.isBikeFlyOverLane());
        assertFalse(attributes.isFixedCycleTime());
        assertFalse(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.isIsolatedByBarrier());
        assertTrue(attributes.isUnsignalizedSegmentsPresent());
    }

    @Test
    public void testHashCode() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        attributes.setFixedCycleTime(true);
        attributes.setBiDirectionalCycleTimes(true);

        LaneAttributesBike attributes2 = new LaneAttributesBike();
        attributes2.setFixedCycleTime(false);
        attributes2.setBiDirectionalCycleTimes(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesBike attributes3 = new LaneAttributesBike();
        attributes3.setBikeRevocableLane(attributes.isBikeRevocableLane());
        attributes3.setPedestrianUseAllowed(attributes.isPedestrianUseAllowed());
        attributes3.setBikeFlyOverLane(attributes.isBikeFlyOverLane());
        attributes3.setFixedCycleTime(attributes.isFixedCycleTime());
        attributes3.setBiDirectionalCycleTimes(attributes.isBiDirectionalCycleTimes());
        attributes3.setIsolatedByBarrier(attributes.isIsolatedByBarrier());
        attributes3.setUnsignalizedSegmentsPresent(attributes.isUnsignalizedSegmentsPresent());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesBike attributes = new LaneAttributesBike();
        attributes.setBikeRevocableLane(true);
        attributes.setPedestrianUseAllowed(true);
        attributes.setBikeFlyOverLane(true);
        attributes.setFixedCycleTime(true);
        attributes.setBiDirectionalCycleTimes(true);
        attributes.setIsolatedByBarrier(true);
        attributes.setUnsignalizedSegmentsPresent(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesBike attributes2 = new LaneAttributesBike();
        attributes2.setBikeRevocableLane(false);
        attributes2.setPedestrianUseAllowed(false);
        attributes2.setBikeFlyOverLane(false);
        attributes2.setFixedCycleTime(false);
        attributes2.setBiDirectionalCycleTimes(false);
        attributes2.setIsolatedByBarrier(false);
        attributes2.setUnsignalizedSegmentsPresent(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setBikeRevocableLane(attributes.isBikeRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setPedestrianUseAllowed(attributes.isPedestrianUseAllowed());
        assertFalse(attributes.equals(attributes2));

        attributes2.setBikeFlyOverLane(attributes.isBikeFlyOverLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setFixedCycleTime(attributes.isFixedCycleTime());
        assertFalse(attributes.equals(attributes2));

        attributes2.setBiDirectionalCycleTimes(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.equals(attributes2));

        attributes2.setIsolatedByBarrier(attributes.isIsolatedByBarrier());
        assertFalse(attributes.equals(attributes2));

        attributes2.setUnsignalizedSegmentsPresent(attributes.isUnsignalizedSegmentsPresent());
        assertTrue(attributes.equals(attributes2));

    }
}
