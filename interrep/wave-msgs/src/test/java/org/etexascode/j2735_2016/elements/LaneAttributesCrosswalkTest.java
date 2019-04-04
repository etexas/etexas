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
 * Unit tests for the lane attributes crosswalk element.
 * 
 * @author ttevendale
 */
public class LaneAttributesCrosswalkTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();

        // min
        assertTrue("0000000000000000".equals(attributes.encodeUPER()));

        attributes.setBiDirectionalCycleTimes(true);
        attributes.setPushToWalkButton(true);
        attributes.setUnsignalizedSegmentsPresent(true);

        // some turned on
        assertTrue("0000110010000000".equals(attributes.encodeUPER()));

        attributes.setCrosswalkRevocableLane(true);
        attributes.setBicycleUseAllowed(true);
        attributes.setXWalkFlyOverLane(true);
        attributes.setFixedCycleTime(true);
        attributes.setAudioSupport(true);
        attributes.setRfSignalRequestPresent(true);

        // max
        assertTrue("1111111110000000".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();

        // min
        String remainingBits = attributes.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isCrosswalkRevocableLane());
        assertFalse(attributes.isBicycleUseAllowed());
        assertFalse(attributes.isXWalkFlyOverLane());
        assertFalse(attributes.isFixedCycleTime());
        assertFalse(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.hasPushToWalkButton());
        assertFalse(attributes.hasAudioSupport());
        assertFalse(attributes.isRfSignalRequestPresent());
        assertFalse(attributes.isUnsignalizedSegmentsPresent());

        // some turned on
        remainingBits = attributes.decodeUPER("0101001000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isCrosswalkRevocableLane());
        assertTrue(attributes.isBicycleUseAllowed());
        assertFalse(attributes.isXWalkFlyOverLane());
        assertTrue(attributes.isFixedCycleTime());
        assertFalse(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.hasPushToWalkButton());
        assertTrue(attributes.hasAudioSupport());
        assertFalse(attributes.isRfSignalRequestPresent());
        assertFalse(attributes.isUnsignalizedSegmentsPresent());

        // max
        remainingBits = attributes.decodeUPER("1111111110000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isCrosswalkRevocableLane());
        assertTrue(attributes.isBicycleUseAllowed());
        assertTrue(attributes.isXWalkFlyOverLane());
        assertTrue(attributes.isFixedCycleTime());
        assertTrue(attributes.isBiDirectionalCycleTimes());
        assertTrue(attributes.hasPushToWalkButton());
        assertTrue(attributes.hasAudioSupport());
        assertTrue(attributes.isRfSignalRequestPresent());
        assertTrue(attributes.isUnsignalizedSegmentsPresent());
    }

    @Test
    public void testDecodeUPERRservedBit9() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000001000000");
    }

    @Test
    public void testDecodeUPERRservedBit10() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000100000");
    }

    @Test
    public void testDecodeUPERRservedBit11() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000010000");
    }

    @Test
    public void testDecodeUPERRservedBit12() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000001000");
    }

    @Test
    public void testDecodeUPERRservedBit13() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000100");
    }

    @Test
    public void testDecodeUPERRservedBit14() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERRservedBit15() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        String remainingBits = attributes.decodeUPER("0111001000000000010");
        assertTrue("010".equals(remainingBits));
        assertFalse(attributes.isCrosswalkRevocableLane());
        assertTrue(attributes.isBicycleUseAllowed());
        assertTrue(attributes.isXWalkFlyOverLane());
        assertTrue(attributes.isFixedCycleTime());
        assertFalse(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.hasPushToWalkButton());
        assertTrue(attributes.hasAudioSupport());
        assertFalse(attributes.isRfSignalRequestPresent());
        assertFalse(attributes.isUnsignalizedSegmentsPresent());
    }

    @Test
    public void testHashCode() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        attributes.setFixedCycleTime(true);
        attributes.setBiDirectionalCycleTimes(true);

        LaneAttributesCrosswalk attributes2 = new LaneAttributesCrosswalk();
        attributes2.setFixedCycleTime(false);
        attributes2.setBiDirectionalCycleTimes(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesCrosswalk attributes3 = new LaneAttributesCrosswalk();
        attributes3.setCrosswalkRevocableLane(attributes.isCrosswalkRevocableLane());
        attributes3.setBicycleUseAllowed(attributes.isBicycleUseAllowed());
        attributes3.setXWalkFlyOverLane(attributes.isXWalkFlyOverLane());
        attributes3.setFixedCycleTime(attributes.isFixedCycleTime());
        attributes3.setBiDirectionalCycleTimes(attributes.isBiDirectionalCycleTimes());
        attributes3.setPushToWalkButton(attributes.hasPushToWalkButton());
        attributes3.setAudioSupport(attributes.hasAudioSupport());
        attributes3.setRfSignalRequestPresent(attributes.isRfSignalRequestPresent());
        attributes3.setUnsignalizedSegmentsPresent(attributes.isUnsignalizedSegmentsPresent());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesCrosswalk attributes = new LaneAttributesCrosswalk();
        attributes.setCrosswalkRevocableLane(true);
        attributes.setBicycleUseAllowed(true);
        attributes.setXWalkFlyOverLane(true);
        attributes.setFixedCycleTime(true);
        attributes.setBiDirectionalCycleTimes(true);
        attributes.setPushToWalkButton(true);
        attributes.setAudioSupport(true);
        attributes.setRfSignalRequestPresent(true);
        attributes.setUnsignalizedSegmentsPresent(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesCrosswalk attributes2 = new LaneAttributesCrosswalk();
        attributes2.setCrosswalkRevocableLane(false);
        attributes2.setBicycleUseAllowed(false);
        attributes2.setXWalkFlyOverLane(false);
        attributes2.setFixedCycleTime(false);
        attributes2.setBiDirectionalCycleTimes(false);
        attributes2.setPushToWalkButton(false);
        attributes2.setAudioSupport(false);
        attributes2.setRfSignalRequestPresent(false);
        attributes2.setUnsignalizedSegmentsPresent(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setCrosswalkRevocableLane(attributes.isCrosswalkRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setBicycleUseAllowed(attributes.isBicycleUseAllowed());
        assertFalse(attributes.equals(attributes2));

        attributes2.setXWalkFlyOverLane(attributes.isXWalkFlyOverLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setFixedCycleTime(attributes.isFixedCycleTime());
        assertFalse(attributes.equals(attributes2));

        attributes2.setBiDirectionalCycleTimes(attributes.isBiDirectionalCycleTimes());
        assertFalse(attributes.equals(attributes2));

        attributes2.setPushToWalkButton(attributes.hasPushToWalkButton());
        assertFalse(attributes.equals(attributes2));

        attributes2.setAudioSupport(attributes.hasAudioSupport());
        assertFalse(attributes.equals(attributes2));

        attributes2.setRfSignalRequestPresent(attributes.isRfSignalRequestPresent());
        assertFalse(attributes.equals(attributes2));

        attributes2.setUnsignalizedSegmentsPresent(attributes.isUnsignalizedSegmentsPresent());
        assertTrue(attributes.equals(attributes2));
    }
}
