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
 * Unit tests for the lane attributes parking element.
 * 
 * @author ttevendale
 */
public class LaneAttributesParkingTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesParking attributes = new LaneAttributesParking();

        // min
        assertTrue("0000000000000000".equals(attributes.encodeUPER()));

        attributes.setParallelParkingInUse(true);
        attributes.setHeadInParkingInUse(true);
        attributes.setParkingForTaxiUse(true);

        // some turned on
        assertTrue("0110010000000000".equals(attributes.encodeUPER()));

        attributes.setParkingRevocableLane(true);
        attributes.setDoNotParkZone(true);
        attributes.setParkingForBusUse(true);
        attributes.setNoPublicParkingUse(true);

        // max
        assertTrue("1111111000000000".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesParking attributes = new LaneAttributesParking();

        // min
        String remainingBits = attributes.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isParkingRevocableLane());
        assertFalse(attributes.isParallelParkingInUse());
        assertFalse(attributes.isHeadInParkingInUse());
        assertFalse(attributes.isDoNotParkZone());
        assertFalse(attributes.isParkingForBusUse());
        assertFalse(attributes.isParkingForTaxiUse());
        assertFalse(attributes.isNoPublicParkingUse());

        // some turned on
        remainingBits = attributes.decodeUPER("0100010000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isParkingRevocableLane());
        assertTrue(attributes.isParallelParkingInUse());
        assertFalse(attributes.isHeadInParkingInUse());
        assertFalse(attributes.isDoNotParkZone());
        assertFalse(attributes.isParkingForBusUse());
        assertTrue(attributes.isParkingForTaxiUse());
        assertFalse(attributes.isNoPublicParkingUse());

        // max
        remainingBits = attributes.decodeUPER("1111111000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isParkingRevocableLane());
        assertTrue(attributes.isParallelParkingInUse());
        assertTrue(attributes.isHeadInParkingInUse());
        assertTrue(attributes.isDoNotParkZone());
        assertTrue(attributes.isParkingForBusUse());
        assertTrue(attributes.isParkingForTaxiUse());
        assertTrue(attributes.isNoPublicParkingUse());
    }

    @Test
    public void testDecodeUPERRservedBit7() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000100000000");
    }

    @Test
    public void testDecodeUPERRservedBit8() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000010000000");
    }

    @Test
    public void testDecodeUPERRservedBit9() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000001000000");
    }

    @Test
    public void testDecodeUPERRservedBit10() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000100000");
    }

    @Test
    public void testDecodeUPERRservedBit11() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000010000");
    }

    @Test
    public void testDecodeUPERRservedBit12() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000001000");
    }

    @Test
    public void testDecodeUPERRservedBit13() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000100");
    }

    @Test
    public void testDecodeUPERRservedBit14() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERRservedBit15() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        String remainingBits = attributes.decodeUPER("0101100000000000010");
        assertTrue("010".equals(remainingBits));
        assertFalse(attributes.isParkingRevocableLane());
        assertTrue(attributes.isParallelParkingInUse());
        assertFalse(attributes.isHeadInParkingInUse());
        assertTrue(attributes.isDoNotParkZone());
        assertTrue(attributes.isParkingForBusUse());
        assertFalse(attributes.isParkingForTaxiUse());
        assertFalse(attributes.isNoPublicParkingUse());
    }

    @Test
    public void testHashCode() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        attributes.setDoNotParkZone(true);
        attributes.setParkingForBusUse(true);

        LaneAttributesParking attributes2 = new LaneAttributesParking();
        attributes2.setDoNotParkZone(false);
        attributes2.setParkingForBusUse(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesParking attributes3 = new LaneAttributesParking();
        attributes3.setParkingRevocableLane(attributes.isParkingRevocableLane());
        attributes3.setParallelParkingInUse(attributes.isParallelParkingInUse());
        attributes3.setHeadInParkingInUse(attributes.isHeadInParkingInUse());
        attributes3.setDoNotParkZone(attributes.isDoNotParkZone());
        attributes3.setParkingForBusUse(attributes.isParkingForBusUse());
        attributes3.setParkingForTaxiUse(attributes.isParkingForTaxiUse());
        attributes3.setNoPublicParkingUse(attributes.isNoPublicParkingUse());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesParking attributes = new LaneAttributesParking();
        attributes.setParkingRevocableLane(true);
        attributes.setParallelParkingInUse(true);
        attributes.setHeadInParkingInUse(true);
        attributes.setDoNotParkZone(true);
        attributes.setParkingForBusUse(true);
        attributes.setParkingForTaxiUse(true);
        attributes.setNoPublicParkingUse(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesParking attributes2 = new LaneAttributesParking();
        attributes2.setParkingRevocableLane(false);
        attributes2.setParallelParkingInUse(false);
        attributes2.setHeadInParkingInUse(false);
        attributes2.setDoNotParkZone(false);
        attributes2.setParkingForBusUse(false);
        attributes2.setParkingForTaxiUse(false);
        attributes2.setNoPublicParkingUse(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setParkingRevocableLane(attributes.isParkingRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setParallelParkingInUse(attributes.isParallelParkingInUse());
        assertFalse(attributes.equals(attributes2));

        attributes2.setHeadInParkingInUse(attributes.isHeadInParkingInUse());
        assertFalse(attributes.equals(attributes2));

        attributes2.setDoNotParkZone(attributes.isDoNotParkZone());
        assertFalse(attributes.equals(attributes2));

        attributes2.setParkingForBusUse(attributes.isParkingForBusUse());
        assertFalse(attributes.equals(attributes2));

        attributes2.setParkingForTaxiUse(attributes.isParkingForTaxiUse());
        assertFalse(attributes.equals(attributes2));

        attributes2.setNoPublicParkingUse(attributes.isNoPublicParkingUse());
        assertTrue(attributes.equals(attributes2));
    }
}
