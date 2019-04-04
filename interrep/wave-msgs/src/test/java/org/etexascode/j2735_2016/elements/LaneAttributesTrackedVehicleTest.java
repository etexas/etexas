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
 * Unit tests for the lane attributes tracked vehicle element.
 * 
 * @author ttevendale
 */
public class LaneAttributesTrackedVehicleTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();

        // min
        assertTrue("0000000000000000".equals(attributes.encodeUPER()));

        attributes.setSpecRevocableLane(true);
        attributes.setSpecLightRailRoadTrack(true);
        attributes.setSpecHeavyRailRoadTrack(true);

        // some turned on
        assertTrue("1011000000000000".equals(attributes.encodeUPER()));

        attributes.setSpecCommuterRailRoadTrack(true);
        attributes.setSpecOtherRailType(true);

        // max
        assertTrue("1111100000000000".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();

        // min
        String remainingBits = attributes.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isSpecRevocableLane());
        assertFalse(attributes.isSpecCommuterRailRoadTrack());
        assertFalse(attributes.isSpecLightRailRoadTrack());
        assertFalse(attributes.isSpecHeavyRailRoadTrack());
        assertFalse(attributes.isSpecOtherRailType());

        // some turned on
        remainingBits = attributes.decodeUPER("1100100000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isSpecRevocableLane());
        assertTrue(attributes.isSpecCommuterRailRoadTrack());
        assertFalse(attributes.isSpecLightRailRoadTrack());
        assertFalse(attributes.isSpecHeavyRailRoadTrack());
        assertTrue(attributes.isSpecOtherRailType());

        // max
        remainingBits = attributes.decodeUPER("1111100000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isSpecRevocableLane());
        assertTrue(attributes.isSpecCommuterRailRoadTrack());
        assertTrue(attributes.isSpecLightRailRoadTrack());
        assertTrue(attributes.isSpecHeavyRailRoadTrack());
        assertTrue(attributes.isSpecOtherRailType());
    }

    @Test
    public void testDecodeUPERRservedBit5() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000010000000000");
    }

    @Test
    public void testDecodeUPERRservedBit6() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000001000000000");
    }

    @Test
    public void testDecodeUPERRservedBit7() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000100000000");
    }

    @Test
    public void testDecodeUPERRservedBit8() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000010000000");
    }

    @Test
    public void testDecodeUPERRservedBit9() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000001000000");
    }

    @Test
    public void testDecodeUPERRservedBit10() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000100000");
    }

    @Test
    public void testDecodeUPERRservedBit11() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000010000");
    }

    @Test
    public void testDecodeUPERRservedBit12() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000001000");
    }

    @Test
    public void testDecodeUPERRservedBit13() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000100");
    }

    @Test
    public void testDecodeUPERRservedBit14() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERRservedBit15() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        String remainingBits = attributes.decodeUPER("010110000000000010100");
        assertTrue("10100".equals(remainingBits));
        assertFalse(attributes.isSpecRevocableLane());
        assertTrue(attributes.isSpecCommuterRailRoadTrack());
        assertFalse(attributes.isSpecLightRailRoadTrack());
        assertTrue(attributes.isSpecHeavyRailRoadTrack());
        assertTrue(attributes.isSpecOtherRailType());
    }

    @Test
    public void testHashCode() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        attributes.setSpecOtherRailType(true);

        LaneAttributesTrackedVehicle attributes2 = new LaneAttributesTrackedVehicle();
        attributes2.setSpecOtherRailType(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesTrackedVehicle attributes3 = new LaneAttributesTrackedVehicle();
        attributes3.setSpecRevocableLane(attributes.isSpecRevocableLane());
        attributes3.setSpecCommuterRailRoadTrack(attributes.isSpecCommuterRailRoadTrack());
        attributes3.setSpecLightRailRoadTrack(attributes.isSpecLightRailRoadTrack());
        attributes3.setSpecHeavyRailRoadTrack(attributes.isSpecHeavyRailRoadTrack());
        attributes3.setSpecOtherRailType(attributes.isSpecOtherRailType());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesTrackedVehicle attributes = new LaneAttributesTrackedVehicle();
        attributes.setSpecRevocableLane(true);
        attributes.setSpecCommuterRailRoadTrack(true);
        attributes.setSpecLightRailRoadTrack(true);
        attributes.setSpecHeavyRailRoadTrack(true);
        attributes.setSpecOtherRailType(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesTrackedVehicle attributes2 = new LaneAttributesTrackedVehicle();
        attributes2.setSpecRevocableLane(false);
        attributes2.setSpecCommuterRailRoadTrack(false);
        attributes2.setSpecLightRailRoadTrack(false);
        attributes2.setSpecHeavyRailRoadTrack(false);
        attributes2.setSpecOtherRailType(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setSpecRevocableLane(attributes.isSpecRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setSpecCommuterRailRoadTrack(attributes.isSpecCommuterRailRoadTrack());
        assertFalse(attributes.equals(attributes2));

        attributes2.setSpecLightRailRoadTrack(attributes.isSpecLightRailRoadTrack());
        assertFalse(attributes.equals(attributes2));

        attributes2.setSpecHeavyRailRoadTrack(attributes.isSpecHeavyRailRoadTrack());
        assertFalse(attributes.equals(attributes2));

        attributes2.setSpecOtherRailType(attributes.isSpecOtherRailType());
        assertTrue(attributes.equals(attributes2));
    }
}
