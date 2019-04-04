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
 * Unit tests for the lane attributes vehicle element.
 * 
 * @author ttevendale
 */
public class LaneAttributesVehicleTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneAttributesVehicle attributes = new LaneAttributesVehicle();

        // min
        assertTrue("000000000".equals(attributes.encodeUPER()));

        attributes.setVehicleRevocableLane(true);
        attributes.setHovLaneUseOnly(true);
        attributes.setIRbeaconCoverage(true);
        attributes.setPermissionOnRequest(true);

        // some turned on
        assertTrue("010100011".equals(attributes.encodeUPER()));

        attributes.setVehicleFlyOverLane(true);
        attributes.setRestrictedToBusUse(true);
        attributes.setRestrictedToTaxiUse(true);
        attributes.setRestrictedFromPublicUse(true);

        // max
        assertTrue("011111111".equals(attributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributesVehicle attributes = new LaneAttributesVehicle();

        // min
        String remainingBits = attributes.decodeUPER("000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(attributes.isVehicleRevocableLane());
        assertFalse(attributes.isVehicleFlyOverLane());
        assertFalse(attributes.isHovLaneUseOnly());
        assertFalse(attributes.isRestrictedToBusUse());
        assertFalse(attributes.isRestrictedToTaxiUse());
        assertFalse(attributes.isRestrictedFromPublicUse());
        assertFalse(attributes.hasIRbeaconCoverage());
        assertFalse(attributes.hasPermissionOnRequest());

        // some turned on
        remainingBits = attributes.decodeUPER("011000011");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isVehicleRevocableLane());
        assertTrue(attributes.isVehicleFlyOverLane());
        assertFalse(attributes.isHovLaneUseOnly());
        assertFalse(attributes.isRestrictedToBusUse());
        assertFalse(attributes.isRestrictedToTaxiUse());
        assertFalse(attributes.isRestrictedFromPublicUse());
        assertTrue(attributes.hasIRbeaconCoverage());
        assertTrue(attributes.hasPermissionOnRequest());

        // max
        remainingBits = attributes.decodeUPER("011111111");
        assertTrue("".equals(remainingBits));
        assertTrue(attributes.isVehicleRevocableLane());
        assertTrue(attributes.isVehicleFlyOverLane());
        assertTrue(attributes.isHovLaneUseOnly());
        assertTrue(attributes.isRestrictedToBusUse());
        assertTrue(attributes.isRestrictedToTaxiUse());
        assertTrue(attributes.isRestrictedFromPublicUse());
        assertTrue(attributes.hasIRbeaconCoverage());
        assertTrue(attributes.hasPermissionOnRequest());
    }

    @Test
    public void testDecodeUPERExtension() {

        LaneAttributesVehicle attributes = new LaneAttributesVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("100000000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneAttributesVehicle attributes = new LaneAttributesVehicle();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneAttributesVehicle attributes = new LaneAttributesVehicle();
        String remainingBits = attributes.decodeUPER("0010110011010");
        assertTrue("1010".equals(remainingBits));
        assertFalse(attributes.isVehicleRevocableLane());
        assertTrue(attributes.isVehicleFlyOverLane());
        assertFalse(attributes.isHovLaneUseOnly());
        assertTrue(attributes.isRestrictedToBusUse());
        assertTrue(attributes.isRestrictedToTaxiUse());
        assertFalse(attributes.isRestrictedFromPublicUse());
        assertFalse(attributes.hasIRbeaconCoverage());
        assertTrue(attributes.hasPermissionOnRequest());
    }

    @Test
    public void testHashCode() {

        LaneAttributesVehicle attributes = new LaneAttributesVehicle();
        attributes.setHovLaneUseOnly(true);
        attributes.setRestrictedFromPublicUse(true);

        LaneAttributesVehicle attributes2 = new LaneAttributesVehicle();
        attributes2.setHovLaneUseOnly(false);
        attributes2.setRestrictedFromPublicUse(false);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneAttributesVehicle attributes3 = new LaneAttributesVehicle();
        attributes3.setVehicleRevocableLane(attributes.isVehicleRevocableLane());
        attributes3.setVehicleFlyOverLane(attributes.isVehicleFlyOverLane());
        attributes3.setHovLaneUseOnly(attributes.isHovLaneUseOnly());
        attributes3.setRestrictedToBusUse(attributes.isRestrictedToBusUse());
        attributes3.setRestrictedToTaxiUse(attributes.isRestrictedToTaxiUse());
        attributes3.setRestrictedFromPublicUse(attributes.isRestrictedFromPublicUse());
        attributes3.setIRbeaconCoverage(attributes.hasIRbeaconCoverage());
        attributes3.setPermissionOnRequest(attributes.hasPermissionOnRequest());

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesVehicle attributes = new LaneAttributesVehicle();
        attributes.setVehicleRevocableLane(true);
        attributes.setVehicleFlyOverLane(true);
        attributes.setHovLaneUseOnly(true);
        attributes.setRestrictedToBusUse(true);
        attributes.setRestrictedToTaxiUse(true);
        attributes.setRestrictedFromPublicUse(true);
        attributes.setIRbeaconCoverage(true);
        attributes.setPermissionOnRequest(true);

        assertFalse(attributes.equals(null));

        assertTrue(attributes.equals(attributes));

        LaneAttributesVehicle attributes2 = new LaneAttributesVehicle();
        attributes2.setVehicleRevocableLane(false);
        attributes2.setVehicleFlyOverLane(false);
        attributes2.setHovLaneUseOnly(false);
        attributes2.setRestrictedToBusUse(false);
        attributes2.setRestrictedToTaxiUse(false);
        attributes2.setRestrictedFromPublicUse(false);
        attributes2.setIRbeaconCoverage(false);
        attributes2.setPermissionOnRequest(false);

        assertFalse(attributes.equals(new String()));
        assertFalse(attributes.equals(attributes2));

        attributes2.setVehicleRevocableLane(attributes.isVehicleRevocableLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setVehicleFlyOverLane(attributes.isVehicleFlyOverLane());
        assertFalse(attributes.equals(attributes2));

        attributes2.setHovLaneUseOnly(attributes.isHovLaneUseOnly());
        assertFalse(attributes.equals(attributes2));

        attributes2.setRestrictedToBusUse(attributes.isRestrictedToBusUse());
        assertFalse(attributes.equals(attributes2));

        attributes2.setRestrictedToTaxiUse(attributes.isRestrictedToTaxiUse());
        assertFalse(attributes.equals(attributes2));

        attributes2.setRestrictedFromPublicUse(attributes.isRestrictedFromPublicUse());
        assertFalse(attributes.equals(attributes2));

        attributes2.setIRbeaconCoverage(attributes.hasIRbeaconCoverage());
        assertFalse(attributes.equals(attributes2));

        attributes2.setPermissionOnRequest(attributes.hasPermissionOnRequest());
        assertTrue(attributes.equals(attributes2));
    }
}
