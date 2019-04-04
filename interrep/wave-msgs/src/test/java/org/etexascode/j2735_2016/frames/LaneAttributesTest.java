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

import org.etexascode.j2735_2016.elements.LaneAttributesBike;
import org.etexascode.j2735_2016.elements.LaneAttributesParking;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LaneDirection;
import org.etexascode.j2735_2016.elements.LaneSharing;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the lane attributes frame.
 * 
 * @author ttevendale
 */
public class LaneAttributesTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    LaneAttributes laneAttributes;

    String encodedBits;

    @Before
    public void init() {

        LaneDirection directionalUse = new LaneDirection();
        directionalUse.setEgressPath(true);

        LaneSharing sharedWith = new LaneSharing();
        sharedWith.setBusVehicleTraffic(true);
        sharedWith.setMultipleLanesTreatedAsOneLaneStatus(true);

        LaneAttributesVehicle vehicle = new LaneAttributesVehicle();
        vehicle.setRestrictedFromPublicUse(true);
        vehicle.setVehicleRevocableLane(true);

        LaneTypeAttributes laneType = new LaneTypeAttributes(vehicle);

        laneAttributes = new LaneAttributes(directionalUse, sharedWith, laneType);
        String optionalBits = "0";
        encodedBits = optionalBits + directionalUse.encodeUPER() + sharedWith.encodeUPER() + laneType.encodeUPER();
    }

    @Test
    public void testConstructor() {

        LaneDirection directionalUse = new LaneDirection();
        directionalUse.setIngressPath(true);

        LaneSharing sharedWith = new LaneSharing();
        sharedWith.setPedestriansTraffic(true);
        sharedWith.setMultipleLanesTreatedAsOneLaneStatus(true);

        LaneAttributesBike bikeLane = new LaneAttributesBike();
        bikeLane.setUnsignalizedSegmentsPresent(true);

        LaneTypeAttributes laneType = new LaneTypeAttributes(bikeLane);

        LaneAttributes laneAttributes = new LaneAttributes(directionalUse, sharedWith, laneType);

        assertTrue(directionalUse.equals(laneAttributes.getDirectionalUse()));
        assertTrue(sharedWith.equals(laneAttributes.getSharedWith()));
        assertTrue(laneType.equals(laneAttributes.getLaneType()));
    }

    @Test
    public void testConstructorNullDirectionalUse() {

        thrown.expect(NullPointerException.class);
        new LaneAttributes(null, new LaneSharing(), new LaneTypeAttributes());
    }

    @Test
    public void testConstructorNullSharedWith() {

        thrown.expect(NullPointerException.class);
        new LaneAttributes(new LaneDirection(), null, new LaneTypeAttributes());
    }

    @Test
    public void testConstructorNullLaneType() {

        thrown.expect(NullPointerException.class);
        new LaneAttributes(new LaneDirection(), new LaneSharing(), null);
    }

    @Test
    public void testSetDirectionalUse() {

        LaneDirection directionalUse = new LaneDirection();

        LaneAttributes laneAttributes = new LaneAttributes();
        laneAttributes.setDirectionalUse(directionalUse);

        assertTrue(directionalUse.equals(laneAttributes.getDirectionalUse()));

        thrown.expect(NullPointerException.class);
        laneAttributes.setDirectionalUse(null);
    }

    @Test
    public void testSetSharedWith() {

        LaneSharing sharedWith = new LaneSharing();

        LaneAttributes laneAttributes = new LaneAttributes();
        laneAttributes.setSharedWith(sharedWith);

        assertTrue(sharedWith.equals(laneAttributes.getSharedWith()));

        thrown.expect(NullPointerException.class);
        laneAttributes.setSharedWith(null);
    }

    @Test
    public void testSetLaneType() {

        LaneTypeAttributes laneType = new LaneTypeAttributes();

        LaneAttributes laneAttributes = new LaneAttributes();
        laneAttributes.setLaneType(laneType);

        assertTrue(laneType.equals(laneAttributes.getLaneType()));

        thrown.expect(NullPointerException.class);
        laneAttributes.setLaneType(null);
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equalsIgnoreCase(laneAttributes.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneAttributes decodedLaneAttributes = new LaneAttributes();
        decodedLaneAttributes.decodeUPER(encodedBits);
        assertTrue(laneAttributes.equals(decodedLaneAttributes));
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String laneAttributesOptionals = "1";

        LaneAttributes laneAttributes = new LaneAttributes();
        thrown.expect(IllegalArgumentException.class);
        laneAttributes.decodeUPER(laneAttributesOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String laneAttributesOptionals = "";

        LaneAttributes laneAttributes = new LaneAttributes();
        thrown.expect(IllegalArgumentException.class);
        laneAttributes.decodeUPER(laneAttributesOptionals);
    }

    @Test
    public void testHashCode() {

        LaneDirection directionalUse = laneAttributes.getDirectionalUse();
        LaneSharing sharedWith = laneAttributes.getSharedWith();
        LaneTypeAttributes laneType = laneAttributes.getLaneType();

        LaneDirection diffDirectionalUse = new LaneDirection();
        diffDirectionalUse.setEgressPath(!directionalUse.isEgressPath());
        LaneSharing diffSharedWith = new LaneSharing();
        diffSharedWith.setBusVehicleTraffic(!sharedWith.isBusVehicleTraffic());
        LaneTypeAttributes diffLaneType = new LaneTypeAttributes(new LaneAttributesParking());

        LaneAttributes laneAttributes2 = new LaneAttributes(diffDirectionalUse, diffSharedWith, diffLaneType);

        assertFalse(laneAttributes.hashCode() == laneAttributes2.hashCode());
        assertTrue(laneAttributes.hashCode() == laneAttributes.hashCode());
        assertTrue(laneAttributes2.hashCode() == laneAttributes2.hashCode());

        LaneAttributes laneAttributes3 = new LaneAttributes(directionalUse, sharedWith, laneType);

        assertTrue(laneAttributes.hashCode() == laneAttributes3.hashCode());
        assertFalse(laneAttributes2.hashCode() == laneAttributes3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(laneAttributes.equals(laneAttributes));
        assertFalse(laneAttributes.equals(null));
        assertFalse(laneAttributes.equals(new String()));

        LaneDirection directionalUse = laneAttributes.getDirectionalUse();
        LaneSharing sharedWith = laneAttributes.getSharedWith();
        LaneTypeAttributes laneType = laneAttributes.getLaneType();

        LaneDirection diffDirectionalUse = new LaneDirection();
        diffDirectionalUse.setEgressPath(!directionalUse.isEgressPath());
        LaneSharing diffSharedWith = new LaneSharing();
        diffSharedWith.setBusVehicleTraffic(!sharedWith.isBusVehicleTraffic());
        LaneTypeAttributes diffLaneType = new LaneTypeAttributes(new LaneAttributesParking());

        // different
        LaneAttributes laneAttributes2 = new LaneAttributes(diffDirectionalUse, diffSharedWith, diffLaneType);

        assertFalse(laneAttributes.equals(laneAttributes2));

        // different directional use
        laneAttributes2 = new LaneAttributes(diffDirectionalUse, sharedWith, laneType);

        assertFalse(laneAttributes.equals(laneAttributes2));

        // different shared with
        laneAttributes2 = new LaneAttributes(directionalUse, diffSharedWith, laneType);

        assertFalse(laneAttributes.equals(laneAttributes2));

        // different lane type
        laneAttributes2 = new LaneAttributes(directionalUse, sharedWith, diffLaneType);

        assertFalse(laneAttributes.equals(laneAttributes2));

        // same
        laneAttributes2 = new LaneAttributes(directionalUse, sharedWith, laneType);

        assertTrue(laneAttributes.equals(laneAttributes2));
    }
}
