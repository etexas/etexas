/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.LaneAttributesBarrier;
import org.etexascode.j2735_2016.elements.LaneAttributesBike;
import org.etexascode.j2735_2016.elements.LaneAttributesCrosswalk;
import org.etexascode.j2735_2016.elements.LaneAttributesParking;
import org.etexascode.j2735_2016.elements.LaneAttributesSidewalk;
import org.etexascode.j2735_2016.elements.LaneAttributesStriping;
import org.etexascode.j2735_2016.elements.LaneAttributesTrackedVehicle;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the lane type attributes frame (Choice).
 * 
 * @author ttevendale
 */
public class LaneTypeAttributesTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorLaneAttributesVehicle() {

        LaneAttributesVehicle vehicle = new LaneAttributesVehicle();
        vehicle.setHovLaneUseOnly(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(vehicle);

        assertTrue(vehicle.equals(attributes.getVehicle()));
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesVehicle)null);
    }

    @Test
    public void testConstructorLaneAttributesCrosswalk() {

        LaneAttributesCrosswalk crosswalk = new LaneAttributesCrosswalk();
        crosswalk.setAudioSupport(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(crosswalk);

        assertTrue(crosswalk.equals(attributes.getCrosswalk()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesCrosswalk)null);
    }

    @Test
    public void testConstructorLaneAttributesBike() {

        LaneAttributesBike bikeLane = new LaneAttributesBike();
        bikeLane.setBiDirectionalCycleTimes(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(bikeLane);

        assertTrue(bikeLane.equals(attributes.getBikeLane()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesBike)null);
    }

    @Test
    public void testConstructorLaneAttributesSidewalk() {

        LaneAttributesSidewalk sidewalk = new LaneAttributesSidewalk();
        sidewalk.setBicycleUseAllowed(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(sidewalk);

        assertTrue(sidewalk.equals(attributes.getSidewalk()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesSidewalk)null);
    }

    @Test
    public void testConstructorLaneAttributesBarrier() {

        LaneAttributesBarrier median = new LaneAttributesBarrier();
        median.setConstructionBarrier(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(median);

        assertTrue(median.equals(attributes.getMedian()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesBarrier)null);
    }

    @Test
    public void testConstructorLaneAttributesStriping() {

        LaneAttributesStriping striping = new LaneAttributesStriping();
        striping.setStripeDrawOnLeft(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(striping);

        assertTrue(striping.equals(attributes.getStriping()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesStriping)null);
    }

    @Test
    public void testConstructorLaneAttributesTrackedVehicle() {

        LaneAttributesTrackedVehicle trackedVehicle = new LaneAttributesTrackedVehicle();
        trackedVehicle.setSpecCommuterRailRoadTrack(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(trackedVehicle);

        assertTrue(trackedVehicle.equals(attributes.getTrackedVehicle()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getParking());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesTrackedVehicle)null);
    }

    @Test
    public void testConstructorLaneAttributesParking() {

        LaneAttributesParking parking = new LaneAttributesParking();
        parking.setDoNotParkZone(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(parking);

        assertTrue(parking.equals(attributes.getParking()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());

        thrown.expect(NullPointerException.class);
        new LaneTypeAttributes((LaneAttributesParking)null);
    }

    @Test
    public void testEncodeUPERLaneAttributesVehicle() {

        LaneAttributesVehicle vehicle = new LaneAttributesVehicle();
        vehicle.setIRbeaconCoverage(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(vehicle);

        String attributeChoice = "0000";
        String remainingBits = vehicle.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERLaneAttributesCrosswalk() {

        LaneAttributesCrosswalk crosswalk = new LaneAttributesCrosswalk();
        crosswalk.setBicycleUseAllowed(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(crosswalk);

        String attributeChoice = "0001";
        String remainingBits = crosswalk.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERLaneAttributesBike() {

        LaneAttributesBike bikeLane = new LaneAttributesBike();
        bikeLane.setBikeFlyOverLane(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(bikeLane);

        String attributeChoice = "0010";
        String remainingBits = bikeLane.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERLaneAttributesSidewalk() {

        LaneAttributesSidewalk sidewalk = new LaneAttributesSidewalk();
        sidewalk.setSidewalkFlyOverLane(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(sidewalk);

        String attributeChoice = "0011";
        String remainingBits = sidewalk.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERLaneAttributesBarrier() {

        LaneAttributesBarrier median = new LaneAttributesBarrier();
        median.setDoubleStripedLines(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(median);

        String attributeChoice = "0100";
        String remainingBits = median.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERLaneAttributesStriping() {

        LaneAttributesStriping striping = new LaneAttributesStriping();
        striping.setStripeDrawOnRight(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(striping);

        String attributeChoice = "0101";
        String remainingBits = striping.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERLaneAttributesTrackedVehicle() {

        LaneAttributesTrackedVehicle trackedVehicle = new LaneAttributesTrackedVehicle();
        trackedVehicle.setSpecHeavyRailRoadTrack(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(trackedVehicle);

        String attributeChoice = "0110";
        String remainingBits = trackedVehicle.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERLaneAttributesParking() {

        LaneAttributesParking parking = new LaneAttributesParking();
        parking.setHeadInParkingInUse(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(parking);

        String attributeChoice = "0111";
        String remainingBits = parking.encodeUPER();

        assertTrue((attributeChoice + remainingBits).equals(attributes.encodeUPER()));
    }

    @Test
    public void testEncodeUPERAllNull() {

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        thrown.expect(IllegalStateException.class);
        attributes.encodeUPER();
    }

    @Test
    public void testDecodeUPERLaneAttributesVehicle() {

        LaneAttributesVehicle vehicle = new LaneAttributesVehicle();
        vehicle.setPermissionOnRequest(true);

        String attributeChoice = "0000";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + vehicle.encodeUPER());

        assertTrue(vehicle.equals(attributes.getVehicle()));
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());
    }

    @Test
    public void testDecodeUPERLaneAttributesCrosswalk() {

        LaneAttributesCrosswalk crosswalk = new LaneAttributesCrosswalk();
        crosswalk.setBiDirectionalCycleTimes(true);

        String attributeChoice = "0001";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + crosswalk.encodeUPER());

        assertTrue(crosswalk.equals(attributes.getCrosswalk()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());
    }

    @Test
    public void testDecodeUPERLaneAttributesBike() {

        LaneAttributesBike bikeLane = new LaneAttributesBike();
        bikeLane.setBikeRevocableLane(true);

        String attributeChoice = "0010";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + bikeLane.encodeUPER());

        assertTrue(bikeLane.equals(attributes.getBikeLane()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());
    }

    @Test
    public void testDecodeUPERLaneAttributesSidewalk() {

        LaneAttributesSidewalk sidewalk = new LaneAttributesSidewalk();
        sidewalk.setSidewalkRevocableLane(true);

        String attributeChoice = "0011";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + sidewalk.encodeUPER());

        assertTrue(sidewalk.equals(attributes.getSidewalk()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());
    }

    @Test
    public void testDecodeUPERLaneAttributesBarrier() {

        LaneAttributesBarrier median = new LaneAttributesBarrier();
        median.setHighCurbs(true);

        String attributeChoice = "0100";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + median.encodeUPER());

        assertTrue(median.equals(attributes.getMedian()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());
    }

    @Test
    public void testDecodeUPERLaneAttributesStriping() {

        LaneAttributesStriping striping = new LaneAttributesStriping();
        striping.setStripeToConnectingLanesAhead(true);

        String attributeChoice = "0101";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + striping.encodeUPER());

        assertTrue(striping.equals(attributes.getStriping()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getTrackedVehicle());
        assertNull(attributes.getParking());
    }

    @Test
    public void testDecodeUPERLaneAttributesTrackedVehicle() {

        LaneAttributesTrackedVehicle trackedVehicle = new LaneAttributesTrackedVehicle();
        trackedVehicle.setSpecLightRailRoadTrack(true);

        String attributeChoice = "0110";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + trackedVehicle.encodeUPER());

        assertTrue(trackedVehicle.equals(attributes.getTrackedVehicle()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getParking());
    }

    @Test
    public void testDecodeUPERLaneAttributesParking() {

        LaneAttributesParking parking = new LaneAttributesParking();
        parking.setNoPublicParkingUse(true);

        String attributeChoice = "0111";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        attributes.decodeUPER(attributeChoice + parking.encodeUPER());

        assertTrue(parking.equals(attributes.getParking()));
        assertNull(attributes.getVehicle());
        assertNull(attributes.getCrosswalk());
        assertNull(attributes.getBikeLane());
        assertNull(attributes.getSidewalk());
        assertNull(attributes.getMedian());
        assertNull(attributes.getStriping());
        assertNull(attributes.getTrackedVehicle());
    }

    @Test
    public void testDecodeUPERExtension() {

        String attributeChoice = "1000";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER(attributeChoice);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String attributeChoice = "000";

        LaneTypeAttributes attributes = new LaneTypeAttributes();
        thrown.expect(IllegalArgumentException.class);
        attributes.decodeUPER(attributeChoice);
    }

    @Test
    public void testHashCode() {

        LaneAttributesVehicle vehicle = new LaneAttributesVehicle();
        vehicle.setRestrictedFromPublicUse(true);
        LaneAttributesVehicle sameVehicle = new LaneAttributesVehicle();
        sameVehicle.setRestrictedFromPublicUse(true);
        LaneAttributesVehicle diffVehicle = new LaneAttributesVehicle();
        diffVehicle.setRestrictedToBusUse(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(vehicle);

        LaneTypeAttributes attributes2 = new LaneTypeAttributes(diffVehicle);

        assertFalse(attributes.hashCode() == attributes2.hashCode());
        assertTrue(attributes.hashCode() == attributes.hashCode());
        assertTrue(attributes2.hashCode() == attributes2.hashCode());

        LaneTypeAttributes attributes3 = new LaneTypeAttributes(sameVehicle);

        assertTrue(attributes.hashCode() == attributes3.hashCode());
        assertFalse(attributes2.hashCode() == attributes3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneAttributesVehicle vehicle = new LaneAttributesVehicle();
        vehicle.setRestrictedFromPublicUse(true);
        LaneAttributesVehicle sameVehicle = new LaneAttributesVehicle();
        sameVehicle.setRestrictedFromPublicUse(true);
        LaneAttributesVehicle diffVehicle = new LaneAttributesVehicle();
        diffVehicle.setRestrictedToBusUse(true);

        LaneAttributesCrosswalk crosswalk = new LaneAttributesCrosswalk();
        crosswalk.setCrosswalkRevocableLane(true);
        LaneAttributesCrosswalk sameCrosswalk = new LaneAttributesCrosswalk();
        sameCrosswalk.setCrosswalkRevocableLane(true);
        LaneAttributesCrosswalk diffCrosswalk = new LaneAttributesCrosswalk();
        diffCrosswalk.setFixedCycleTime(true);

        LaneAttributesBike bikeLane = new LaneAttributesBike();
        bikeLane.setFixedCycleTime(true);
        LaneAttributesBike sameBikeLane = new LaneAttributesBike();
        sameBikeLane.setFixedCycleTime(true);
        LaneAttributesBike diffBikeLane = new LaneAttributesBike();
        diffBikeLane.setIsolatedByBarrier(true);

        LaneAttributesSidewalk sidewalk = new LaneAttributesSidewalk();
        sidewalk.setWalkBikes(true);
        LaneAttributesSidewalk sameSidewalk = new LaneAttributesSidewalk();
        sameSidewalk.setWalkBikes(true);
        LaneAttributesSidewalk diffSidewalk = new LaneAttributesSidewalk();
        diffSidewalk.setBicycleUseAllowed(true);

        LaneAttributesBarrier median = new LaneAttributesBarrier();
        median.setLowCurbs(true);
        LaneAttributesBarrier sameMedian = new LaneAttributesBarrier();
        sameMedian.setLowCurbs(true);
        LaneAttributesBarrier diffMedian = new LaneAttributesBarrier();
        diffMedian.setMedian(true);

        LaneAttributesStriping striping = new LaneAttributesStriping();
        striping.setStripeToConnectingLanesLeft(true);
        LaneAttributesStriping sameStriping = new LaneAttributesStriping();
        sameStriping.setStripeToConnectingLanesLeft(true);
        LaneAttributesStriping diffStriping = new LaneAttributesStriping();
        diffStriping.setStripeToConnectingLanesRevocableLane(true);

        LaneAttributesTrackedVehicle trackedVehicle = new LaneAttributesTrackedVehicle();
        trackedVehicle.setSpecOtherRailType(true);
        LaneAttributesTrackedVehicle sameTrackedVehicle = new LaneAttributesTrackedVehicle();
        sameTrackedVehicle.setSpecOtherRailType(true);
        LaneAttributesTrackedVehicle diffTrackedVehicle = new LaneAttributesTrackedVehicle();
        diffTrackedVehicle.setSpecRevocableLane(true);

        LaneAttributesParking parking = new LaneAttributesParking();
        parking.setParallelParkingInUse(true);
        LaneAttributesParking sameParking = new LaneAttributesParking();
        sameParking.setParallelParkingInUse(true);
        LaneAttributesParking diffParking = new LaneAttributesParking();
        diffParking.setParkingForBusUse(true);

        LaneTypeAttributes attributes = new LaneTypeAttributes(vehicle);

        assertTrue(attributes.equals(attributes));
        assertFalse(attributes.equals(null));
        assertFalse(attributes.equals(new String()));

        // different lane attribute vehicle
        LaneTypeAttributes attributes2 = new LaneTypeAttributes(diffVehicle);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute vehicle
        attributes2 = new LaneTypeAttributes(sameVehicle);
        assertTrue(attributes.equals(attributes2));

        // different lane attribute crosswalk
        attributes = new LaneTypeAttributes(crosswalk);
        attributes2 = new LaneTypeAttributes(diffCrosswalk);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute crosswalk
        attributes2 = new LaneTypeAttributes(sameCrosswalk);
        assertTrue(attributes.equals(attributes2));

        // different lane attribute bike
        attributes = new LaneTypeAttributes(bikeLane);
        attributes2 = new LaneTypeAttributes(diffBikeLane);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute bike
        attributes2 = new LaneTypeAttributes(sameBikeLane);
        assertTrue(attributes.equals(attributes2));

        // different lane attribute sidewalk
        attributes = new LaneTypeAttributes(sidewalk);
        attributes2 = new LaneTypeAttributes(diffSidewalk);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute
        attributes2 = new LaneTypeAttributes(sameSidewalk);
        assertTrue(attributes.equals(attributes2));

        // different lane attribute barrier
        attributes = new LaneTypeAttributes(median);
        attributes2 = new LaneTypeAttributes(diffMedian);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute barrier
        attributes2 = new LaneTypeAttributes(sameMedian);
        assertTrue(attributes.equals(attributes2));

        // different lane attribute striping
        attributes = new LaneTypeAttributes(striping);
        attributes2 = new LaneTypeAttributes(diffStriping);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute striping
        attributes2 = new LaneTypeAttributes(sameStriping);
        assertTrue(attributes.equals(attributes2));

        // different lane attribute tracked vehicle
        attributes = new LaneTypeAttributes(trackedVehicle);
        attributes2 = new LaneTypeAttributes(diffTrackedVehicle);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute tracked vehicle
        attributes2 = new LaneTypeAttributes(sameTrackedVehicle);
        assertTrue(attributes.equals(attributes2));

        // different lane attribute parking
        attributes = new LaneTypeAttributes(parking);
        attributes2 = new LaneTypeAttributes(diffParking);
        assertFalse(attributes.equals(attributes2));

        // same lane attribute parking
        attributes2 = new LaneTypeAttributes(sameParking);
        assertTrue(attributes.equals(attributes2));
    }
}
