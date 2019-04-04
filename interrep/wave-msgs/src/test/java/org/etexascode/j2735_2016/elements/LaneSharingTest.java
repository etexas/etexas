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
 * Unit tests for the lane sharing element.
 * 
 * @author ttevendale
 */
public class LaneSharingTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneSharing sharing = new LaneSharing();

        // min
        assertTrue("0000000000".equals(sharing.encodeUPER()));

        sharing.setMultipleLanesTreatedAsOneLaneStatus(true);
        sharing.setOtherNonMotorizedTrafficTypes(true);
        sharing.setIndividualMotorizedVehicleTraffic(true);
        sharing.setBusVehicleTraffic(true);
        sharing.setCyclistVehicleTraffic(true);

        // some turned on
        assertTrue("0111100100".equals(sharing.encodeUPER()));

        sharing.setOverlappingLaneDescriptionProvided(true);
        sharing.setTaxiVehicleTraffic(true);
        sharing.setPedestriansTraffic(true);
        sharing.setTrackedVehicleTraffic(true);
        sharing.setPedestrianTraffic(true);

        // max
        assertTrue("1111111111".equals(sharing.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneSharing sharing = new LaneSharing();

        // min
        String remainingBits = sharing.decodeUPER("0000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(sharing.isOverlappingLaneDescriptionProvided());
        assertFalse(sharing.isMultipleLanesTreatedAsOneLane());
        assertFalse(sharing.isOtherNonMotorizedTrafficTypes());
        assertFalse(sharing.isIndividualMotorizedVehicleTraffic());
        assertFalse(sharing.isBusVehicleTraffic());
        assertFalse(sharing.isTaxiVehicleTraffic());
        assertFalse(sharing.isPedestriansTraffic());
        assertFalse(sharing.isCyclistVehicleTraffic());
        assertFalse(sharing.isTrackedVehicleTraffic());
        assertFalse(sharing.isPedestrianTraffic());

        // some turned on
        remainingBits = sharing.decodeUPER("1000010010");
        assertTrue("".equals(remainingBits));
        assertTrue(sharing.isOverlappingLaneDescriptionProvided());
        assertFalse(sharing.isMultipleLanesTreatedAsOneLane());
        assertFalse(sharing.isOtherNonMotorizedTrafficTypes());
        assertFalse(sharing.isIndividualMotorizedVehicleTraffic());
        assertFalse(sharing.isBusVehicleTraffic());
        assertTrue(sharing.isTaxiVehicleTraffic());
        assertFalse(sharing.isPedestriansTraffic());
        assertFalse(sharing.isCyclistVehicleTraffic());
        assertTrue(sharing.isTrackedVehicleTraffic());
        assertFalse(sharing.isPedestrianTraffic());

        // max
        remainingBits = sharing.decodeUPER("1111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(sharing.isOverlappingLaneDescriptionProvided());
        assertTrue(sharing.isMultipleLanesTreatedAsOneLane());
        assertTrue(sharing.isOtherNonMotorizedTrafficTypes());
        assertTrue(sharing.isIndividualMotorizedVehicleTraffic());
        assertTrue(sharing.isBusVehicleTraffic());
        assertTrue(sharing.isTaxiVehicleTraffic());
        assertTrue(sharing.isPedestriansTraffic());
        assertTrue(sharing.isCyclistVehicleTraffic());
        assertTrue(sharing.isTrackedVehicleTraffic());
        assertTrue(sharing.isPedestrianTraffic());
    }

    @Test
    public void test() {

        LaneSharing sharing = new LaneSharing();
        sharing.setOverlappingLaneDescriptionProvided(true);
        sharing.setMultipleLanesTreatedAsOneLaneStatus(true);
        sharing.setOtherNonMotorizedTrafficTypes(true);
        sharing.setIndividualMotorizedVehicleTraffic(true);
        sharing.setBusVehicleTraffic(true);
        sharing.setTaxiVehicleTraffic(true);
        sharing.setPedestriansTraffic(true);
        sharing.setCyclistVehicleTraffic(true);
        sharing.setTrackedVehicleTraffic(true);
        sharing.setPedestrianTraffic(true);

        sharing.isOverlappingLaneDescriptionProvided();
        sharing.isMultipleLanesTreatedAsOneLane();
        sharing.isOtherNonMotorizedTrafficTypes();
        sharing.isIndividualMotorizedVehicleTraffic();
        sharing.isBusVehicleTraffic();
        sharing.isTaxiVehicleTraffic();
        sharing.isPedestriansTraffic();
        sharing.isCyclistVehicleTraffic();
        sharing.isTrackedVehicleTraffic();
        sharing.isPedestrianTraffic();
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneSharing sharing = new LaneSharing();
        thrown.expect(IllegalArgumentException.class);
        sharing.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneSharing sharing = new LaneSharing();
        String remainingBits = sharing.decodeUPER("010111010100100");
        assertTrue("00100".equals(remainingBits));
        assertFalse(sharing.isOverlappingLaneDescriptionProvided());
        assertTrue(sharing.isMultipleLanesTreatedAsOneLane());
        assertFalse(sharing.isOtherNonMotorizedTrafficTypes());
        assertTrue(sharing.isIndividualMotorizedVehicleTraffic());
        assertTrue(sharing.isBusVehicleTraffic());
        assertTrue(sharing.isTaxiVehicleTraffic());
        assertFalse(sharing.isPedestriansTraffic());
        assertTrue(sharing.isCyclistVehicleTraffic());
        assertFalse(sharing.isTrackedVehicleTraffic());
        assertTrue(sharing.isPedestrianTraffic());
    }

    @Test
    public void testHashCode() {

        LaneSharing sharing = new LaneSharing();
        sharing.setOverlappingLaneDescriptionProvided(true);
        sharing.setMultipleLanesTreatedAsOneLaneStatus(true);
        sharing.setOtherNonMotorizedTrafficTypes(true);
        sharing.setIndividualMotorizedVehicleTraffic(true);
        sharing.setBusVehicleTraffic(true);
        sharing.setTaxiVehicleTraffic(true);
        sharing.setPedestriansTraffic(true);
        sharing.setCyclistVehicleTraffic(true);
        sharing.setTrackedVehicleTraffic(true);
        sharing.setPedestrianTraffic(true);

        LaneSharing sharing2 = new LaneSharing();
        sharing2.setOverlappingLaneDescriptionProvided(false);
        sharing2.setMultipleLanesTreatedAsOneLaneStatus(false);
        sharing2.setOtherNonMotorizedTrafficTypes(false);
        sharing2.setIndividualMotorizedVehicleTraffic(false);
        sharing2.setBusVehicleTraffic(false);
        sharing2.setTaxiVehicleTraffic(false);
        sharing2.setPedestriansTraffic(false);
        sharing2.setCyclistVehicleTraffic(false);
        sharing2.setTrackedVehicleTraffic(false);
        sharing2.setPedestrianTraffic(false);

        assertFalse(sharing.hashCode() == sharing2.hashCode());
        assertTrue(sharing.hashCode() == sharing.hashCode());
        assertTrue(sharing2.hashCode() == sharing2.hashCode());

        LaneSharing sharing3 = new LaneSharing();
        sharing3.setOverlappingLaneDescriptionProvided(sharing.isOverlappingLaneDescriptionProvided());
        sharing3.setMultipleLanesTreatedAsOneLaneStatus(sharing.isMultipleLanesTreatedAsOneLane());
        sharing3.setOtherNonMotorizedTrafficTypes(sharing.isOtherNonMotorizedTrafficTypes());
        sharing3.setIndividualMotorizedVehicleTraffic(sharing.isIndividualMotorizedVehicleTraffic());
        sharing3.setBusVehicleTraffic(sharing.isBusVehicleTraffic());
        sharing3.setTaxiVehicleTraffic(sharing.isTaxiVehicleTraffic());
        sharing3.setPedestriansTraffic(sharing.isPedestriansTraffic());
        sharing3.setCyclistVehicleTraffic(sharing.isCyclistVehicleTraffic());
        sharing3.setTrackedVehicleTraffic(sharing.isTrackedVehicleTraffic());
        sharing3.setPedestrianTraffic(sharing.isPedestrianTraffic());

        assertTrue(sharing.hashCode() == sharing3.hashCode());
        assertFalse(sharing2.hashCode() == sharing3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneSharing sharing = new LaneSharing();
        sharing.setOverlappingLaneDescriptionProvided(true);
        sharing.setMultipleLanesTreatedAsOneLaneStatus(true);
        sharing.setOtherNonMotorizedTrafficTypes(true);
        sharing.setIndividualMotorizedVehicleTraffic(true);
        sharing.setBusVehicleTraffic(true);
        sharing.setTaxiVehicleTraffic(true);
        sharing.setPedestriansTraffic(true);
        sharing.setCyclistVehicleTraffic(true);
        sharing.setTrackedVehicleTraffic(true);
        sharing.setPedestrianTraffic(true);

        assertFalse(sharing.equals(null));

        assertTrue(sharing.equals(sharing));

        LaneSharing sharing2 = new LaneSharing();
        sharing2.setOverlappingLaneDescriptionProvided(false);
        sharing2.setMultipleLanesTreatedAsOneLaneStatus(false);
        sharing2.setOtherNonMotorizedTrafficTypes(false);
        sharing2.setIndividualMotorizedVehicleTraffic(false);
        sharing2.setBusVehicleTraffic(false);
        sharing2.setTaxiVehicleTraffic(false);
        sharing2.setPedestriansTraffic(false);
        sharing2.setCyclistVehicleTraffic(false);
        sharing2.setTrackedVehicleTraffic(false);
        sharing2.setPedestrianTraffic(false);

        assertFalse(sharing.equals(new String()));
        assertFalse(sharing.equals(sharing2));

        sharing2.setOverlappingLaneDescriptionProvided(sharing.isOverlappingLaneDescriptionProvided());
        assertFalse(sharing.equals(sharing2));

        sharing2.setMultipleLanesTreatedAsOneLaneStatus(sharing.isMultipleLanesTreatedAsOneLane());
        assertFalse(sharing.equals(sharing2));

        sharing2.setOtherNonMotorizedTrafficTypes(sharing.isOtherNonMotorizedTrafficTypes());
        assertFalse(sharing.equals(sharing2));

        sharing2.setIndividualMotorizedVehicleTraffic(sharing.isIndividualMotorizedVehicleTraffic());
        assertFalse(sharing.equals(sharing2));

        sharing2.setBusVehicleTraffic(sharing.isBusVehicleTraffic());
        assertFalse(sharing.equals(sharing2));

        sharing2.setTaxiVehicleTraffic(sharing.isTaxiVehicleTraffic());
        assertFalse(sharing.equals(sharing2));

        sharing2.setPedestriansTraffic(sharing.isPedestriansTraffic());
        assertFalse(sharing.equals(sharing2));

        sharing2.setCyclistVehicleTraffic(sharing.isCyclistVehicleTraffic());
        assertFalse(sharing.equals(sharing2));

        sharing2.setTrackedVehicleTraffic(sharing.isTrackedVehicleTraffic());
        assertFalse(sharing.equals(sharing2));

        sharing2.setPedestrianTraffic(sharing.isPedestrianTraffic());
        assertTrue(sharing.equals(sharing2));
    }
}
