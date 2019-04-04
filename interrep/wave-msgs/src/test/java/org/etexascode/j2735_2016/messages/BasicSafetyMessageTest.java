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
package org.etexascode.j2735_2016.messages;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus.AntiLockBrake;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus.AuxiliaryBrake;
import org.etexascode.j2735_2016.elements.BrakeAppliedStatus;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied.BrakeBoost;
import org.etexascode.j2735_2016.elements.StabilityControlStatus.StabilityControl;
import org.etexascode.j2735_2016.elements.TractionControlStatus.TractionControl;
import org.etexascode.j2735_2016.elements.TransmissionState.Transmission;
import org.etexascode.j2735_2016.frames.AccelerationSet4Way;
import org.etexascode.j2735_2016.frames.BSMcoreData;
import org.etexascode.j2735_2016.frames.BrakeSystemStatus;
import org.etexascode.j2735_2016.frames.PositionalAccuracy;
import org.etexascode.j2735_2016.frames.VehicleSize;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the basic safety message.
 * 
 * @author ttevendale
 */
public class BasicSafetyMessageTest {

    BasicSafetyMessage bsm;

    String encodedBits;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void init() {

        BSMcoreData bsmCoreData = new BSMcoreData(12, "abcd1234", 50000, -246874, (long)9545151, -4000, new PositionalAccuracy(200, 100, 20), Transmission.NEUTRAL, 200, 28700, 54,
                new AccelerationSet4Way(-5, 38, 15, 100),
                new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.ON, AntiLockBrake.OFF, StabilityControl.ENGAGED, BrakeBoost.OFF, AuxiliaryBrake.RESERVED),
                new VehicleSize(20, 60));
        bsm = new BasicSafetyMessage(bsmCoreData);
        encodedBits = bsmCoreData.encodeUPER();
    }

    @Test
    public void testConstructor() {

        BSMcoreData bsmCoreData = new BSMcoreData(12, "ffff1234", 5000, -46874, (long)9645151, 4000, new PositionalAccuracy(220, 100, 20), Transmission.RESERVED1, 200, 28700, 54,
                new AccelerationSet4Way(-50, 80, 15, 100),
                new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.ON, AntiLockBrake.ON, StabilityControl.ENGAGED, BrakeBoost.UNAVAILABLE, AuxiliaryBrake.OFF),
                new VehicleSize(200, 300));

        BasicSafetyMessage bsm = new BasicSafetyMessage(bsmCoreData);

        assertTrue(bsmCoreData.equals(bsm.getCoreData()));

        thrown.expect(NullPointerException.class);
        new BasicSafetyMessage(null);
    }

    @Test
    public void testSetCoreData() {

        BSMcoreData bsmCoreData = new BSMcoreData(12, "ffff1234", 5000, -46874, (long)9645151, 4000, new PositionalAccuracy(220, 100, 20), Transmission.RESERVED1, 200, 28700, 54,
                new AccelerationSet4Way(-50, 80, 15, 100),
                new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.ON, AntiLockBrake.ON, StabilityControl.ENGAGED, BrakeBoost.UNAVAILABLE, AuxiliaryBrake.OFF),
                new VehicleSize(200, 300));

        BasicSafetyMessage bsm = new BasicSafetyMessage();
        bsm.setCoreData(bsmCoreData);

        assertTrue(bsmCoreData.equals(bsm.getCoreData()));

        thrown.expect(NullPointerException.class);
        bsm.setCoreData(null);
    }

    @Test
    public void testEncodeUPER() {

        String bsmOptionals = "000";
        assertTrue((bsmOptionals + encodedBits).equals(bsm.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        BasicSafetyMessage decodedBsm = new BasicSafetyMessage();
        String bsmOptionals = "000";
        decodedBsm.decodeUPER(bsmOptionals + encodedBits);
        assertTrue(bsm.equals(decodedBsm));
    }

    @Test
    public void testDecodeUPERExtension() {

        String bsmOptionals = "100";

        BasicSafetyMessage bsm = new BasicSafetyMessage();
        thrown.expect(IllegalArgumentException.class);
        bsm.decodeUPER(bsmOptionals);
    }

    @Test
    public void testDecodeUPERPartIIContent() {

        String bsmOptionals = "010";

        BasicSafetyMessage bsm = new BasicSafetyMessage();
        thrown.expect(IllegalArgumentException.class);
        bsm.decodeUPER(bsmOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String bsmOptionals = "001";

        BasicSafetyMessage bsm = new BasicSafetyMessage();
        thrown.expect(IllegalArgumentException.class);
        bsm.decodeUPER(bsmOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String bsmOptionals = "00";

        BasicSafetyMessage bsm = new BasicSafetyMessage();
        thrown.expect(IllegalArgumentException.class);
        bsm.decodeUPER(bsmOptionals);
    }

    @Test
    public void testHashCode() {

        int messageCount = bsm.getCoreData().getMessageCount().getValue();
        String id = bsm.getCoreData().getId().getValue();
        int secMark = bsm.getCoreData().getSecMark().getValue();
        int latitude = bsm.getCoreData().getLatitude().getValue();
        long longitude = bsm.getCoreData().getLongitude().getValue();
        int elevation = bsm.getCoreData().getElevation().getValue();
        PositionalAccuracy accuracy = bsm.getCoreData().getAccuracy();
        Transmission transmission = bsm.getCoreData().getTransmission().getEnumeration();
        int speed = bsm.getCoreData().getSpeed().getValue();
        int heading = bsm.getCoreData().getHeading().getValue();
        int angle = bsm.getCoreData().getAngle().getValue();
        AccelerationSet4Way accelerationSet = bsm.getCoreData().getAccelerationSet();
        BrakeSystemStatus brakes = bsm.getCoreData().getBrakes();
        VehicleSize size = bsm.getCoreData().getSize();

        BasicSafetyMessage bsm2 = new BasicSafetyMessage(
                new BSMcoreData(messageCount + 1, id, secMark, latitude + 1, longitude + 1, elevation + 1, accuracy, transmission, speed + 1, heading + 1, angle + 1, accelerationSet, brakes, size));

        assertFalse(bsm.hashCode() == bsm2.hashCode());
        assertTrue(bsm.hashCode() == bsm.hashCode());
        assertTrue(bsm2.hashCode() == bsm2.hashCode());

        BasicSafetyMessage bsm3 = new BasicSafetyMessage(
                new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size));

        assertTrue(bsm.hashCode() == bsm3.hashCode());
        assertFalse(bsm2.hashCode() == bsm3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(bsm.equals(bsm));
        assertFalse(bsm.equals(null));
        assertFalse(bsm.equals(new String()));

        int messageCount = bsm.getCoreData().getMessageCount().getValue();
        String id = bsm.getCoreData().getId().getValue();
        int secMark = bsm.getCoreData().getSecMark().getValue();
        int latitude = bsm.getCoreData().getLatitude().getValue();
        long longitude = bsm.getCoreData().getLongitude().getValue();
        int elevation = bsm.getCoreData().getElevation().getValue();
        PositionalAccuracy accuracy = bsm.getCoreData().getAccuracy();
        Transmission transmission = bsm.getCoreData().getTransmission().getEnumeration();
        int speed = bsm.getCoreData().getSpeed().getValue();
        int heading = bsm.getCoreData().getHeading().getValue();
        int angle = bsm.getCoreData().getAngle().getValue();
        AccelerationSet4Way accelerationSet = bsm.getCoreData().getAccelerationSet();
        BrakeSystemStatus brakes = bsm.getCoreData().getBrakes();
        VehicleSize size = bsm.getCoreData().getSize();

        // different
        BasicSafetyMessage bsm2 = new BasicSafetyMessage(
                new BSMcoreData(messageCount + 1, id, secMark, latitude + 1, longitude + 1, elevation + 1, accuracy, transmission, speed + 1, heading + 1, angle + 1, accelerationSet, brakes, size));

        assertFalse(bsm.equals(bsm2));

        // same
        bsm2 = new BasicSafetyMessage(new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size));

        assertTrue(bsm.equals(bsm2));
    }
}
