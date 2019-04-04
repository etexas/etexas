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

import org.etexascode.j2735_2016.elements.Acceleration;
import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus;
import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus.AntiLockBrake;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus.AuxiliaryBrake;
import org.etexascode.j2735_2016.elements.BrakeAppliedStatus;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied.BrakeBoost;
import org.etexascode.j2735_2016.elements.DSecond;
import org.etexascode.j2735_2016.elements.Elevation;
import org.etexascode.j2735_2016.elements.Heading;
import org.etexascode.j2735_2016.elements.Latitude;
import org.etexascode.j2735_2016.elements.Longitude;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.SemiMajorAxisAccuracy;
import org.etexascode.j2735_2016.elements.SemiMajorAxisOrientation;
import org.etexascode.j2735_2016.elements.SemiMinorAxisAccuracy;
import org.etexascode.j2735_2016.elements.Speed;
import org.etexascode.j2735_2016.elements.StabilityControlStatus;
import org.etexascode.j2735_2016.elements.StabilityControlStatus.StabilityControl;
import org.etexascode.j2735_2016.elements.SteeringWheelAngle;
import org.etexascode.j2735_2016.elements.TemporaryID;
import org.etexascode.j2735_2016.elements.TractionControlStatus;
import org.etexascode.j2735_2016.elements.TractionControlStatus.TractionControl;
import org.etexascode.j2735_2016.elements.TransmissionState;
import org.etexascode.j2735_2016.elements.TransmissionState.Transmission;
import org.etexascode.j2735_2016.elements.VehicleLength;
import org.etexascode.j2735_2016.elements.VehicleWidth;
import org.etexascode.j2735_2016.elements.VerticalAcceleration;
import org.etexascode.j2735_2016.elements.YawRate;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the BSM core data.
 * 
 * @author ttevendale
 */
public class BSMcoreDataTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    BSMcoreData bsmCoreData;

    String encodedBits;

    @Before
    public void init() {

        MsgCount messageCount = new MsgCount(12);

        TemporaryID id = new TemporaryID("00112233");

        DSecond secMark = new DSecond(60000);

        Latitude latitude = new Latitude(546874);

        Longitude longitude = new Longitude(8545181);

        Elevation elevation = new Elevation(-4000);

        SemiMajorAxisAccuracy semiMajor = new SemiMajorAxisAccuracy(200);
        SemiMinorAxisAccuracy semiMinor = new SemiMinorAxisAccuracy(100);
        SemiMajorAxisOrientation orientation = new SemiMajorAxisOrientation(5);
        PositionalAccuracy accuracy = new PositionalAccuracy(semiMajor, semiMinor, orientation);

        TransmissionState transmission = new TransmissionState(Transmission.FORWARD_GEARS);

        Speed speed = new Speed(20);

        Heading heading = new Heading(127);

        SteeringWheelAngle angle = new SteeringWheelAngle(120);

        Acceleration longitudeAccel = new Acceleration(-5);
        Acceleration latitudeAccel = new Acceleration(15);
        VerticalAcceleration vertical = new VerticalAcceleration(10);
        YawRate yawRate = new YawRate(100);
        AccelerationSet4Way accelerationSet = new AccelerationSet4Way(longitudeAccel, latitudeAccel, vertical, yawRate);

        BrakeAppliedStatus wheelBrakes = new BrakeAppliedStatus();
        wheelBrakes.setRightFront(true);
        TractionControlStatus traction = new TractionControlStatus(TractionControl.OFF);
        AntiLockBrakeStatus abs = new AntiLockBrakeStatus(AntiLockBrake.ENGAGED);
        StabilityControlStatus scs = new StabilityControlStatus(StabilityControl.ON);
        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.OFF);
        AuxiliaryBrakeStatus auxBrakes = new AuxiliaryBrakeStatus(AuxiliaryBrake.ON);
        BrakeSystemStatus brakes = new BrakeSystemStatus(wheelBrakes, traction, abs, scs, brakeBoost, auxBrakes);

        VehicleWidth width = new VehicleWidth(548);
        VehicleLength length = new VehicleLength(84);
        VehicleSize size = new VehicleSize(width, length);

        bsmCoreData = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);
        encodedBits = messageCount.encodeUPER() + id.encodeUPER() + secMark.encodeUPER() + latitude.encodeUPER() + longitude.encodeUPER() + elevation.encodeUPER() + accuracy.encodeUPER()
                + transmission.encodeUPER() + speed.encodeUPER() + heading.encodeUPER() + angle.encodeUPER() + accelerationSet.encodeUPER() + brakes.encodeUPER() + size.encodeUPER();
    }

    @Test
    public void testConstructor() {

        MsgCount messageCount = new MsgCount(12);

        TemporaryID id = new TemporaryID("00112233");

        DSecond secMark = new DSecond(60000);

        Latitude latitude = new Latitude(546874);

        Longitude longitude = new Longitude(8545181);

        Elevation elevation = new Elevation(-4000);

        SemiMajorAxisAccuracy semiMajor = new SemiMajorAxisAccuracy(200);
        SemiMinorAxisAccuracy semiMinor = new SemiMinorAxisAccuracy(100);
        SemiMajorAxisOrientation orientation = new SemiMajorAxisOrientation(5);
        PositionalAccuracy accuracy = new PositionalAccuracy(semiMajor, semiMinor, orientation);

        TransmissionState transmission = new TransmissionState(Transmission.FORWARD_GEARS);

        Speed speed = new Speed(20);

        Heading heading = new Heading(127);

        SteeringWheelAngle angle = new SteeringWheelAngle(120);

        Acceleration longitudeAccel = new Acceleration(-5);
        Acceleration latitudeAccel = new Acceleration(15);
        VerticalAcceleration vertical = new VerticalAcceleration(10);
        YawRate yawRate = new YawRate(100);
        AccelerationSet4Way accelerationSet = new AccelerationSet4Way(longitudeAccel, latitudeAccel, vertical, yawRate);

        BrakeAppliedStatus wheelBrakes = new BrakeAppliedStatus();
        wheelBrakes.setRightFront(true);
        TractionControlStatus traction = new TractionControlStatus(TractionControl.OFF);
        AntiLockBrakeStatus abs = new AntiLockBrakeStatus(AntiLockBrake.ENGAGED);
        StabilityControlStatus scs = new StabilityControlStatus(StabilityControl.ON);
        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.OFF);
        AuxiliaryBrakeStatus auxBrakes = new AuxiliaryBrakeStatus(AuxiliaryBrake.ON);
        BrakeSystemStatus brakes = new BrakeSystemStatus(wheelBrakes, traction, abs, scs, brakeBoost, auxBrakes);

        VehicleWidth width = new VehicleWidth(548);
        VehicleLength length = new VehicleLength(84);
        VehicleSize size = new VehicleSize(width, length);

        bsmCoreData = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertTrue(messageCount.equals(bsmCoreData.getMessageCount()));
        assertTrue(id.equals(bsmCoreData.getId()));
        assertTrue(secMark.equals(bsmCoreData.getSecMark()));
        assertTrue(latitude.equals(bsmCoreData.getLatitude()));
        assertTrue(longitude.equals(bsmCoreData.getLongitude()));
        assertTrue(elevation.equals(bsmCoreData.getElevation()));
        assertTrue(accuracy.equals(bsmCoreData.getAccuracy()));
        assertTrue(transmission.equals(bsmCoreData.getTransmission()));
        assertTrue(speed.equals(bsmCoreData.getSpeed()));
        assertTrue(heading.equals(bsmCoreData.getHeading()));
        assertTrue(angle.equals(bsmCoreData.getAngle()));
        assertTrue(accelerationSet.equals(bsmCoreData.getAccelerationSet()));
        assertTrue(brakes.equals(bsmCoreData.getBrakes()));
        assertTrue(size.equals(bsmCoreData.getSize()));
    }

    @Test
    public void testConstructorNullMessageCount() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(null, new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullId() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), null, new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullSecond() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), null, new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullLatitude() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), null, new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullLongitude() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), null, new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullElevation() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), null, new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullAccuracy() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), null, new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullTransmission() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), null, new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullSpeed() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), null,
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullHeading() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                null, new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullAngle() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), null, new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullAccelerationSet() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), null, new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorNullbrakes() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), null, new VehicleSize());
    }

    @Test
    public void testConstructorNullSize() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(new MsgCount(), new TemporaryID(), new DSecond(), new Latitude(), new Longitude(), new Elevation(), new PositionalAccuracy(), new TransmissionState(), new Speed(),
                new Heading(), new SteeringWheelAngle(), new AccelerationSet4Way(), new BrakeSystemStatus(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int messageCount = 120;
        String id = "abcd1234";
        int secMark = 60000;
        int latitude = 500;
        int longitude = -90000;
        int elevation = -400;
        PositionalAccuracy accuracy = new PositionalAccuracy(255, 23, 0);
        Transmission transmission = Transmission.PARK;
        int speed = 200;
        int heading = 800;
        int angle = 120;
        AccelerationSet4Way accelerationSet = new AccelerationSet4Way(500, -500, -50, 127);
        BrakeSystemStatus brakes = new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.ENGAGED, AntiLockBrake.OFF, StabilityControl.ON, BrakeBoost.UNAVAILABLE, AuxiliaryBrake.ON);
        VehicleSize size = new VehicleSize(400, 200);

        BSMcoreData bsmCoreData = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertTrue(messageCount == bsmCoreData.getMessageCount().getValue());
        assertTrue(id.equals(bsmCoreData.getId().getValue()));
        assertTrue(secMark == bsmCoreData.getSecMark().getValue());
        assertTrue(latitude == bsmCoreData.getLatitude().getValue());
        assertTrue(longitude == bsmCoreData.getLongitude().getValue());
        assertTrue(elevation == bsmCoreData.getElevation().getValue());
        assertTrue(accuracy.equals(bsmCoreData.getAccuracy()));
        assertTrue(transmission.equals(bsmCoreData.getTransmission().getEnumeration()));
        assertTrue(speed == bsmCoreData.getSpeed().getValue());
        assertTrue(heading == bsmCoreData.getHeading().getValue());
        assertTrue(angle == bsmCoreData.getAngle().getValue());
        assertTrue(accelerationSet.equals(bsmCoreData.getAccelerationSet()));
        assertTrue(brakes.equals(bsmCoreData.getBrakes()));
        assertTrue(size.equals(bsmCoreData.getSize()));
    }

    @Test
    public void testConstructorPrimitiveNullId() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(0, null, 0, 0, 0, 0, new PositionalAccuracy(), Transmission.UNAVAILABLE, 0, 0, 0, new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorPrimitiveNullAccuracy() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(0, "00000000", 0, 0, 0, 0, null, Transmission.UNAVAILABLE, 0, 0, 0, new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorPrimitiveNullTransmission() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(0, "00000000", 0, 0, 0, 0, new PositionalAccuracy(), null, 0, 0, 0, new AccelerationSet4Way(), new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorPrimitiveNullAccelerationSet() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(0, "00000000", 0, 0, 0, 0, new PositionalAccuracy(), Transmission.UNAVAILABLE, 0, 0, 0, null, new BrakeSystemStatus(), new VehicleSize());
    }

    @Test
    public void testConstructorPrimitiveNullBrakes() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(0, "00000000", 0, 0, 0, 0, new PositionalAccuracy(), Transmission.UNAVAILABLE, 0, 0, 0, new AccelerationSet4Way(), null, new VehicleSize());
    }

    @Test
    public void testConstructorPrimitiveNullSize() {

        thrown.expect(NullPointerException.class);
        new BSMcoreData(0, "00000000", 0, 0, 0, 0, new PositionalAccuracy(), Transmission.UNAVAILABLE, 0, 0, 0, new AccelerationSet4Way(), new BrakeSystemStatus(), null);
    }

    @Test
    public void testSetMessageCount() {

        MsgCount messageCount = new MsgCount(124);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setMessageCount(messageCount);

        assertTrue(messageCount.equals(bsmCoreData.getMessageCount()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setMessageCount(null);
    }

    @Test
    public void testSetMessageCountPrimitive() {

        int messageCount = 120;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setMessageCount(messageCount);

        assertTrue(messageCount == bsmCoreData.getMessageCount().getValue());
    }

    @Test
    public void testSetId() {

        TemporaryID id = new TemporaryID("12345678");

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setId(id);

        assertTrue(id.equals(bsmCoreData.getId()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setId((TemporaryID)null);
    }

    @Test
    public void testSetIdPrimitive() {

        String id = "1234abcd";

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setId(id);

        assertTrue(id.equals(bsmCoreData.getId().getValue()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setId((String)null);
    }

    @Test
    public void testSetSecMark() {

        DSecond secMark = new DSecond(879);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setSecMark(secMark);

        assertTrue(secMark.equals(bsmCoreData.getSecMark()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setSecMark(null);
    }

    @Test
    public void testSetSecMarkPrimitive() {

        int secMark = 60000;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setSecMark(secMark);

        assertTrue(secMark == bsmCoreData.getSecMark().getValue());
    }

    @Test
    public void testSetLatitude() {

        Latitude latitude = new Latitude(8975411);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setLatitude(latitude);

        assertTrue(latitude.equals(bsmCoreData.getLatitude()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setLatitude(null);
    }

    @Test
    public void testSetLatitudePrimitive() {

        int latitude = 500;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setLatitude(latitude);

        assertTrue(latitude == bsmCoreData.getLatitude().getValue());
    }

    @Test
    public void testSetLongitude() {

        Longitude longitude = new Longitude(-845148541);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setLongitude(longitude);

        assertTrue(longitude.equals(bsmCoreData.getLongitude()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setLongitude(null);
    }

    @Test
    public void testSetLongitudePrimitive() {

        int longitude = -90000;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setLongitude(longitude);

        assertTrue(longitude == bsmCoreData.getLongitude().getValue());
    }

    @Test
    public void testSetElevation() {

        Elevation elevation = new Elevation(500);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setElevation(elevation);

        assertTrue(elevation.equals(bsmCoreData.getElevation()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setElevation(null);
    }

    @Test
    public void testSetElevationPrimitive() {

        int elevation = -400;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setElevation(elevation);

        assertTrue(elevation == bsmCoreData.getElevation().getValue());
    }

    @Test
    public void testSetAccuracy() {

        PositionalAccuracy accuracy = new PositionalAccuracy(201, 200, 65000);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setAccuracy(accuracy);

        assertTrue(accuracy.equals(bsmCoreData.getAccuracy()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setAccuracy(null);
    }

    @Test
    public void testSetTransmission() {

        TransmissionState transmission = new TransmissionState(Transmission.PARK);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setTransmission(transmission);

        assertTrue(transmission.equals(bsmCoreData.getTransmission()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setTransmission((TransmissionState)null);
    }

    @Test
    public void testSetTransmissionPrimitive() {

        Transmission transmission = Transmission.REVERSE_GEARS;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setTransmission(transmission);

        assertTrue(transmission.equals(bsmCoreData.getTransmission().getEnumeration()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setTransmission((Transmission)null);
    }

    @Test
    public void testSetSpeed() {

        Speed speed = new Speed(20);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setSpeed(speed);

        assertTrue(speed.equals(bsmCoreData.getSpeed()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setSpeed(null);
    }

    @Test
    public void testSetSpeedPrimitive() {

        int speed = 200;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setSpeed(speed);

        assertTrue(speed == bsmCoreData.getSpeed().getValue());
    }

    @Test
    public void testSetHeading() {

        Heading heading = new Heading(127);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setHeading(heading);

        assertTrue(heading.equals(bsmCoreData.getHeading()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setHeading(null);
    }

    @Test
    public void testSetHeadingPrimitive() {

        int heading = 800;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setHeading(heading);

        assertTrue(heading == bsmCoreData.getHeading().getValue());
    }

    @Test
    public void testSetAngle() {

        SteeringWheelAngle angle = new SteeringWheelAngle(-5);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setAngle(angle);

        assertTrue(angle.equals(bsmCoreData.getAngle()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setAngle(null);
    }

    @Test
    public void testSetAnglePrimitive() {

        int angle = 120;

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setAngle(angle);

        assertTrue(angle == bsmCoreData.getAngle().getValue());
    }

    @Test
    public void testSetAccelerationSet() {

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way(-5, 32, -50, 838);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setAccelerationSet(accelerationSet);

        assertTrue(accelerationSet.equals(bsmCoreData.getAccelerationSet()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setAccelerationSet(null);
    }

    @Test
    public void testSetBrakes() {

        BrakeSystemStatus brakes = new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.OFF, AntiLockBrake.ENGAGED, StabilityControl.ON, BrakeBoost.UNAVAILABLE, AuxiliaryBrake.ON);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setBrakes(brakes);

        assertTrue(brakes.equals(bsmCoreData.getBrakes()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setBrakes(null);
    }

    @Test
    public void testSetSize() {

        VehicleSize size = new VehicleSize(100, 300);

        BSMcoreData bsmCoreData = new BSMcoreData();
        bsmCoreData.setSize(size);

        assertTrue(size.equals(bsmCoreData.getSize()));

        thrown.expect(NullPointerException.class);
        bsmCoreData.setSize(null);
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equalsIgnoreCase(bsmCoreData.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        BSMcoreData decodedBsmCoreData = new BSMcoreData();
        decodedBsmCoreData.decodeUPER(encodedBits);
        assertTrue(bsmCoreData.equals(decodedBsmCoreData));
    }

    @Test
    public void testHashCode() {

        int messageCount = bsmCoreData.getMessageCount().getValue();
        String id = bsmCoreData.getId().getValue();
        int secMark = bsmCoreData.getSecMark().getValue();
        int latitude = bsmCoreData.getLatitude().getValue();
        long longitude = bsmCoreData.getLongitude().getValue();
        int elevation = bsmCoreData.getElevation().getValue();
        PositionalAccuracy accuracy = bsmCoreData.getAccuracy();
        Transmission transmission = bsmCoreData.getTransmission().getEnumeration();
        int speed = bsmCoreData.getSpeed().getValue();
        int heading = bsmCoreData.getHeading().getValue();
        int angle = bsmCoreData.getAngle().getValue();
        AccelerationSet4Way accelerationSet = bsmCoreData.getAccelerationSet();
        BrakeSystemStatus brakes = bsmCoreData.getBrakes();
        VehicleSize size = bsmCoreData.getSize();

        BSMcoreData bsmCoreData2 = new BSMcoreData(messageCount + 1, id, secMark, latitude, longitude + 1, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.hashCode() == bsmCoreData2.hashCode());
        assertTrue(bsmCoreData.hashCode() == bsmCoreData.hashCode());
        assertTrue(bsmCoreData2.hashCode() == bsmCoreData2.hashCode());

        BSMcoreData bsmCoreData3 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertTrue(bsmCoreData.hashCode() == bsmCoreData3.hashCode());
        assertFalse(bsmCoreData2.hashCode() == bsmCoreData3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(bsmCoreData.equals(bsmCoreData));
        assertFalse(bsmCoreData.equals(null));
        assertFalse(bsmCoreData.equals(new String()));

        int messageCount = bsmCoreData.getMessageCount().getValue();
        String id = bsmCoreData.getId().getValue();
        int secMark = bsmCoreData.getSecMark().getValue();
        int latitude = bsmCoreData.getLatitude().getValue();
        long longitude = bsmCoreData.getLongitude().getValue();
        int elevation = bsmCoreData.getElevation().getValue();
        PositionalAccuracy accuracy = bsmCoreData.getAccuracy();
        Transmission transmission = bsmCoreData.getTransmission().getEnumeration();
        int speed = bsmCoreData.getSpeed().getValue();
        int heading = bsmCoreData.getHeading().getValue();
        int angle = bsmCoreData.getAngle().getValue();
        AccelerationSet4Way accelerationSet = bsmCoreData.getAccelerationSet();
        BrakeSystemStatus brakes = bsmCoreData.getBrakes();
        VehicleSize size = bsmCoreData.getSize();

        PositionalAccuracy accuracy2 = new PositionalAccuracy(0, 0, 0);
        AccelerationSet4Way accelerationSet2 = new AccelerationSet4Way(0, 0, 0, 0);
        BrakeSystemStatus brakes2 = new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.OFF, AntiLockBrake.OFF, StabilityControl.OFF, BrakeBoost.OFF, AuxiliaryBrake.OFF);
        VehicleSize size2 = new VehicleSize(0, 0);

        // different
        BSMcoreData bsmCoreData2 = new BSMcoreData(messageCount + 1, "abcdffff", secMark + 1, latitude + 1, longitude + 1, elevation + 1, accuracy2, Transmission.RESERVED3, speed + 1, heading + 1,
                angle + 1, accelerationSet2, brakes2, size2);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different message count
        bsmCoreData2 = new BSMcoreData(messageCount + 1, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different id
        bsmCoreData2 = new BSMcoreData(messageCount, "abcdffff", secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different sec mark
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark + 1, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different latitude
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude + 1, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different longitude
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude + 1, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different elevation
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation + 1, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different accuracy
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy2, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different transmission
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, Transmission.RESERVED3, speed, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different speed
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed + 1, heading, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different heading
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading + 1, angle, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different angle
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle + 1, accelerationSet, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different acceleration set
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet2, brakes, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different brakes
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes2, size);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // different size
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size2);

        assertFalse(bsmCoreData.equals(bsmCoreData2));

        // same
        bsmCoreData2 = new BSMcoreData(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);

        assertTrue(bsmCoreData.equals(bsmCoreData2));
    }
}
