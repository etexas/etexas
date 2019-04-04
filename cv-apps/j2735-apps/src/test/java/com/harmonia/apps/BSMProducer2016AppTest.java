/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package com.harmonia.apps;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.etexascode.apps.OBUDevice;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735_2016.elements.Acceleration;
import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus.AntiLockBrake;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus.AuxiliaryBrake;
import org.etexascode.j2735_2016.elements.BrakeAppliedStatus;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied.BrakeBoost;
import org.etexascode.j2735_2016.elements.Elevation;
import org.etexascode.j2735_2016.elements.SemiMajorAxisAccuracy;
import org.etexascode.j2735_2016.elements.SemiMajorAxisOrientation;
import org.etexascode.j2735_2016.elements.SemiMinorAxisAccuracy;
import org.etexascode.j2735_2016.elements.Speed;
import org.etexascode.j2735_2016.elements.StabilityControlStatus.StabilityControl;
import org.etexascode.j2735_2016.elements.SteeringWheelAngle;
import org.etexascode.j2735_2016.elements.TractionControlStatus.TractionControl;
import org.etexascode.j2735_2016.elements.TransmissionState.Transmission;
import org.etexascode.j2735_2016.elements.VerticalAcceleration;
import org.etexascode.j2735_2016.frames.AccelerationSet4Way;
import org.etexascode.j2735_2016.frames.BSMcoreData;
import org.etexascode.j2735_2016.frames.BrakeSystemStatus;
import org.etexascode.j2735_2016.frames.PositionalAccuracy;
import org.etexascode.j2735_2016.frames.VehicleSize;
import org.etexascode.j2735_2016.messages.BasicSafetyMessage;
import org.etexascode.j2735_2016.messages.MessageFrame;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the BSMProducer2016App.
 * 
 * @author ttevendale
 */
public class BSMProducer2016AppTest {

    BSMProducer2016App bsmProducer;

    Vehicle vehicle;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void init() {

        bsmProducer = new BSMProducer2016App();
        bsmProducer.init(new String[] { "0.1" });

        vehicle = new Vehicle(1, 100, 100, 0, 0, 0);
        vehicle.setSpeed(15);
        vehicle.setAcceleration(15);
    }

    @Test
    public void testGetAppName() {

        assertTrue(BSMProducer2016App.APP_NAME_BSM_PRODUCER_2016_APP.equals(new BSMProducer2016App().getAppName()));
    }

    @Test
    public void testInitFrequency() {

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        BSMProducer2016App bsmProducer = new BSMProducer2016App();
        bsmProducer.init(new String[] { "1.0" });

        // The device will send a message.
        bsmProducer.performUpdate(device, null, null, 0.0, null);
        assertFalse(device.getAppMessages().isEmpty());

        device.getAppMessages().clear();

        // The device will not send a message since 1 second has not elapsed since the last time.
        bsmProducer.performUpdate(device, null, null, 0.5, null);
        assertTrue(device.getAppMessages().isEmpty());

        device.getAppMessages().clear();

        // The device will send a message since its been a second.
        bsmProducer.performUpdate(device, null, null, 1.0, null);
        assertFalse(device.getAppMessages().isEmpty());
    }

    @Test
    public void testPerformUpdate() {

        int vehicleId = 1;
        int latitude = 50;
        int longitude = -84;
        int speed = 15;
        int heading = 100;
        int steeringWheelangle = 100;
        double acceleration = 12.1;
        int width = 250;
        int length = 400;
        Vehicle vehicle = new Vehicle(vehicleId, length, width, 0, 0, 0);
        vehicle.setGlobalId(vehicleId);
        vehicle.setLatitude(latitude);
        vehicle.setLongitude(longitude);
        vehicle.setSpeed(speed);
        vehicle.setHeading(heading);
        vehicle.setSteeringAngle(steeringWheelangle);
        vehicle.setAcceleration(acceleration);
        vehicle.setBrakePressed(false);

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        assertTrue(device.getAppMessages().size() == 1);

        BasicSafetyMessage bsm = getBSM(device);
        BSMcoreData coreData = bsm.getCoreData();
        assertTrue(coreData.getMessageCount().getValue() == 0);
        assertTrue(coreData.getId().getValue().equalsIgnoreCase("80000001"));
        assertTrue(coreData.getSecMark().getValue() == 0);
        assertTrue(coreData.getLatitude().getValue() == (int)UtilsUnitConversion.convertToOneTenthMicrodegree(latitude));
        assertTrue(coreData.getLongitude().getValue() == (int)UtilsUnitConversion.convertToOneTenthMicrodegree(longitude));
        assertTrue(coreData.getElevation().getValue() == Elevation.UNAVAILABLE);

        PositionalAccuracy accuracy = coreData.getAccuracy();
        assertTrue(accuracy.getSemiMajor().getValue() == SemiMajorAxisAccuracy.UNAVAILABLE);
        assertTrue(accuracy.getSemiMinor().getValue() == SemiMinorAxisAccuracy.UNAVAILABLE);
        assertTrue(accuracy.getOrientation().getValue() == SemiMajorAxisOrientation.UNAVAILABLE);

        assertTrue(coreData.getTransmission().getEnumeration().equals(Transmission.FORWARD_GEARS));
        assertTrue(coreData.getSpeed().getValue() == (int)UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(speed));
        assertTrue(coreData.getHeading().getValue() == (int)UtilsUnitConversion.convertDegreesToBSMHeading(heading));
        assertTrue(coreData.getAngle().getValue() == (int)UtilsUnitConversion.convertDegreesToOneAndAHalfDegrees(steeringWheelangle));

        AccelerationSet4Way accelerationSet = coreData.getAccelerationSet();

        assertTrue(accelerationSet.getLongitude().getValue() == UtilsUnitConversion.convertMetersPerSecondToOneHundredthMetersPerSecond(acceleration));
        assertTrue(accelerationSet.getLatitude().getValue() == Acceleration.UNAVAILABLE);
        assertTrue(accelerationSet.getVertical().getValue() == VerticalAcceleration.UNAVAILABLE);
        assertTrue(accelerationSet.getYawRate().getValue() == 0);

        BrakeSystemStatus brakes = coreData.getBrakes();
        BrakeAppliedStatus wheelBrakes = brakes.getWheelBrakes();
        assertFalse(wheelBrakes.isLeftFront());
        assertFalse(wheelBrakes.isLeftRear());
        assertFalse(wheelBrakes.isRightFront());
        assertFalse(wheelBrakes.isRightRear());

        assertTrue(brakes.getTraction().getEnumeration().equals(TractionControl.UNAVAILABLE));
        assertTrue(brakes.getAbs().getEnumeration().equals(AntiLockBrake.UNAVAILABLE));
        assertTrue(brakes.getScs().getEnumeration().equals(StabilityControl.UNAVAILABLE));
        assertTrue(brakes.getBrakeBoost().getEnumeration().equals(BrakeBoost.UNAVAILABLE));
        assertTrue(brakes.getAuxBrakes().getEnumeration().equals(AuxiliaryBrake.UNAVAILABLE));

        VehicleSize size = coreData.getSize();
        assertTrue(size.getWidth().getValue() == width);
        assertTrue(size.getLength().getValue() == length);
    }

    @Test
    public void testPerformUpdateTwice() {

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        assertTrue(device.getAppMessages().size() == 1);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(bsm.getCoreData().getMessageCount().getValue() == 0);

        device.getAppMessages().clear();
        bsmProducer.performUpdate(device, null, null, 0.5, null);

        assertTrue(device.getAppMessages().size() == 1);

        bsm = MessageFrame.decodeBSM(new String((byte[])device.getAppMessages().get(0).getData()));

        // message count increments
        assertTrue(bsm.getCoreData().getMessageCount().getValue() == 1);
    }

    @Test
    public void testPerformUpdateTooHighVehicleId() {

        vehicle.setGlobalId((long)Integer.MAX_VALUE + 1);

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        thrown.expect(IllegalArgumentException.class);
        bsmProducer.performUpdate(device, null, null, 0.0, null);
    }

    @Test
    public void testPerformUpdateTooLowVehicleId() {

        vehicle.setGlobalId((long)Integer.MIN_VALUE - 1);

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        thrown.expect(IllegalArgumentException.class);
        bsmProducer.performUpdate(device, null, null, 0.0, null);
    }

    @Test
    public void testPerformUpdateTooHighSpeed() {

        vehicle.setSpeed(UtilsUnitConversion.convertOneFiftiethMetersPerSecondToMetersPerSecond(Speed.MAX + 10));

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(Speed.UNAVAILABLE == bsm.getCoreData().getSpeed().getValue());
    }

    @Test
    public void testPerformUpdateTooLowSpeed() {

        vehicle.setSpeed(UtilsUnitConversion.convertOneFiftiethMetersPerSecondToMetersPerSecond(Speed.MIN - 10));

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(Speed.UNAVAILABLE == bsm.getCoreData().getSpeed().getValue());
    }

    @Test
    public void testPerformUpdateTooHighSteeringWheelAngle() {

        vehicle.setSteeringAngle(UtilsUnitConversion.convertOneAndAHalfDegreesToDegrees(SteeringWheelAngle.MAX + 10));

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(SteeringWheelAngle.MAX == bsm.getCoreData().getAngle().getValue());
    }

    @Test
    public void testPerformUpdateTooLowSteeringWheelAngle() {

        vehicle.setSteeringAngle(UtilsUnitConversion.convertOneAndAHalfDegreesToDegrees(SteeringWheelAngle.MIN - 10));

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(SteeringWheelAngle.MIN == bsm.getCoreData().getAngle().getValue());
    }

    @Test
    public void testPerformUpdateNoSteeringWheelAngle() {

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(SteeringWheelAngle.UNAVAILABLE == bsm.getCoreData().getAngle().getValue());
    }

    @Test
    public void testPerformUpdateTooHighAcceleration() {

        vehicle.setAcceleration(UtilsUnitConversion.convertOneHundredthMetersPerSecondToMetersPerSecond(Acceleration.MAX + 10));

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(Acceleration.MAX == bsm.getCoreData().getAccelerationSet().getLongitude().getValue());
    }

    @Test
    public void testPerformUpdateTooLowAcceleration() {

        vehicle.setAcceleration(UtilsUnitConversion.convertOneHundredthMetersPerSecondToMetersPerSecond(Acceleration.MIN - 10));

        OBUDevice device = new OBUDevice(new OBUDeviceInfo(vehicle, new ArrayList<BasicMessage>(), 123));

        bsmProducer.performUpdate(device, null, null, 0.0, null);

        BasicSafetyMessage bsm = getBSM(device);

        assertTrue(Acceleration.MIN == bsm.getCoreData().getAccelerationSet().getLongitude().getValue());
    }

    /**
     * Gets the basic safety message from a device.
     * 
     * @param device The device to get the BSMs from.
     * @return The BSMs.
     */
    private BasicSafetyMessage getBSM(OBUDevice device) {

        Object data = device.getAppMessages().get(0).getData();
        String bsmBytes = new String((byte[])data);
        return MessageFrame.decodeBSM(bsmBytes);
    }
}
