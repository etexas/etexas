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

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.etexascode.CoberturaIgnore;
import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.OBUDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735_2016.elements.Acceleration;
import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus.AntiLockBrake;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus.AuxiliaryBrake;
import org.etexascode.j2735_2016.elements.BrakeAppliedStatus;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied.BrakeBoost;
import org.etexascode.j2735_2016.elements.DSRCmsgID;
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
import org.etexascode.j2735_2016.elements.StabilityControlStatus.StabilityControl;
import org.etexascode.j2735_2016.elements.SteeringWheelAngle;
import org.etexascode.j2735_2016.elements.TemporaryID;
import org.etexascode.j2735_2016.elements.TractionControlStatus.TractionControl;
import org.etexascode.j2735_2016.elements.TransmissionState;
import org.etexascode.j2735_2016.elements.TransmissionState.Transmission;
import org.etexascode.j2735_2016.elements.VerticalAcceleration;
import org.etexascode.j2735_2016.frames.AccelerationSet4Way;
import org.etexascode.j2735_2016.frames.BSMcoreData;
import org.etexascode.j2735_2016.frames.BrakeSystemStatus;
import org.etexascode.j2735_2016.frames.PositionalAccuracy;
import org.etexascode.j2735_2016.frames.VehicleSize;
import org.etexascode.j2735_2016.messages.BasicSafetyMessage;
import org.etexascode.j2735_2016.messages.MessageFrame;
import org.etexascode.wavesim.WaveMessage;

/**
 * A BSM producing Connected Vehicle App.
 * 
 * @author ttevendale
 */
public class BSMProducer2016App implements IOBUBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_BSM_PRODUCER_2016_APP = "BSMProducer2016App";

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * An incremented counter for the current number of generated messages (modulo 128).
     */
    private short msgCount = 0;

    /**
     * Builds a basic safety message byte[].
     * 
     * @param vehicle The vehicle to parse.
     * @param msgCount The message count to use.
     * @param simTime The current simulation time.
     * @return The BSM as a byte[].
     */
    private byte[] buildBSM(IVehicle vehicle, double simTime) {

        BasicSafetyMessage bsm = new BasicSafetyMessage(buildCoreData(vehicle, simTime));

        MessageFrame frame = new MessageFrame(DSRCmsgID.BSM, bsm.encodeUPER());
        byte[] bsmBytes = null;

        try {

            bsmBytes = frame.encodeHexUPER().getBytes("UTF-8");
        }
        catch (UnsupportedEncodingException e) {

            throw new IllegalStateException("Unsupported Encoding Exception was thrown while creating BSM.");
        }

        return bsmBytes;
    }

    /**
     * Builds the core data part of the message.
     * 
     * @param vehicle The vehicle to parse.
     * @param msgCount The message count to use.
     * @param simTime The current simulation time.
     * @return The core data.
     */
    private BSMcoreData buildCoreData(IVehicle vehicle, double simTime) {

        return new BSMcoreData(new MsgCount(msgCount),
                buildId(vehicle.getGlobalId()),
                buildSecMark(simTime),
                buildLatitude(vehicle.getLatitude()),
                buildLongitude(vehicle.getLongitude()),
                new Elevation(Elevation.UNAVAILABLE),
                buildAccuracy(),
                buildTransmission(),
                buildSpeed(vehicle.getSpeed()),
                buildHeading(vehicle.getHeading()),
                buildAngle(vehicle),
                buildAccelerationSet(vehicle.getAcceleration()),
                buildBrakes(vehicle.isBrakePressed()),
                new VehicleSize((int)vehicle.getWidth(), (int)vehicle.getLength()));
    }

    /**
     * Builds the temporary vehicle ID part of the message.
     * 
     * @param vehicleId The vehicle ID to use.
     * @return The ID.
     */
    private TemporaryID buildId(long vehicleId) {

        if (vehicleId > Integer.MAX_VALUE || vehicleId < Integer.MIN_VALUE) {

            throw new IllegalArgumentException("The vehicle ID has become either too large or too small for the scope of this application.");
        }
        return new TemporaryID((int)vehicleId);
    }

    /**
     * Builds the second mark part for the message. NOTE: Converts from seconds (simulation time) to
     * milliseconds and ensures that the value is within a minute.
     * 
     * @param simTime The simulation time to calculate the second mark with.
     * @return The second mark.
     */
    private DSecond buildSecMark(double simTime) {

        // constrains to a minute
        double seconds = simTime % 60;

        return new DSecond((int)UtilsUnitConversion.convertSecondsToMilliseconds(seconds));
    }

    /**
     * Builds the latitude part of the message.
     * 
     * @param latitude The latitude to use.
     * @return The latitude.
     */
    private Latitude buildLatitude(double latitude) {

        return new Latitude(UtilsUnitConversion.convertToOneTenthMicrodegree(latitude));
    }

    /**
     * Builds the longitude part of the message.
     * 
     * @param longitude The longitude to use.
     * @return The longitude.
     */
    private Longitude buildLongitude(double longitude) {

        return new Longitude(UtilsUnitConversion.convertToOneTenthMicrodegree(longitude));
    }

    /**
     * Builds the accuracy part of the message.
     * 
     * @return The accuracy.
     */
    private PositionalAccuracy buildAccuracy() {

        return new PositionalAccuracy(SemiMajorAxisAccuracy.UNAVAILABLE, SemiMinorAxisAccuracy.UNAVAILABLE, SemiMajorAxisOrientation.UNAVAILABLE);
    }

    /**
     * Builds the transmission part of the message. NOTE: A previous developer said that TEXAS does
     * not appear to support any vehicle transmission states other than forward gears.
     * 
     * @return The transmission.
     */
    private TransmissionState buildTransmission() {

        return new TransmissionState(Transmission.FORWARD_GEARS);
    }

    /**
     * Builds the speed part of the message. NOTE: the speed is converted from m/s to 0.02 m/s.
     * 
     * @param speed The speed to use.
     * @return The speed.
     */
    private Speed buildSpeed(double speed) {

        int convertedSpeed = (int)UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(speed);
        if (convertedSpeed > Speed.MAX || convertedSpeed < Speed.MIN) {

            convertedSpeed = Speed.UNAVAILABLE;
        }
        return new Speed(convertedSpeed);
    }

    /**
     * Builds the heading part of the message.
     * 
     * @param heading The heading to use.
     * @return The heading.
     */
    private Heading buildHeading(double heading) {

        return new Heading(UtilsUnitConversion.convertDegreesToBSMHeading(heading));
    }

    /**
     * Builds the angle part of the message. NOTE: converts degrees to 1.5 degrees. Also this is an
     * imprecise conversion since we don't need it to be precise.
     * 
     * @param vehicle The vehicle to use.
     * @return The angle.
     */
    private SteeringWheelAngle buildAngle(IVehicle vehicle) {

        SteeringWheelAngle angle = new SteeringWheelAngle();

        if (((Vehicle)vehicle).isSteeringAngleAvailable()) {

            int steeringAngle = (int)UtilsUnitConversion.convertDegreesToOneAndAHalfDegrees(vehicle.getSteeringAngle());

            steeringAngle = (steeringAngle > SteeringWheelAngle.MAX) ? SteeringWheelAngle.MAX : steeringAngle;
            steeringAngle = (steeringAngle < SteeringWheelAngle.MIN) ? SteeringWheelAngle.MIN : steeringAngle;

            angle.setValue(steeringAngle);
        }
        else {

            angle.setValue(SteeringWheelAngle.UNAVAILABLE);
        }
        return angle;
    }

    /**
     * Builds the acceleration set part of the message. NOTE: the longitude acceleration is
     * converted from m/s^2 to 0.01 m/s^2.
     * 
     * @param acceleration The acceleration to use.
     * @return The acceleration set.
     */
    private AccelerationSet4Way buildAccelerationSet(double acceleration) {

        int longitude = (int)UtilsUnitConversion.convertMetersPerSecondToOneHundredthMetersPerSecond(acceleration);

        longitude = (longitude > Acceleration.MAX) ? Acceleration.MAX : longitude;
        longitude = (longitude < Acceleration.MIN) ? Acceleration.MIN : longitude;

        return new AccelerationSet4Way(longitude, Acceleration.UNAVAILABLE, VerticalAcceleration.UNAVAILABLE, 0);
    }

    /**
     * Builds the brakes part of the message.
     * 
     * @param brakesPressed The indication if the brakes are pressed or not.
     * @return The brakes.
     */
    private BrakeSystemStatus buildBrakes(boolean brakesPressed) {

        BrakeAppliedStatus wheelBrakes = new BrakeAppliedStatus();
        wheelBrakes.setLeftFront(brakesPressed);
        wheelBrakes.setLeftRear(brakesPressed);
        wheelBrakes.setRightFront(brakesPressed);
        wheelBrakes.setRightRear(brakesPressed);

        return new BrakeSystemStatus(wheelBrakes, TractionControl.UNAVAILABLE, AntiLockBrake.UNAVAILABLE, StabilityControl.UNAVAILABLE, BrakeBoost.UNAVAILABLE, AuxiliaryBrake.UNAVAILABLE);
    }

    /**
     * The frequency at which to produce messages in seconds. SPEC states that 10Hz is the default.
     */
    @AppConfigProperty(value = "0.1", displayName = "frequency", description = "The frequency to send messages in seconds.")
    double frequency;

    @Override
    public void init(String[] appConfigs) {

        frequency = Double.parseDouble(appConfigs[0]);
    }

    @Override
    public void performUpdate(OBUDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        if (lastTxTime != null && simTime - lastTxTime < frequency) {

            return;
        }

        lastTxTime = simTime;

        byte[] bsmBytes = buildBSM(device.getVehicleInfo(), simTime);

        List<BasicMessage> ret = new ArrayList<BasicMessage>(1);
        ret.add(new DSRCMessage(bsmBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, bsmBytes.length));

        msgCount = (short)((msgCount + 1) % 128);

        device.addAppMessages(ret);
    }

    @Override
    @CoberturaIgnore
    public void appShutdown(AppLogger logger) {

        logger.log(APP_NAME_BSM_PRODUCER_2016_APP, "The application has shutdown.");
    }

    @Override
    public String getAppName() {

        return APP_NAME_BSM_PRODUCER_2016_APP;
    }

}