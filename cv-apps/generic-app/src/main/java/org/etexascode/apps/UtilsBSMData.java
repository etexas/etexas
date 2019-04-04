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

package org.etexascode.apps;

import java.nio.ByteBuffer;
import java.util.BitSet;

import org.etexascode.interrep.datamodel.BSM;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.VehicleSize;

/**
 * Utility class to bind information received from the j2735 Basic Safety Messages to the java BSM
 * class.
 * 
 * @author dranker
 */
public class UtilsBSMData {

    /**
     * Translate a J2735 {@link BasicSafetyMessage} to a {@link BSM}
     * 
     * @param inputMessage The message to translate.
     * @return The translated message.
     */
    public static BSM getBSMData(BasicSafetyMessage inputMessage) {
        BSM outputMessage = new BSM();

        // Set the message ID field
        outputMessage.setDSRCmsgID(inputMessage.getMsgID());

        // blob1 contains most of the messages actual info
        byte[] messageInfo = inputMessage.getBlob1();

        // Reads the first byte from the blob, which is the message count
        outputMessage.setMsgCount(ByteBuffer.wrap(messageInfo).get());

        // Reads the next 4 bytes from the blob, which is the temporary ID of the message
        int tempID = ByteBuffer.wrap(messageInfo, 1, 4).getInt();
        outputMessage.setTempID(tempID);

        // Reads the next 2 bytes from the blob, which is the second of the message
        short msgSecond = ByteBuffer.wrap(messageInfo, 5, 2).getShort();
        outputMessage.setSecond(msgSecond);

        // Reads the next 4 bytes from the blob, which is the latitude
        double msgLatitude = UtilsUnitConversion.convertToDegrees(ByteBuffer.wrap(messageInfo, 7, 4).getInt());
        outputMessage.setLatitude(msgLatitude);

        // Reads the next 4 bytes from the blob, which is the longitude
        double msgLongitude = UtilsUnitConversion.convertToDegrees(ByteBuffer.wrap(messageInfo, 11, 4).getInt());
        outputMessage.setLongitude(msgLongitude);

        // Reads the next 2 bytes from the blob, which is the elevation
        short msgElevation = ByteBuffer.wrap(messageInfo, 15, 2).getShort();
        outputMessage.setElevation(msgElevation);

        // Reads the next 4 bytes from the blob, which is the accuracy of the
        // location data
        int msgLocationAccuracy = ByteBuffer.wrap(messageInfo, 17, 4).getInt();
        outputMessage.setAccuracy(msgLocationAccuracy);

        // Reads the next 2 bytes from the blob, which is the speed
        short tranmissionAndSpeed = ByteBuffer.wrap(messageInfo, 21, 2).getShort();
        double speed = tranmissionAndSpeed & 8191; // 0d8191 == 0b1111111111111
        speed *= 0.02; // convert speed from (0.02) meters per second to meters per second
        int transmission = tranmissionAndSpeed >> 13;
        transmission &= 7; // 0d7 == 0b111
        outputMessage.setSpeed(speed);
        outputMessage.setTransmission(BSM.getTransmissionType(transmission));

        // Reads the next 2 bytes from the blob, which is heading
        short heading = ByteBuffer.wrap(messageInfo, 23, 2).getShort();
        outputMessage.setHeading(heading);

        // Reads the next byte from the blob, which is steering wheel angle
        outputMessage.setSteeringAngle(ByteBuffer.wrap(messageInfo).get(25));

        // Bytes 27-33 are an octet string which is simply a byte[] in java
        // These bytes contain the longitudinal acceleration,lateral acceleration,
        // vertical acceleration, and yaw
        ByteBuffer acceleration4Way = ByteBuffer.wrap(messageInfo, 26, 7);

        outputMessage.setLongitudeAcceleration(acceleration4Way.getShort());
        outputMessage.setLateralAcceleration(acceleration4Way.getShort());
        outputMessage.setVerticalAcceleration(acceleration4Way.get());
        outputMessage.setYaw(acceleration4Way.getShort());

        // Reads the next 2 bytes from the blob, which is the brake status
        short brakeStatus = ByteBuffer.wrap(messageInfo, 33, 2).getShort();
        outputMessage.setBrakeStatus(brakeStatus);

        // Reads the final 3 bytes from the blob, which is the vehicle size (14 bits for length, 10
        // bits for width)
        byte[] bytes = { messageInfo[35], messageInfo[36], messageInfo[37] };
        BitSet bits = BitUtils.fromByteArray(bytes);
        int width = BitUtils.getWidth(bits, 0, 10);
        int length = BitUtils.getLength(bits, 10, 24);
        outputMessage.setVehicleLength(length);
        outputMessage.setVehicleWidth(width);

        return outputMessage;
    }

    /**
     * Translate a J2735 {@link BasicSafetyMessageVerbose} to a {@link BSM}
     * 
     * @param inputMessage The message to translate.
     * @return The translated message.
     */
    public static BSM getBSMVerboseData(BasicSafetyMessageVerbose inputMessage) {
        BSM outputMessage = new BSM();

        // Set the message ID field
        outputMessage.setDSRCmsgID(inputMessage.getMsgID());

        // Reads the first byte from the blob, which is the message count
        outputMessage.setMsgCount(inputMessage.getMsgCnt());

        // Reads the next 4 bytes from the blob, which is the temporary ID of the message
        int tempID = ByteBuffer.wrap(inputMessage.getId()).getInt();
        outputMessage.setTempID(tempID);

        // Reads the next 2 bytes from the blob, which is the second of the message
        outputMessage.setSecond((short)inputMessage.getSecMark());

        // Reads the next 4 bytes from the blob, which is the latitude
        // immediately converts from 1/10th microdegrees to degrees
        outputMessage.setLatitude(UtilsUnitConversion.convertToDegrees(inputMessage.getLat()));

        // Reads the next 4 bytes from the blob, which is the longitude
        // immediately converts from 1/10th microdegrees to degrees
        outputMessage.setLongitude(UtilsUnitConversion.convertToDegrees(inputMessage.getLong()));

        // Reads the next 2 bytes from the blob, which is the elevation
        short msgElevation = ByteBuffer.wrap(inputMessage.getElev()).getShort();
        if (msgElevation == (short)0xF000) { // TODO: dgolman - refer to pg 158 of j2735 spec
                                             // document to check for elevation ranges
            msgElevation = 0;
        }
        outputMessage.setElevation(msgElevation);

        // Reads the next 4 bytes from the blob, which is the accuracy of the
        // location data
        int msgLocationAccuracy = ByteBuffer.wrap(inputMessage.getAccuracy()).getInt();
        outputMessage.setAccuracy(msgLocationAccuracy);

        // Reads the next 2 bytes from the blob, which is the speed
        short tranmissionAndSpeed = ByteBuffer.wrap(inputMessage.getSpeed()).getShort();
        double speed = tranmissionAndSpeed & 8191; // 0d8191 == 0b1111111111111
        speed *= 0.02; // convert speed from (0.02) meters per second to meters per second
        int transmission = tranmissionAndSpeed >> 13;
        transmission &= 7; // 0d7 == 0b111
        outputMessage.setSpeed(speed);
        outputMessage.setTransmission(BSM.getTransmissionType(transmission));

        // Reads the next 2 bytes from the blob, which is heading
        outputMessage.setHeading((short)inputMessage.getHeading());

        // Reads the next byte from the blob, which is steering wheel angle
        outputMessage.setSteeringAngle(ByteBuffer.wrap(inputMessage.getAngle()).get(0));

        // Bytes 27-33 are an octet string which is simply a byte[] in java

        // These bytes contain the lateral acceleration, longitudinal acceleration,
        // vertical acceleration, and yaw
        ByteBuffer acceleration4Way = ByteBuffer.wrap(inputMessage.getAccelSet());
        outputMessage.setLongitudeAcceleration(acceleration4Way.getShort());
        outputMessage.setLateralAcceleration(acceleration4Way.getShort());
        outputMessage.setVerticalAcceleration(acceleration4Way.get());
        outputMessage.setYaw(acceleration4Way.getShort());

        // Reads the next 2 bytes from the blob, which is the brake status
        short brakeStatus = ByteBuffer.wrap(inputMessage.getBrakes()).getShort();
        outputMessage.setBrakeStatus(brakeStatus);

        // Reads the final 2 bytes from the blob, which is the vehicle size
        VehicleSize size = inputMessage.getSize();
        if (size != null) {
            outputMessage.setVehicleLength(size.getLength());
            outputMessage.setVehicleWidth(size.getWidth());
        }

        return outputMessage;
    }
}
