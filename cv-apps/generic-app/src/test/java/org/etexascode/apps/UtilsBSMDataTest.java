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

import static org.junit.Assert.assertEquals;

import java.nio.ByteBuffer;

import org.etexascode.interrep.datamodel.BSM;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.VehicleSize;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UtilsBSMDataTest {

    private final static Logger LOGGER = LoggerFactory.getLogger(UtilsBSMDataTest.class);

    ByteBuffer blobbuffer = ByteBuffer.allocate(38);

    @Test
    public void testGetBSMData() {

        BasicSafetyMessage input = new BasicSafetyMessage();
        input.setMsgID("msgID");

        byte[] blob = { 0x00, // count -> 0
                0x00, 0x00, 0x00, 0x01, // tempID -> 1
                0x00, 0x02, // second -> 2
                0x00, (byte)0x98, (byte)0x96, (byte)0x80, // latitude -> 1.0
                0x00, (byte)0x98, (byte)0x96, (byte)0x80, // longitude -> 1.0
                0x00, 0x03, // elevation -> 3
                0x00, 0x00, 0x00, 0x04, // accuracy -> 4
                0x00, 0x64, // speed (3 bits transmission, 9 bits speed converted to m/s) ->
                            // NEUTRAL, 2.0
                0x00, 0x05, // heading -> 5
                0x06, // steering angle -> 6
                0x00, 0x07, // lat accel -> 7
                0x00, 0x07, // long accel -> 7
                0x08, // vert accel -> 8
                0x00, 0x09, // yaw -> 9
                0x00, 0x0A // brake status -> 10

        };
        int width = 11;
        int length = 12;

        blobbuffer.put(blob);
        LOGGER.debug(blobbuffer.toString());
        ByteBuffer buffer = BitUtils.getVehicleSizeBytes(width, length, 10, 14);
        LOGGER.debug(blobbuffer.toString());
        blobbuffer.put(buffer);
        LOGGER.debug(blobbuffer.toString());
        input.setBlob1(blobbuffer.array());

        BSM actualOutput = UtilsBSMData.getBSMData(input);

        assertEquals("msgID did not match.", "msgID", actualOutput.getDSRCmsgID());
        assertEquals("msgCnt did not match.", 0, actualOutput.getMsgCount());
        assertEquals("tempID did not match.", 1, actualOutput.getTempID());
        assertEquals("second did not match.", 2, actualOutput.getSecond());
        assertEquals("latitude did not match.", 1.0, actualOutput.getLatitude(), .001);
        assertEquals("longitude did not match.", 1.0, actualOutput.getLongitude(), .001);
        assertEquals("elevation did not match.", 3, actualOutput.getElevation());
        assertEquals("accuracy did not match.", 4, actualOutput.getAccuracy());
        assertEquals("transmission did not match.", BSM.Transmission.NEUTRAL, actualOutput.getTransmission());
        assertEquals("speed did not match.", 2.0, actualOutput.getSpeed(), .001);
        assertEquals("heading did not match.", 5, actualOutput.getHeading());
        assertEquals("steering angle did not match.", 6, actualOutput.getSteeringAngle());
        assertEquals("lat accel did not match.", 7, actualOutput.getLateralAcceleration());
        assertEquals("long accel did not match.", 7, actualOutput.getLongitudeAcceleration());
        assertEquals("vert accel did not match.", 8, actualOutput.getVerticalAcceleration());
        assertEquals("yaw did not match.", 9, actualOutput.getYaw());
        assertEquals("brake status did not match.", 10, actualOutput.getBrakeStatus());
        assertEquals("vehicle width did not match.", 11, actualOutput.getVehicleWidth());
        assertEquals("vehicle length did not match.", 12, actualOutput.getVehicleLength());
    }

    @Test
    public void testGetBSMVerboseData() {

        BasicSafetyMessageVerbose input = new BasicSafetyMessageVerbose();

        input.setMsgID("msgID");
        input.setMsgCnt((short)0);
        input.setId(new byte[] { (byte)0, (byte)0, (byte)0, (byte)1 });
        input.setSecMark(2);
        input.setLat(10000000);
        input.setLong(10000000);
        input.setElev(new byte[] { (byte)0x00, (byte)0x03 });
        input.setAccuracy(new byte[] { (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x04 });
        input.setSpeed(new byte[] { (byte)0x00, (byte)0x64 });
        input.setHeading(5);
        input.setAngle(new byte[] { 6 });
        input.setAccelSet(new byte[] { (byte)0x00, (byte)0x07, (byte)0x00, (byte)0x07, (byte)0x08, (byte)0x00, (byte)0x09 });
        input.setBrakes(new byte[] { (byte)0x00, (byte)0x0A });
        VehicleSize vs = new VehicleSize();
        vs.setWidth(11);
        vs.setLength(12);
        input.setSize(vs);

        BSM actualOutput = UtilsBSMData.getBSMVerboseData(input);

        assertEquals("msgID did not match.", "msgID", actualOutput.getDSRCmsgID());
        assertEquals("msgCnt did not match.", 0, actualOutput.getMsgCount());
        assertEquals("tempID did not match.", 1, actualOutput.getTempID());
        assertEquals("second did not match.", 2, actualOutput.getSecond());
        assertEquals("latitude did not match.", 1.0, actualOutput.getLatitude(), .001);
        assertEquals("longitude did not match.", 1.0, actualOutput.getLongitude(), .001);
        assertEquals("elevation did not match.", 3, actualOutput.getElevation());
        assertEquals("accuracy did not match.", 4, actualOutput.getAccuracy());
        assertEquals("transmission did not match.", BSM.Transmission.NEUTRAL, actualOutput.getTransmission());
        assertEquals("speed did not match.", 2.0, actualOutput.getSpeed(), .001);
        assertEquals("heading did not match.", 5, actualOutput.getHeading());
        assertEquals("steering angle did not match.", 6, actualOutput.getSteeringAngle());
        assertEquals("lat accel did not match.", 7, actualOutput.getLateralAcceleration());
        assertEquals("long accel did not match.", 7, actualOutput.getLongitudeAcceleration());
        assertEquals("vert accel did not match.", 8, actualOutput.getVerticalAcceleration());
        assertEquals("yaw did not match.", 9, actualOutput.getYaw());
        assertEquals("brake status did not match.", 10, actualOutput.getBrakeStatus());
        assertEquals("vehicle width did not match.", 11, actualOutput.getVehicleWidth());
        assertEquals("vehicle length did not match.", 12, actualOutput.getVehicleLength());
    }

    @Test
    public void testGetBSMVerboseData2() {

        BasicSafetyMessageVerbose input = new BasicSafetyMessageVerbose();

        input.setMsgID("msgID");
        input.setMsgCnt((short)20);
        input.setId(new byte[] { (byte)0, (byte)0, (byte)0, (byte)3 });
        input.setSecMark(3);
        input.setLat(20000000);
        input.setLong(20000000);
        input.setElev(new byte[] { (byte)0xF0, (byte)0x00 });
        input.setAccuracy(new byte[] { (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x02 });
        input.setSpeed(new byte[] { (byte)0x00, (byte)0x64 });
        input.setHeading(5);
        input.setAngle(new byte[] { 6 });
        input.setAccelSet(new byte[] { (byte)0x00, (byte)0x07, (byte)0x00, (byte)0x07, (byte)0x08, (byte)0x00, (byte)0x09 });
        input.setBrakes(new byte[] { (byte)0x00, (byte)0x0A });
        VehicleSize vs = new VehicleSize();
        vs.setWidth(11);
        vs.setLength(12);
        input.setSize(vs);

        BSM actualOutput = UtilsBSMData.getBSMVerboseData(input);

        assertEquals("msgID did not match.", "msgID", actualOutput.getDSRCmsgID());
        assertEquals("msgCnt did not match.", 20, actualOutput.getMsgCount());
        assertEquals("tempID did not match.", 3, actualOutput.getTempID());
        assertEquals("second did not match.", 3, actualOutput.getSecond());
        assertEquals("latitude did not match.", 2.0, actualOutput.getLatitude(), .001);
        assertEquals("longitude did not match.", 2.0, actualOutput.getLongitude(), .001);
        assertEquals("elevation did not match.", 0, actualOutput.getElevation());
        assertEquals("accuracy did not match.", 2, actualOutput.getAccuracy());
        assertEquals("transmission did not match.", BSM.Transmission.NEUTRAL, actualOutput.getTransmission());
        assertEquals("speed did not match.", 2.0, actualOutput.getSpeed(), .001);
        assertEquals("heading did not match.", 5, actualOutput.getHeading());
        assertEquals("steering angle did not match.", 6, actualOutput.getSteeringAngle());
        assertEquals("lat accel did not match.", 7, actualOutput.getLateralAcceleration());
        assertEquals("long accel did not match.", 7, actualOutput.getLongitudeAcceleration());
        assertEquals("vert accel did not match.", 8, actualOutput.getVerticalAcceleration());
        assertEquals("yaw did not match.", 9, actualOutput.getYaw());
        assertEquals("brake status did not match.", 10, actualOutput.getBrakeStatus());
        assertEquals("vehicle width did not match.", 11, actualOutput.getVehicleWidth());
        assertEquals("vehicle length did not match.", 12, actualOutput.getVehicleLength());
    }
}
