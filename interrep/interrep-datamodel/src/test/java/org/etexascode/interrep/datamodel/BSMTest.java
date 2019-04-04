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

package org.etexascode.interrep.datamodel;

import static org.junit.Assert.assertEquals;

import org.etexascode.interrep.datamodel.BSM.Transmission;
import org.junit.Before;
import org.junit.Test;

public class BSMTest {

    private BSM expectedBsm;

    @Before
    public void setUp() {
        expectedBsm = new BSM();
        expectedBsm.setAccuracy(2);
        expectedBsm.setBrakeStatus((short)3);
        expectedBsm.setDSRCmsgID("Hello world");
        expectedBsm.setElevation((short)4);
        expectedBsm.setHeading((short)5);
        expectedBsm.setLatitude(2.0);
        expectedBsm.setLateralAcceleration((short)1000);
        expectedBsm.setLongitude(2.0);
        expectedBsm.setLongitudeAcceleration((short)1000);
        expectedBsm.setMsgCount(3);
        expectedBsm.setSecond((short)6);
        expectedBsm.setSpeed(2.0);
        expectedBsm.setSteeringAngle(10);
        expectedBsm.setTempID(1);
        expectedBsm.setTransmission(Transmission.NEUTRAL);
        expectedBsm.setVehicleLength(1);
        expectedBsm.setVehicleWidth(2);
        expectedBsm.setVerticalAcceleration((short)29);
        expectedBsm.setYaw(20);
    }

    @Test
    public void testConvertSpeed() {
        assertEquals(BSM.convertSpeed(2), .04, .001);
    }

    @Test
    public void testGetTransmissionText() {
        assertEquals(BSM.getTransmissionText(BSM.Transmission.NEUTRAL), "Neutral");
        assertEquals(BSM.getTransmissionText(BSM.Transmission.PARK), "Park");
        assertEquals(BSM.getTransmissionText(BSM.Transmission.FORWARDGEARS), "Forward Gears");
        assertEquals(BSM.getTransmissionText(BSM.Transmission.REVERSEGEARS), "Reverse Gears");
        assertEquals(BSM.getTransmissionText(BSM.Transmission.RESERVED1), "Reserved 1");
        assertEquals(BSM.getTransmissionText(BSM.Transmission.RESERVED2), "Reserved 2");
        assertEquals(BSM.getTransmissionText(BSM.Transmission.RESERVED3), "Reserved 3");
        assertEquals(BSM.getTransmissionText(BSM.Transmission.UNAVAILABLE), "Unavailable");
    }

    @Test
    public void testGetTransmissionType() {
        assertEquals(BSM.getTransmissionType(0), BSM.Transmission.NEUTRAL);
        assertEquals(BSM.getTransmissionType(1), BSM.Transmission.PARK);
        assertEquals(BSM.getTransmissionType(2), BSM.Transmission.FORWARDGEARS);
        assertEquals(BSM.getTransmissionType(3), BSM.Transmission.REVERSEGEARS);
        assertEquals(BSM.getTransmissionType(4), BSM.Transmission.RESERVED1);
        assertEquals(BSM.getTransmissionType(5), BSM.Transmission.RESERVED2);
        assertEquals(BSM.getTransmissionType(6), BSM.Transmission.RESERVED3);
        assertEquals(BSM.getTransmissionType(7), BSM.Transmission.UNAVAILABLE);
    }

    @Test
    public void testParseSpeedAndTransmission() {
        assertEquals(BSM.parseSpeedAndTransmission(new byte[] { 1, 1 })[0], 0);
        assertEquals(BSM.parseSpeedAndTransmission(new byte[] { 1, 1 })[1], 257);
    }

    @Test
    public void testGetAccuracy() {
        assertEquals(expectedBsm.getAccuracy(), 2);
    }

    @Test
    public void testGetBrakeStatus() {
        assertEquals(expectedBsm.getBrakeStatus(), (short)3);
    }

    @Test
    public void testGetDSRCmsgID() {
        assertEquals(expectedBsm.getDSRCmsgID(), "Hello world");
    }

    @Test
    public void testGetElevation() {
        assertEquals(expectedBsm.getElevation(), (short)4);
    }

    @Test
    public void testGetHeading() {
        assertEquals(expectedBsm.getHeading(), (short)5);
    }

    @Test
    public void testGetLatitude() {
        assertEquals(expectedBsm.getLatitude(), 2.0, .001);
    }

    @Test
    public void testGetLateralAcceleration() {
        assertEquals(expectedBsm.getLateralAcceleration(), (short)1000);
    }

    @Test
    public void testGetLongitude() {
        assertEquals(expectedBsm.getLongitude(), 2.0, .001);
    }

    @Test
    public void testGetLongitudeAcceleration() {
        assertEquals(expectedBsm.getLongitudeAcceleration(), (short)1000);
    }

    @Test
    public void testGetMsgCount() {
        assertEquals(expectedBsm.getMsgCount(), 3);
    }

    @Test
    public void testGetSecond() {
        assertEquals(expectedBsm.getSecond(), (short)6);
    }

    @Test
    public void testGetSpeed() {
        assertEquals(expectedBsm.getSpeed(), 2.0, .001);
    }

    @Test
    public void testGetSteeringAngle() {
        assertEquals(expectedBsm.getSteeringAngle(), 10);
    }

    @Test
    public void testGetTempID() {
        assertEquals(expectedBsm.getTempID(), 1);
    }

    @Test
    public void testGetTransmission() {
        assertEquals(expectedBsm.getTransmission(), Transmission.NEUTRAL);
    }

    @Test
    public void testGetVehicleLength() {
        assertEquals(expectedBsm.getVehicleLength(), 1);
    }

    @Test
    public void testGetVehicleWidth() {
        assertEquals(expectedBsm.getVehicleWidth(), 2);
    }

    @Test
    public void testGetVerticalAcceleration() {
        assertEquals(expectedBsm.getVerticalAcceleration(), (short)29);
    }

    @Test
    public void testGetYaw() {
        assertEquals(expectedBsm.getYaw(), 20);
    }

    @Test
    public void testSetAccuracy() {
        expectedBsm.setAccuracy(2);
        assertEquals(expectedBsm.getAccuracy(), 2);
    }

    @Test
    public void testSetBrakeStatus() {
        expectedBsm.setBrakeStatus((short)3);
        assertEquals(expectedBsm.getBrakeStatus(), (short)3);
    }

    @Test
    public void testSetDSRCmsgID() {
        expectedBsm.setDSRCmsgID("Hello world");
        assertEquals(expectedBsm.getDSRCmsgID(), "Hello world");
    }

    @Test
    public void testSetElevation() {
        expectedBsm.setElevation((short)4);
        assertEquals(expectedBsm.getElevation(), (short)4);
    }

    @Test
    public void testSetHeading() {
        expectedBsm.setHeading((short)5);
        assertEquals(expectedBsm.getHeading(), (short)5);
    }

    @Test
    public void testSetLatitude() {
        expectedBsm.setLatitude(2.0);
        assertEquals(expectedBsm.getLatitude(), 2.0, .001);
    }

    @Test
    public void testSetLateralAcceleration() {
        expectedBsm.setLateralAcceleration((short)1000);
        assertEquals(expectedBsm.getLateralAcceleration(), (short)1000);

    }

    @Test
    public void testSetLongitude() {
        expectedBsm.setLongitude(2.0);
        assertEquals(expectedBsm.getLongitude(), 2.0, .001);
    }

    @Test
    public void testSetLongitudeAcceleration() {
        expectedBsm.setLongitudeAcceleration((short)1000);
        assertEquals(expectedBsm.getLongitudeAcceleration(), (short)1000);
    }

    @Test
    public void testSetMsgCount() {
        expectedBsm.setMsgCount(3);
        assertEquals(expectedBsm.getMsgCount(), 3);
    }

    @Test
    public void testSetSecond() {
        expectedBsm.setSecond((short)3);
        assertEquals(expectedBsm.getSecond(), (short)3);
    }

    @Test
    public void testSetSpeed() {
        expectedBsm.setSpeed(2.0);
        assertEquals(expectedBsm.getSpeed(), 2.0, .001);
    }

    @Test
    public void testSetSteeringAngle() {
        expectedBsm.setSteeringAngle(10);
        assertEquals(expectedBsm.getSteeringAngle(), 10);
    }

    @Test
    public void testSetTempID() {
        expectedBsm.setTempID(1);
        assertEquals(expectedBsm.getTempID(), 1);
    }

    @Test
    public void testSetTransmission() {
        expectedBsm.setTransmission(Transmission.NEUTRAL);
        assertEquals(expectedBsm.getTransmission(), Transmission.NEUTRAL);
    }

    @Test
    public void testSetVehicleLength() {
        expectedBsm.setVehicleLength(1);
        assertEquals(expectedBsm.getVehicleLength(), 1);
    }

    @Test
    public void testSetVehicleWidth() {
        expectedBsm.setVehicleWidth(2);
        assertEquals(expectedBsm.getVehicleWidth(), 2);
    }

    @Test
    public void testSetVerticalAcceleration() {
        expectedBsm.setVerticalAcceleration((short)29);
        assertEquals(expectedBsm.getVerticalAcceleration(), (short)29);
    }

    @Test
    public void testSetYaw() {
        expectedBsm.setYaw(20);
        assertEquals(expectedBsm.getYaw(), 20);
    }

}
