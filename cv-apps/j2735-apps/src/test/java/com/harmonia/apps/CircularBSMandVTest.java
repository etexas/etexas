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

import static org.junit.Assert.assertEquals;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;

import org.etexascode.apps.UtilsMessageImports;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.FullPositionVector;
import org.etexascode.j2735.VehicleSafetyExtension;
import org.etexascode.j2735.VehicleSize;
import org.etexascode.j2735.VehicleStatus;
import org.etexascode.j2735.VehicleStatus.VehicleData;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.test.GenLaneFunctions;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author ablatt
 * @author bmauldon
 * @author cdeisher
 */
public class CircularBSMandVTest {

    /** Create vehicle and lane manager */
    private static final Logger LOGGER = LoggerFactory.getLogger(CircularBSMandVTest.class);

    LaneManager manager = GenLaneFunctions.genLaneManager();

    LaneManager managerPrime = GenLaneFunctions.genLaneManager();

    BasicSafetyMessage bsm = null;

    BasicSafetyMessageVerbose bsmv = null;

    Vehicle v = GenVehicleFunctions.genVehicle(-1600, 26, 180, 180, 4, 2);

    Vehicle finish = null;

    Vehicle vPrime = GenVehicleFunctions.genVehicle(-1600, 26, 180, 180, 4, 2);

    BasicSafetyMessageVerbose bsmvPrime = null;

    double simtime = 0.5;

    @Before
    public void setUp() throws Exception {
        v.setHeading(0.0);
        v.setSpeed(0.0);
        v.setAcceleration(0.0);
        /** Create bsm and bsmv and set lane manager latitude and longitude */

        manager.setLatitude(35);
        manager.setLongitude(35);

        double[] latLon = UtilsLatLongConversion.convertCentimeterOffsetToLatLong(vPrime.getX(), vPrime.getY(), manager.getLatitude(), manager.getLongitude(), manager.getGeoCalculatorType());
        vPrime.setLatitude(latLon[0]);
        vPrime.setLongitude(latLon[1]);

        vPrime.setHeading(0.0);
        vPrime.setSpeed(0.0);
        vPrime.setAcceleration(0.0);

        bsmv = new BasicSafetyMessageVerbose();

        // setup our bsmv

        bsmv.setMsgID(DSRCMessageID.MSG_ID_BSM);
        bsmv.setSecMark(65535);
        bsmv.setElev(new byte[] { (byte)0xF0, 0 });
        bsmv.setAccuracy(new byte[] { 0, 0, 0, 0 });
        VehicleSize vehicleSize = new VehicleSize();
        vehicleSize.setWidth((int)v.getWidth());
        vehicleSize.setLength((int)v.getLength());
        bsmv.setSize(vehicleSize);
        ByteBuffer id4Bytes = ByteBuffer.allocate(4);
        bsmv.setId(id4Bytes.putInt(v.getVehicleID()).array());
        bsmv.setAngle(new byte[] { (byte)0 });
        ByteBuffer brakes2Bytes = ByteBuffer.allocate(2);
        bsmv.setBrakes(brakes2Bytes.putShort((short)0).array());

        bsm = new BasicSafetyMessage();
        int bsmSize = BSMProducerApp.getBasicSafetyMessage(simtime, vPrime, false, (short)0, bsm);
    }

    @After
    public void teardown() {
        finish = null;
    }

    @Test
    public void testBSM() {
        finish = UtilsMessageImports.importBSM(bsm, manager, manager.getGeoCalculatorType());
        finish.setVehicleID(vPrime.getVehicleID());
        finish.setAcceleration(0.0);
        assertEquals(vPrime, finish);
    }

    @Test
    public void testBSMPart2() throws UnsupportedEncodingException {
        Vehicle v = GenVehicleFunctions.genVehicle(-1600, 26, 180, 80, 4, 2);
        v.setHeading(0.0);
        v.setSpeed(0.0);
        v.setAcceleration(0.0);
        BasicSafetyMessage bsm = new BasicSafetyMessage();
        int size = BSMProducerApp.getBasicSafetyMessage(simtime, v, true, (short)0, bsm);

        Assert.assertTrue("", bsm.getSafetyExt() != null && bsm.getStatus() != null);
    }

    @Test
    public void testBSMPart2VehicleData() throws UnsupportedEncodingException {
        Vehicle v = GenVehicleFunctions.genVehicle(-1600, 26, 180, 80, 4, 2);
        v.setHeading(0.0);
        v.setSpeed(0.0);
        v.setAcceleration(0.0);
        BasicSafetyMessage bsm = new BasicSafetyMessage();
        int size = BSMProducerApp.getBasicSafetyMessage(simtime, v, true, (short)0, bsm);
        VehicleStatus exn = bsm.getStatus();
        VehicleData data = exn.getVehicleData();

        Assert.assertTrue("Bumper heights not equal. Was: " + data.getBumpers().getFrnt() + ", Expected: " + v.getHeight(), data.getBumpers().getFrnt() == v.getHeight());

        Assert.assertTrue("Bumper heights not equal. Was: " + data.getBumpers().getRear() + ", Expected: " + v.getHeight(), data.getBumpers().getRear() == v.getHeight());

        Assert.assertTrue("Vehicle Types not equal. Was: " + data.getType().getBytes()[0] + ", Expected: " + v.getType().getValue(), data.getType().getBytes()[0] == v.getType().getValue());
    }

    @Test
    public void testBSMPart2VehicleSafetyExtension() throws UnsupportedEncodingException {
        Vehicle v = GenVehicleFunctions.genVehicle(-1600, 26, 180, 80, 4, 2);
        v.setHeading(0.0);
        v.setAcceleration(0.0);
        v.setSpeed(0.0);
        BasicSafetyMessage bsm = new BasicSafetyMessage();
        int size = BSMProducerApp.getBasicSafetyMessage(simtime, v, true, (short)0, bsm);
        VehicleSafetyExtension exn = bsm.getSafetyExt();
        FullPositionVector vector = exn.getPathHistory().getInitialPosition();

        Assert.assertTrue("FullPositionVector latitudes not equal. Was: " + vector.getLat() + ", Expected: " + (int)v.getLatitude(), vector.getLat() == (int)v.getLatitude());
        Assert.assertTrue("FullPositionVector latitudes not equal. Was: " + vector.getLat() + ", Expected: " + (int)Math.round(v.getLatitude()), vector.getLat() == (int)Math.round(v.getLatitude()));

        Assert.assertTrue("FullPositionVector longitudes not equal. Was: " + vector.getLong() + ", Expected: " + (int)Math.round(v.getLongitude()),
                vector.getLong() == (int)Math.round(v.getLongitude()));

        Assert.assertTrue("FullPositionVector elevations not equal. Was: " + ByteBuffer.wrap(vector.getElevation()).getDouble() + ", Expected: " + v.getElev(), ByteBuffer.wrap(vector.getElevation())
                .getDouble() == v.getElev());

        Assert.assertTrue("FullPositionVector headings not equal. Was: " + vector.getHeading() + ", Expected: " + (int)Math.round(v.getHeading()),
                vector.getHeading() == (int)Math.round(v.getHeading()));

        Assert.assertTrue("FullPositionVector speeds not equal. Was: " + ByteBuffer.wrap(vector.getSpeed()).getDouble() + ", Expected: " + v.getSpeed(),
                ByteBuffer.wrap(vector.getSpeed()).getDouble() == v.getSpeed());
    }

    @Test
    public void testBSMV() throws UnsupportedEncodingException {

        BSMVerboseProducerApp bsmvApp = new BSMVerboseProducerApp();
        int size = bsmvApp.getBasicSafetyMessageVerbose(simtime, v, false, (short)0, bsmv);

        finish = UtilsMessageImports.importBSMV(bsmv, manager, manager.getGeoCalculatorType());

        // TODO: cdeisher-- we shouldn't have to do this, the messageImports should cover for us
        finish.setAcceleration(0.0);
        finish.setLaneID(4);
        finish.setX(-1600);
        finish.setY(26.0);
        assertEquals(v, finish);
    }

    @Test
    public void testBSMVPart2() throws UnsupportedEncodingException {
        Vehicle v = GenVehicleFunctions.genVehicle(-1600, 26, 180, 80, 4, 2);
        v.setHeading(0.0);
        v.setSpeed(0.0);
        v.setAcceleration(0.0);
        BasicSafetyMessageVerbose bsmv = new BasicSafetyMessageVerbose();
        int size = new BSMVerboseProducerApp().getBasicSafetyMessageVerbose(simtime, v, true, (short)0, bsmv);

        Assert.assertTrue("BSMV did not contain SafetyExt or VehicleStatus", bsmv.getSafetyExt() != null && bsmv.getStatus() != null);
    }

    @Test
    public void testBSMVPrime() throws UnsupportedEncodingException {
        vPrime.setHeading(0.0);
        vPrime.setSpeed(0.0);
        vPrime.setAcceleration(0.0);

        int size = new BSMVerboseProducerApp().getBasicSafetyMessageVerbose(simtime, vPrime, false, (short)0, bsmv);
        Vehicle out = UtilsMessageImports.importBSMV(bsmv, manager, manager.getGeoCalculatorType());
        out.setAcceleration(0.0);
        assertEquals(vPrime, out);
    }

    @Test
    public void testBSMPrime() throws UnsupportedEncodingException {
        vPrime.setHeading(0.0);
        vPrime.setSpeed(0.0);
        vPrime.setAcceleration(0.0);
        BasicSafetyMessage bsm = new BasicSafetyMessage();
        int size = BSMProducerApp.getBasicSafetyMessage(simtime, vPrime, false, (short)0, bsm);
        Vehicle out = UtilsMessageImports.importBSM(bsm, manager, manager.getGeoCalculatorType());
        out.setAcceleration(0.0);
        assertEquals(vPrime, out);
    }
}