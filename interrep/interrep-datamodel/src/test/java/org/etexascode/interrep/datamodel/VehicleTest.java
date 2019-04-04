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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.etexascode.interrep.datamodel.Vehicle.VEHICLE_TYPE;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test using the xml file below:/src/main/resources/vehiclemanager.xml <?xml version="1.0"
 * encoding="UTF-8" standalone="yes"?> <VehicleManager>
 * <VEHICLE_HISTORY_SIZE>4</VEHICLE_HISTORY_SIZE> <ERROR_TOLERANCE>0.01</ERROR_TOLERANCE>
 * <currentMessageTime>0.0</currentMessageTime>
 * <currentProjectedVehicleID>0</currentProjectedVehicleID> <timeoutLength>30</timeoutLength>
 * <geoCalculatorType>1</geoCalculatorType> <vehicles> <entry> <key>2</key>
 * <value xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type= "vehicle">
 * <vehicleID>2</vehicleID> <x>1619.8900800000001</x> <y>6834.04272</y> <z>0.0</z>
 * <speed>0.0</speed> <length>426.72</length> <acceleration>0.0</acceleration> <width>213.36</width>
 * <laneID>6</laneID> <heading>180.01696957700102</heading> <height>0.0</height> <type>CAR</type>
 * <brakePressed>false</brakePressed> </value> </entry> </vehicles> </VehicleManager>
 *
 * @author amauldon
 * @author ablatt
 */
public class VehicleTest {

    /**
     * Read in vehicle manager, create test vehicle manager, empty vehicle list
     */
    VehicleManager vm = JaxbRead.readJaxManager("/vehiclemanager.xml", VehicleManager.class);

    List<Vehicle> vlist = new ArrayList<Vehicle>();

    /** set of vehicle ids */
    Set<Integer> ids = new HashSet<Integer>();

    /** hardcode some vehicle fields */
    int laneID = 6;

    String vehicleId = "Vehicle:2";

    double x = 1801.68;

    double y = 1689.02;

    double z = 1.00;

    double speed = 35.00;

    double length = 425.0;

    double acceleration = 5.0;

    double width = 215.0;

    double heading = 180.016;

    double height = 10.0;

    /** tolerance */
    double delta = 0.00001;

    /** test vehicles */
    Vehicle vehicle = new Vehicle();

    Vehicle vehicleToo = new Vehicle();

    @Before
    public void setUp() throws Exception {
        vehicle = vm.getVehicle(vehicleId);
        vehicleToo = vehicle;

    }

    @After
    public void tearDown() throws Exception {

    }

    @Test
    public void testEqualsVehicle() {
        assertEquals(vehicle, vehicleToo);
        assertFalse(vehicle.equals(new String()));
        Vehicle v = new Vehicle(vehicle);
        v.setLatitude(vehicle.getLatitude() + 0.0002);
        assertFalse(vehicle.equals(v));
        v.setLatitude(vehicle.getLatitude());
        v.setLongitude(vehicle.getLongitude() + 0.0002);
        assertFalse(vehicle.equals(v));
    }

    @Test
    public void testGetAcceleration() {
        assertEquals(0.0, vehicle.getAcceleration(), delta);
    }

    @Test
    public void testGetHeading() {
        assertEquals(180.016969, vehicle.getHeading(), delta);
        Vehicle vehicleTest = new Vehicle(2, 0.0, 0.0, 0.0, 0.0, 0.0);
        assertEquals(0.0, vehicleTest.getHeading(), delta);

    }

    @Test
    public void testGetHeight() {
        assertEquals(0.0, vehicle.getHeight(), delta);
    }

    @Test
    public void testGetLaneID() {
        assertEquals(6, vehicle.getLaneID(), delta);
    }

    @Test
    public void testGetLength() {
        assertEquals(426.72, vehicle.getLength(), delta);
    }

    @Test
    public void testGetSpeed() {
        assertEquals(0.0, vehicle.getSpeed(), delta);
    }

    @Test
    public void testGetType() {
        assertEquals(VEHICLE_TYPE.CAR, vehicle.getType());
    }

    @Test
    public void testGetVehicleID() {
        assertEquals(2, vehicle.getVehicleID());
    }

    @Test
    public void testGetWidth() {
        assertEquals(213.36, vehicle.getWidth(), delta);
    }

    @Test
    public void testGetX() {
        assertEquals(1619.89008, vehicle.getX(), delta);

    }

    @Test
    public void testGetY() {
        assertEquals(6834.04272, vehicle.getY(), delta);
    }

    @Test
    public void testGetZ() {
        assertEquals(0.0, vehicle.getZ(), delta);
    }

    @Test
    public void testIsBrakePressed() {
        assertEquals(false, vehicle.isBrakePressed());
    }

    @Test
    public void testSetAcceleration() {
        vehicle.setAcceleration(acceleration);
        assertEquals(acceleration, vehicle.getAcceleration(), delta);
    }

    @Test
    public void testSetBrakePressed() {
        vehicle.setBrakePressed(true);
        assertEquals(true, vehicle.isBrakePressed());
    }

    @Test
    public void testSetHeading() {
        vehicle.setHeading(heading);
        assertEquals(heading, vehicle.getHeading(), delta);
    }

    @Test
    public void testSetHeight() {
        vehicle.setHeight(height);
        assertEquals(height, vehicle.getHeight(), delta);
    }

    @Test
    public void testSetLaneID() {
        vehicle.setLaneID(laneID);
        assertTrue(laneID == vehicle.getLaneID());
    }

    @Test
    public void testSetLength() {
        vehicle.setLength(length);
        assertEquals(length, vehicle.getLength(), delta);
    }

    @Test
    public void testSetSpeed() {
        vehicle.setSpeed(speed);
        assertEquals(speed, vehicle.getSpeed(), delta);
    }

    @Test
    public void testSetType() {
        vehicle.setType(VEHICLE_TYPE.TRACTOR_TRAILER);
        assertEquals(VEHICLE_TYPE.TRACTOR_TRAILER, vehicle.getType());
    }

    @Test
    public void testSetVehicleID() {
        vehicle.setVehicleID(6);
        assertEquals(6, vehicle.getVehicleID());
    }

    @Test
    public void testSetWidth() {
        vehicle.setWidth(width);
        assertEquals(width, vehicle.getWidth(), delta);
    }

    @Test
    public void testSetX() {
        vehicle.setX(x);
        assertEquals(x, vehicle.getX(), delta);
    }

    @Test
    public void testSetY() {
        vehicle.setY(y);
        assertEquals(y, vehicle.getY(), delta);
    }

    @Test
    public void testSetZ() {
        vehicle.setZ(z);
        assertEquals(z, vehicle.getZ(), delta);
    }

    @Test
    public void testToString() {
        assertEquals(vehicle.toString(), vehicle.toString());
        Vehicle v = new Vehicle(1, 20, 20, 0, 0, 0);
        assertFalse(vehicle.toString().equals(v.toString()));
        assertTrue(v.toString().equals(v.toString()));
    }

    @Test
    public void testGenRunRiseHeading() {

        assertEquals(-0.99999999999, vehicle.genRunRiseHeading()[0], .005);
        assertEquals(0, vehicle.genRunRiseHeading()[1], .005);
    }

    @Test
    public void testIsSpeedSet() {
        Vehicle v = new Vehicle();
        assertFalse(v.isSpeedSet());
        v.setSpeed(5);
        assertTrue(v.isSpeedSet());
    }

    @Test
    public void testIsAccelerationSet() {
        Vehicle v = new Vehicle();
        assertFalse(v.isAccelerationSet());
        v.setAcceleration(5);
        assertTrue(v.isAccelerationSet());
    }

    @Test
    public void testIsLaneIDSet() {
        Vehicle v = new Vehicle();
        assertFalse(v.isLaneIDSet());
        v.setLaneID(5);
        assertTrue(v.isLaneIDSet());
    }

    @Test
    public void testgetLaneID() {
        Vehicle v = new Vehicle();
        assertTrue(v.getLaneID() == 0);
        v.setLaneID(5);
        assertTrue(v.getLaneID() == 5);
    }

    @Test
    public void testIsHeadingSet() {
        Vehicle v = new Vehicle();
        assertFalse(v.isHeadingSet());
        v.setHeading(5);
        assertTrue(v.isHeadingSet());
    }

    @Test
    public void testClone() {
        assertEquals(vehicle, vehicle.clone());
    }

    @Test
    public void testEqualsId1() {
        Vehicle v = new Vehicle();
        v.setVehicleID(vehicle.getVehicleID());
        assertTrue(vehicle.equalsId(v));
    }

    @Test
    public void testEqualsId2() {
        Vehicle v = new Vehicle();
        v.setVehicleID(vehicle.getVehicleID() + 5);
        assertFalse(vehicle.equalsId(v));
    }

    @Test
    public void testEqualsId3() {
        assertFalse(vehicle.equalsId(new IDable() {

            @Override
            public boolean equalsId(IDable entity) {
                return false;
            }

            @Override
            public String getProperId() {
                return null;
            }
        }));
    }

    @Test
    public void testIsSteeringAngleAvailable() {
        Vehicle v = new Vehicle(1, 20, 20, 0, 0, 0);
        assertFalse(v.isSteeringAngleAvailable());
        v.setSteeringAngle(50);
        assertTrue(v.isSteeringAngleAvailable());
    }

    @Test
    public void testGetProperId() {
        Vehicle v = new Vehicle();
        v.setVehicleID(42);
        assertEquals("Vehicle:42", v.getProperId());
    }
}
