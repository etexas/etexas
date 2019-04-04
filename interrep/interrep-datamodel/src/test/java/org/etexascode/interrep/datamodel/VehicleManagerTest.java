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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.etexascode.interrep.datamodel.Vehicle.VEHICLE_TYPE;
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
 * <value xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="vehicle">
 * <vehicleID>2</vehicleID> <x>1619.8900800000001</x> <y>6834.04272</y> <z>0.0</z>
 * <speed>0.0</speed> <length>426.72</length> <acceleration>0.0</acceleration> <width>213.36</width>
 * <laneID>6</laneID> <heading>180.01696957700102</heading> <height>0.0</height> <type>CAR</type>
 * <brakePressed>false</brakePressed> </value> </entry> </vehicles> </VehicleManager>
 * 
 * @author bmauldon
 */
public class VehicleManagerTest {

    /** Read in vehicle manager, create test vehicle manager, empty vehicle list */
    VehicleManager vm = JaxbRead.readJaxManager("/vehiclemanager.xml", VehicleManager.class);

    VehicleManager vmtest = JaxbRead.readJaxManager("/vehiclemanager.xml", VehicleManager.class);

    List<Vehicle> vlist = new ArrayList<Vehicle>();

    /** set of vehicle ids */
    Set<String> ids = new HashSet<String>();

    /** hardcode some vehicle manager fields */
    double msgTime = 30.0005;

    int laneID = 6;

    String vehicleId = "Vehicle:2";

    /** hardcode fields to make 2 vehicles for testing in list */
    double x1 = 1640.0;

    double y1 = 6870.0;

    double x2 = 1670.0;

    double y2 = 6900.0;

    double z = 0.0;

    double speed = 35.0;

    double length = 425.0;

    double acceleration = 5.0;

    double width = 215.0;

    double heading1 = 180.016;

    double heading2 = 175.00;

    String vehicleId1 = "Vehicle:4";

    String vehicleId2 = "Vehicle:6";

    int laneId1 = 7;

    int laneId2 = 8;

    VEHICLE_TYPE type = VEHICLE_TYPE.CAR;

    boolean brakePressed = false;

    /** tolerance */
    double delta = 0.00001;

    /** vehicles */
    Vehicle vehicle1 = new Vehicle();

    Vehicle vehicle2 = new Vehicle();

    @Before
    public void setUp() throws Exception {
        /** test vehicle manager */
        /** set up vehicle 1 */
        vehicle1.setX(x1);
        vehicle1.setY(y1);
        vehicle1.setZ(z);
        vehicle1.setSpeed(speed);
        vehicle1.setLength(length);
        vehicle1.setAcceleration(acceleration);
        vehicle1.setBrakePressed(brakePressed);
        vehicle1.setHeading(heading1);
        vehicle1.setHeight(0.0);
        vehicle1.setLaneID(laneId1);
        vehicle1.setType(type);
        vehicle1.setVehicleID(4);
        vehicle1.setWidth(width);
        /** add to vehicle list */
        vlist.add(vehicle1);
        /** set up vehicle 2 */
        vehicle2.setX(x2);
        vehicle2.setY(y2);
        vehicle2.setZ(z);
        vehicle2.setSpeed(speed);
        vehicle2.setLength(length);
        vehicle2.setAcceleration(acceleration);
        vehicle2.setBrakePressed(brakePressed);
        vehicle2.setHeading(heading2);
        vehicle2.setHeight(0.0);
        vehicle2.setLaneID(laneId2);
        vehicle2.setType(type);
        vehicle2.setVehicleID(6);
        vehicle2.setWidth(width);

        /** add to vehicle list */
        vlist.add(vehicle2);
        /** add vehicle ids to set */
        ids.add(vehicleId1);
        ids.add(vehicleId2);
    }

    @After
    public void tearDown() throws Exception {
        vlist.clear();
        vehicle1 = null;
        vehicle2 = null;
        ids.clear();
    }

    @Test
    public void testAddVehicle() {
        vm.addVehicle(vehicle1);
        assertEquals(vehicle1, vm.getVehicle(vehicleId1));

    }

    @Test
    public void testAddVehicles() {
        vm.addVehicles(vlist);
        assertEquals(vehicle1, vm.getVehicle(vehicleId1));
        assertEquals(vehicle2, vm.getVehicle(vehicleId2));

    }

    @Test
    public void testEqualsVehicleManager() {
        assertEquals(vm, vmtest);
        vm.addVehicle(vehicle1);
        assertNotEquals(vm, vmtest);
        vm.getAllVehicleIds().clear();
        vmtest.getAllVehicleIds().clear();
        vm.addVehicle(vehicle1);
        vmtest.addVehicle(vehicle2);
        assertNotEquals(vm, vmtest);
        assertFalse(vm.equals(new String()));
    }

    @Test
    public void testGetAllVehicleIds() {
        vm.addVehicles(vlist);
        assertEquals(ids, vm.getAllVehicleIds());

    }

    @Test
    public void testGetVehicle() {
        vm.addVehicles(vlist);
        assertEquals(vehicle1, vm.getVehicle(vehicleId1));
    }

    @Test
    public void testGetVehiclesInLane() {
        assertEquals(vm.getVehicle("Vehicle:2"), vm.getVehiclesInLane(laneID).get(0));
    }

    @Test
    public void testRemoveVehicle() {
        vm.addVehicles(vlist);
        vm.removeVehicle(vehicleId);
        assertNull(vm.getVehicle(vehicleId));
    }

    @Test
    public void testToString() {
        assertEquals(vm.toString(), vm.toString());
    }

    @Test
    public void testIterator() {
        Set<String> tmp = vm.getAllVehicleIds();
        int i = 0;
        for (Vehicle v : vm.getIterable()) {
            assertTrue(tmp.contains(v.getProperId()));
            i++;
        }
        assertEquals(i, tmp.size());
    }
}
