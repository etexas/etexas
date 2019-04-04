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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test class for Vehicle Speed Commands
 * 
 * @author ttevendale
 */
public class VehicleSpeedCommandTest {

    @Before
    public void setUp() throws Exception {}

    @After
    public void tearDown() throws Exception {

    }

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {
        int badSpeedCommand = 500;
        int speed1 = 12;
        int speed2 = 37;
        int speed3 = 2;
        int speed4 = 2343;
        VehicleSpeedCommand vsc1 = new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_ACCELERATE_TO_XX, speed1);
        VehicleSpeedCommand vsc2 = new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_DECELERATE_TO_XX, speed2);
        VehicleSpeedCommand vsc3 = new VehicleSpeedCommand(1, VehicleSpeedCommand.NORMAL_ACCELERATE_TO_XX, speed3);
        VehicleSpeedCommand vsc4 = new VehicleSpeedCommand(1, VehicleSpeedCommand.NORMAL_DECELERATE_TO_XX, speed4);

        assertTrue(vsc1.getSpeedCommand() == VehicleSpeedCommand.MAX_ACCELERATE_TO_XX);
        assertTrue(vsc2.getSpeedCommand() == VehicleSpeedCommand.MAX_DECELERATE_TO_XX);
        assertTrue(vsc3.getSpeedCommand() == VehicleSpeedCommand.NORMAL_ACCELERATE_TO_XX);
        assertTrue(vsc4.getSpeedCommand() == VehicleSpeedCommand.NORMAL_DECELERATE_TO_XX);

        assertTrue(vsc1.getSpeed() == speed1);
        assertTrue(vsc2.getSpeed() == speed2);
        assertTrue(vsc3.getSpeed() == speed3);
        assertTrue(vsc4.getSpeed() == speed4);

        try {
            new VehicleSpeedCommand(1, badSpeedCommand, 5);
            thrown.expect(IllegalArgumentException.class);
        }
        catch (IllegalArgumentException e) {}
    }

    @Test
    public void testToString() {
        VehicleSpeedCommand vsc1 = new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_ACCELERATE_TO_XX, 12);
        VehicleSpeedCommand vsc2 = new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_DECELERATE_TO_XX, 12);
        VehicleSpeedCommand vsc3 = new VehicleSpeedCommand(1, VehicleSpeedCommand.NORMAL_ACCELERATE_TO_XX, 12);
        VehicleSpeedCommand vsc4 = new VehicleSpeedCommand(1, VehicleSpeedCommand.NORMAL_DECELERATE_TO_XX, 12);

        String maxAStr = vsc1.toString();
        String maxDStr = vsc2.toString();
        String normalAStr = vsc3.toString();
        String normalDStr = vsc4.toString();

        System.out.println(maxAStr);
        System.out.println(maxDStr);
        System.out.println(normalAStr);
        System.out.println(normalDStr);

        String testString = "VehicleID = 1, Command: Max Accelerate , Speed: 12.0";

        assertTrue(testString.equals(maxAStr));
        testString = "VehicleID = 1, Command: Max Decelerate , Speed: 12.0";
        assertTrue(testString.equals(maxDStr));
        testString = "VehicleID = 1, Command: Normal Accelerate , Speed: 12.0";
        assertTrue(testString.equals(normalAStr));
        testString = "VehicleID = 1, Command: Normal Decelerate , Speed: 12.0";
        assertTrue(testString.equals(normalDStr));

    }

    @Test
    public void testEquals() {
        VehicleSpeedCommand vsc1 = new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_ACCELERATE_TO_XX, 12);
        VehicleSpeedCommand vsc2 = new VehicleSpeedCommand(vsc1.getVehicleID(), vsc1.getSpeedCommand(), vsc1.getSpeed());
        assertTrue(vsc1.equals(vsc2));
        vsc2 = new VehicleSpeedCommand(vsc1.getVehicleID(), vsc1.getSpeedCommand(), vsc1.getSpeed() * 2);
        assertFalse(vsc1.equals(vsc2));
        vsc2 = new VehicleSpeedCommand(vsc1.getVehicleID(), VehicleSpeedCommand.MAX_DECELERATE_TO_XX, vsc1.getSpeed() / 2);
        assertFalse(vsc1.equals(vsc2));
        vsc2 = new VehicleSpeedCommand(vsc1.getVehicleID() + 1, vsc1.getSpeedCommand(), vsc1.getSpeed());
        assertFalse(vsc1.equals(vsc2));
        assertFalse(vsc1.equals(new String()));

    }
}
