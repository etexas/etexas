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
package org.etexascode.apps.hybrid.defaultimplementation;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SimpleDetermineVehicleLengthTest {

    List<Vehicle> actual = null;

    SimpleDetermineVehicleLength sdvl = null;

    double closeEnoughVal = 0.05;

    @Before
    public void setUp() {
        actual = new ArrayList<Vehicle>(5);

        actual.add(genVeh1());
        actual.add(genVeh2());
        actual.add(genVeh3());
        actual.add(genVeh4());
        actual.add(genVeh5());

        sdvl = new SimpleDetermineVehicleLength();
    }

    @After
    public void tearDown() {
        actual = null;
        sdvl = null;
    }

    @Test
    public void testGetVehicleLengths() {
        List<Vehicle> reted = sdvl.getVehicleLengths(actual);
        for (Vehicle v : reted) {
            assertEquals(497.84, v.getLength(), closeEnoughVal);
        }
    }

    private Vehicle genVeh1() {
        return GenVehicleFunctions.genVehicle(46.6, 51.2, 17.9, 497.84, 1, 1);
    }

    private Vehicle genVeh2() {
        return GenVehicleFunctions.genVehicle(46.6, 51.2, 17.9, 17.5, 1, 1);
    }

    private Vehicle genVeh3() {
        return GenVehicleFunctions.genVehicle(46.6, 82.6, 11.1, 15.2, 2, 1);
    }

    private Vehicle genVeh4() {
        return GenVehicleFunctions.genVehicle(19.2, 42.0, 8, 0, 1, 1729);
    }

    private Vehicle genVeh5() {
        return GenVehicleFunctions.genVehicle(46.6, 51.2, 17.9, -537.8, -26, -42);
    }
}
