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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TestSimpleVehiclesByLane {

    List<Vehicle> input = null;

    Map<Integer, List<Vehicle>> expected = null;

    SimpleVehiclesByLane svbl = null;

    @Before
    public void setup() {
        input = new ArrayList<Vehicle>(5);

        input.add(genVeh1());
        input.add(genVeh2());
        input.add(genVeh3());
        input.add(genVeh4());
        input.add(genVeh5());

        expected = new HashMap<Integer, List<Vehicle>>();

        List<Vehicle> ones = new LinkedList<Vehicle>();
        ones.add(genVeh1());
        ones.add(genVeh2());
        ones.add(genVeh4());

        List<Vehicle> twos = new LinkedList<Vehicle>();
        twos.add(genVeh3());

        List<Vehicle> other = new LinkedList<Vehicle>();
        other.add(genVeh5());

        expected.put(1, ones);
        expected.put(2, twos);
        expected.put(-26, other);

        svbl = new SimpleVehiclesByLane();
    }

    @After
    public void teardown() {
        input = null;
        expected = null;
        svbl = null;
    }

    @Test
    public void testFilterVehiclesByLane() {
        assertEquals(expected, svbl.filterVehiclesByLane(input));
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
