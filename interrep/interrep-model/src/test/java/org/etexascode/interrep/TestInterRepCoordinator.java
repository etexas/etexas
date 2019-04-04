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
package org.etexascode.interrep;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.datalayer.tests.TestInterRepCoordinatorDataLayer;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test for the public methods in InterRepCoordinator
 * 
 * @author cdeisher (one method- rest of authorship unknown)
 */
public class TestInterRepCoordinator {

    TestInterRepCoordinatorDataLayer data = null;

    List<IInterRep> sims = null;

    TestIntersectionSimulation sim1 = null;

    InterRepCoordinator irc = null;

    @Before
    public void setup() {
        data = new TestInterRepCoordinatorDataLayer();
        sim1 = new TestIntersectionSimulation();

        sims = new ArrayList<IInterRep>(1);
        sims.add(sim1);

        irc = new InterRepCoordinator(sims, data);
    }

    @After
    public void teardown() {
        data = null;
        sims = null;
        sim1 = null;
        irc = null;
    }

    @Test
    public void testConstructor() {
        InterRepCoordinator coord = new InterRepCoordinator(sims, data);
        assertTrue(sims == coord.internal);
        for (double d : coord.internalTimes) {
            assertEquals(0.0, d, 0.05);
        }

        for (double d : coord.stepSizes) {
            assertEquals(sim1.getTimeStepInterval(), d, 0.05);
        }

        assertEquals(sim1.getTimeStepInterval(), coord.minStepSize, 0.05);
        assertTrue(data == coord.data);
    }

    @Test
    public void testGetMinStepSize() {
        assertEquals(sim1.getTimeStepInterval(), irc.getMinStepSize(), 0.05);
    }

    @Test
    public void testUpdate() {
        // run
        assertTrue(irc.update(1));

        TestIntersectionSimulation sim2 = new TestIntersectionSimulation();
        sim2.setCurrentTimeStep(1);
        sim2.setSimTime(1);
        sim2.setTimeStepInterval(0.0);
        sim2.setUpdate(false);
        sims.add(sim2);
        irc = new InterRepCoordinator(sims, data);

        assertFalse(irc.update(0));

    }
}
