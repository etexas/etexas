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
package org.etexascode.webapp.ra;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author dgolman
 */
public class SimDriverDataTest {

    private double simTime = 40.9;

    private Object[] messages = new Object[1];

    private boolean finished = true;

    private double delta = .001;

    private SimDriverData sdd = new SimDriverData();

    @Before
    public void setUp() throws Exception {
        messages[0] = "hello";
        sdd.setFinished(true);
        sdd.setSimTime(simTime);
        sdd.setMessages(messages);
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testGetSimTime() {
        assertEquals(simTime, sdd.getSimTime(), delta);
    }

    @Test
    public void testSetSimTime() {
        sdd.setSimTime(39.1);
        assertEquals(39.1, sdd.getSimTime(), delta);

    }

    @Test
    public void testGetMessagesTest() {
        assertEquals("hello", sdd.getMessages()[0]);
    }

    @Test
    public void testSetMessages() {
        sdd.setMessages(messages);
        assertEquals("hello", sdd.getMessages()[0]);
    }

    @Test
    public void testIsFinished() {
        assertEquals(finished, sdd.isFinished());
    }

    @Test
    public void testSetFinished() {
        sdd.setFinished(false);
        assertEquals(false, sdd.isFinished());
    }
}
