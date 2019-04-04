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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 */
public class LaneNodeTest {

    /** Test lane node */
    double x = 180.67;

    double y = 213.12;

    double z = 0.0;

    double width = 1.0;

    LaneNode lnode = new LaneNode(x, y);

    LaneNode lnodeToo = new LaneNode(lnode);

    /** tolerance */
    double delta = 0.0001;

    @Before
    public void setUp() throws Exception {

    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testEqualsLaneNode() {
        assertEquals(lnode, lnodeToo);
        assertFalse(lnode.equals(new String()));
    }

    @Test
    public void testGetWidth() {
        assertEquals(width, lnode.getWidth(), delta);
    }

    @Test
    public void testGetX() {
        assertEquals(x, lnode.getX(), delta);
    }

    @Test
    public void testGetY() {
        assertEquals(y, lnode.getY(), delta);
    }

    @Test
    public void testGetZ() {
        assertEquals(z, lnode.getZ(), delta);
    }

    @Test
    public void testSetWidth() {
        lnode.setWidth(215.0);
        assertEquals(215.0, lnode.getWidth(), delta);
    }

    @Test
    public void testSetX() {
        lnode.setX(190.85);
        assertEquals(190.85, lnode.getX(), delta);
    }

    @Test
    public void testSetY() {
        lnode.setY(215.09);
        assertEquals(215.09, lnode.getY(), delta);
    }

    @Test
    public void testSetZ() {
        lnode.setZ(10.0);
        assertEquals(10.0, lnode.getZ(), delta);

    }

    @Test
    public void testConstructor() {
        LaneNode ln = new LaneNode(x, y, z, width);
        assertEquals(x, ln.getX(), delta);
        assertEquals(y, ln.getY(), delta);
        assertEquals(z, ln.getZ(), delta);
        assertEquals(width, ln.getWidth(), delta);
    }
}
