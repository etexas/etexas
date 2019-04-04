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

import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 * @author ablatt
 */
public class LaneMovementTest {

    /** Test Lane Movement */
    LaneMovement lmove = new LaneMovement();

    LaneMovement lmove2 = new LaneMovement();

    int id = 5;

    Movement move = Movement.LEFT_TURN_ON_RED;

    @Before
    public void setUp() throws Exception {
        lmove.setMovement(move);
        lmove.setMovementId(id);
        lmove2 = lmove;
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testEqualsLaneMovement() {
        assertEquals(lmove, lmove2);
        assertFalse(lmove.equals(new String()));
    }

    @Test
    public void testGetMovement() {
        assertEquals(move, lmove.getMovement());
    }

    @Test
    public void testGetMovementId() {
        assertEquals(id, lmove.getMovementId());
    }

    @Test
    public void testSetMovement() {
        lmove.setMovement(Movement.U_TURN);
        assertEquals(Movement.U_TURN, lmove.getMovement());
    }

    @Test
    public void testSetMovementId() {
        lmove.setMovementId(2);
        assertEquals(2, lmove.getMovementId());
    }

    @Test
    public void testEqualsId1() {
        assertTrue(lmove.equalsId(lmove2));
    }

    @Test
    public void testEqualsId2() {
        assertFalse(lmove.equalsId(new IDable() {

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
    public void testGetProperId() {
        assertEquals("LaneMovement:5", lmove.getProperId());
    }
}
