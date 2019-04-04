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
package com.harmonia.apps.exp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class IndsMeaningTest {

    IndsMeaning im1 = null;

    IndsMeaning im2 = null;

    IndsMeaning im3 = null;

    IndsMeaning im4 = null;

    List<ISignalIndication> sis1 = null;

    List<ISignalIndication> sis2 = null;

    List<ISignalIndication> sis3 = null;

    int[] laneIds = null;

    List<ISignalIndication> siis1 = null;

    List<ISignalIndication> siis2 = null;

    List<ISignalIndication> siis3 = null;

    ISignalIndication sii1 = null;

    ISignalIndication sii2 = null;

    @Before
    public void setup() {
        laneIds = new int[] { 42, 1729, 1337 };

        siis1 = new ArrayList<ISignalIndication>(1);
        SignalIndication si1 = new SignalIndication();
        si1.setColorIndication(SignalIndication.Color.GREEN);
        si1.setStateIndication(SignalIndication.State.STEADY);
        si1.setTypeIndication(SignalIndication.Type.BALL);
        si1.setTimeToChange(42.0);
        siis1.add(si1);
        sii1 = si1;

        SignalIndication si2 = new SignalIndication();
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.FLASHING);
        si2.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si2.setTimeToChange(1729.0);

        siis2 = new ArrayList<ISignalIndication>(0);
        siis3 = new ArrayList<ISignalIndication>(1);
        siis3.add(si2);
        sii2 = si2;
        im1 = new IndsMeaning(siis1, laneIds[0]);
        im2 = new IndsMeaning(siis2, laneIds[1]);
        im3 = new IndsMeaning(siis3, laneIds[2]);
        im4 = new IndsMeaning(siis1, laneIds[2]);
    }

    @After
    public void teardown() {
        im1 = null;
        im2 = null;
        im3 = null;
        sis1 = null;
        sis2 = null;
        sis3 = null;
        laneIds = null;
        siis1 = null;
        sii1 = null;
        sii2 = null;
        siis2 = null;
    }

    @Test
    public void testConstructor() {
        List<ISignalIndication> sis = new ArrayList<ISignalIndication>(0);
        int laneId = 42;
        IndsMeaning im = new IndsMeaning(sis, laneId);
        assertTrue(sis == im.sis);
        assertEquals(1, im.laneIds.size());
        assertTrue(im.laneIds.contains(laneId));
    }

    @Test
    public void testCanCombineWith1() {
        assertFalse(im1.canCombineWith(im2));
    }

    @Test
    public void testCanCombineWith2() {
        assertFalse(im1.canCombineWith(im3));
    }

    @Test
    public void testCanCombineWith3() {
        assertTrue(im1.canCombineWith(im4));
    }

    @Test
    public void testContainsSpecial1() {
        assertTrue(im1.containsSpecial(siis1, sii1));
    }

    @Test
    public void testContainsSpecial2() {
        assertFalse(im1.containsSpecial(siis1, sii2));
    }

    @Test
    public void testCombine() {
        im1.combine(im3);
        assertEquals(2, im1.laneIds.size());
        assertEquals(1, im3.laneIds.size());
        assertTrue(im1.laneIds.contains(laneIds[0]));
        assertTrue(im1.laneIds.contains(laneIds[2]));
        assertTrue(im3.laneIds.contains(laneIds[2]));
    }

    @Test
    public void testCloseEnough1() {
        assertTrue(im1.closeEnough(4.0, 4.01));
    }

    @Test
    public void testCloseEnough2() {
        assertTrue(im1.closeEnough(4.01, 4.0));
    }

    @Test
    public void testCloseEnough3() {
        assertFalse(im1.closeEnough(4.0, 4.06));
    }

    @Test
    public void testCloseEnough4() {
        assertFalse(im1.closeEnough(4.06, 4.0));
    }
}
