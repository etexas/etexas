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
package org.etexascode.apps;

import static org.junit.Assert.*;

import java.lang.reflect.InvocationTargetException;

import org.etexascode.apps.LightChangeCalculator.LightState;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class LightStateTest {

    LightState ls;

    @Before
    public void setUp() throws NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        ls = new LightState();
    }

    @Test
    public void testConstructor() {
        assertEquals(LightChangeCalculator.Color.RED, ls.color);
        assertNull(ls.timeToChange);
    }

    @Test
    public void testAdvance() {
        // Test Cycle
        assertEquals(LightChangeCalculator.Color.RED, ls.color);
        ls.advance();
        assertEquals(LightChangeCalculator.Color.GREEN, ls.color);
        ls.advance();
        assertEquals(LightChangeCalculator.Color.YELLOW, ls.color);
        ls.advance();
        assertEquals(LightChangeCalculator.Color.RED, ls.color);

        // Test remove time to change
        ls.timeToChange = 90.0;
        assertNotNull(ls.timeToChange);
        ls.advance();
        assertNull(ls.timeToChange);
    }

    @Test
    public void testClone() {
        assertEquals(ls, ls.clone());
    }

    @Test
    public void testEquals() {
        assertEquals(ls, ls);
        assertNotEquals(ls, null);
        assertNotEquals(ls, "");
        LightState ls2 = ls.clone();

        ls2.color = LightChangeCalculator.Color.GREEN;
        assertNotEquals(ls, ls2);
        assertNotEquals(ls2, ls);

        ls2 = ls.clone();
        ls2.timeToChange = 10.0;
        assertNotEquals(ls, ls2);
        assertNotEquals(ls2, ls);

        ls2 = ls.clone();
        ls2.color = LightChangeCalculator.Color.GREEN;
        ls2.timeToChange = 10.0;
        assertNotEquals(ls, ls2);
        assertNotEquals(ls2, ls);

        ls.timeToChange = 15.0;
        assertNotEquals(ls, ls2);
        assertNotEquals(ls2, ls);

        ls.timeToChange = 10.0;
        ls.color = LightChangeCalculator.Color.GREEN;
        assertEquals(ls, ls2);
        assertEquals(ls2, ls);
    }

    @Test
    public void testCompareColor() {
        LightState ls2 = ls.clone();
        // test GREEN
        ls2.color = LightChangeCalculator.Color.GREEN;
        assertEquals(0, ls2.compareColor(LightChangeCalculator.Color.GREEN));
        assertEquals(1, ls2.compareColor(LightChangeCalculator.Color.YELLOW));
        assertEquals(1, ls2.compareColor(LightChangeCalculator.Color.RED));

        ls2 = ls.clone();
        // test YELLOW
        ls2.color = LightChangeCalculator.Color.YELLOW;
        assertEquals(-1, ls2.compareColor(LightChangeCalculator.Color.GREEN));
        assertEquals(0, ls2.compareColor(LightChangeCalculator.Color.YELLOW));
        assertEquals(1, ls2.compareColor(LightChangeCalculator.Color.RED));

        ls2 = ls.clone();
        // test RED
        ls2.color = LightChangeCalculator.Color.RED;
        assertEquals(-1, ls2.compareColor(LightChangeCalculator.Color.GREEN));
        assertEquals(-1, ls2.compareColor(LightChangeCalculator.Color.YELLOW));
        assertEquals(0, ls2.compareColor(LightChangeCalculator.Color.RED));
    }
}
