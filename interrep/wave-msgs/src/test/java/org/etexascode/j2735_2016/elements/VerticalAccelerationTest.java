/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.j2735_2016.elements;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the vertical acceleration element.
 * 
 * @author ttevendale
 */
public class VerticalAccelerationTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration(VerticalAcceleration.MIN);
        assertTrue(VerticalAcceleration.MIN == verticalAcceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        verticalAcceleration = new VerticalAcceleration(VerticalAcceleration.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration(VerticalAcceleration.MAX);
        assertTrue(VerticalAcceleration.MAX == verticalAcceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        verticalAcceleration = new VerticalAcceleration(VerticalAcceleration.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 12;
        VerticalAcceleration verticalAcceleration = new VerticalAcceleration(randomNum);
        assertTrue(randomNum == verticalAcceleration.getValue());
    }

    @Test
    public void testSetValueMin() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration();
        verticalAcceleration.setValue(VerticalAcceleration.MIN);
        assertTrue(VerticalAcceleration.MIN == verticalAcceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        verticalAcceleration.setValue(VerticalAcceleration.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration();
        verticalAcceleration.setValue(VerticalAcceleration.MAX);
        assertTrue(VerticalAcceleration.MAX == verticalAcceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        verticalAcceleration.setValue(VerticalAcceleration.MAX + 1);
    }

    @Test
    public void testSetValue() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration();
        int randomNum = -40;
        verticalAcceleration.setValue(randomNum);
        assertTrue(randomNum == verticalAcceleration.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        VerticalAcceleration verticalAcceleration = new VerticalAcceleration(VerticalAcceleration.MIN);
        String encodedVerticalAcceleration = verticalAcceleration.encodeUPER();
        assertTrue("00000000".equals(encodedVerticalAcceleration));

        // test max
        verticalAcceleration = new VerticalAcceleration(VerticalAcceleration.MAX);
        encodedVerticalAcceleration = verticalAcceleration.encodeUPER();
        assertTrue("11111110".equals(encodedVerticalAcceleration));

        int randomNumber = 0;
        verticalAcceleration = new VerticalAcceleration(randomNumber);
        encodedVerticalAcceleration = verticalAcceleration.encodeUPER();
        assertTrue("01111111".equals(encodedVerticalAcceleration));
    }

    @Test
    public void testDecodeUPER() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration();

        // test min
        String remainingBits = verticalAcceleration.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(VerticalAcceleration.MIN == verticalAcceleration.getValue());

        // test max
        remainingBits = verticalAcceleration.decodeUPER("11111110");
        assertTrue("".equals(remainingBits));
        assertTrue(VerticalAcceleration.MAX == verticalAcceleration.getValue());

        int expectedNumber = 23;
        remainingBits = verticalAcceleration.decodeUPER("10010110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == verticalAcceleration.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration();
        thrown.expect(IllegalArgumentException.class);
        verticalAcceleration.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration();
        String remainingBits = verticalAcceleration.decodeUPER("01100100110");
        assertTrue("110".equals(remainingBits));
        assertTrue(verticalAcceleration.getValue() == -27);
    }

    @Test
    public void testHashCode() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration(-89);
        VerticalAcceleration verticalAcceleration2 = new VerticalAcceleration(127);

        assertFalse(verticalAcceleration.hashCode() == verticalAcceleration2.hashCode());
        assertTrue(verticalAcceleration.hashCode() == verticalAcceleration.hashCode());
        assertTrue(verticalAcceleration2.hashCode() == verticalAcceleration2.hashCode());

        VerticalAcceleration verticalAcceleration3 = new VerticalAcceleration(verticalAcceleration.getValue());

        assertTrue(verticalAcceleration.hashCode() == verticalAcceleration3.hashCode());
        assertFalse(verticalAcceleration2.hashCode() == verticalAcceleration3.hashCode());
    }

    @Test
    public void testEquals() {

        VerticalAcceleration verticalAcceleration = new VerticalAcceleration(-12);

        assertFalse(verticalAcceleration.equals(null));

        assertTrue(verticalAcceleration.equals(verticalAcceleration));

        VerticalAcceleration verticalAcceleration2 = new VerticalAcceleration(50);

        assertFalse(verticalAcceleration.equals(new String()));
        assertFalse(verticalAcceleration.equals(verticalAcceleration2));

        verticalAcceleration2.setValue(verticalAcceleration.getValue());
        assertTrue(verticalAcceleration.equals(verticalAcceleration2));
    }
}
