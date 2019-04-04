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
 * Unit tests for the acceleration element.
 * 
 * @author ttevendale
 */
public class AccelerationTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Acceleration acceleration = new Acceleration(Acceleration.MIN);
        assertTrue(Acceleration.MIN == acceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        acceleration = new Acceleration(Acceleration.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Acceleration acceleration = new Acceleration(Acceleration.MAX);
        assertTrue(Acceleration.MAX == acceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        acceleration = new Acceleration(Acceleration.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 120;
        Acceleration acceleration = new Acceleration(randomNum);
        assertTrue(randomNum == acceleration.getValue());
    }

    @Test
    public void testSetValueMin() {

        Acceleration acceleration = new Acceleration();
        acceleration.setValue(Acceleration.MIN);
        assertTrue(Acceleration.MIN == acceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        acceleration.setValue(Acceleration.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Acceleration acceleration = new Acceleration();
        acceleration.setValue(Acceleration.MAX);
        assertTrue(Acceleration.MAX == acceleration.getValue());

        thrown.expect(IllegalArgumentException.class);
        acceleration.setValue(Acceleration.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Acceleration acceleration = new Acceleration();
        int randomNum = 50;
        acceleration.setValue(randomNum);
        assertTrue(randomNum == acceleration.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Acceleration acceleration = new Acceleration(Acceleration.MIN);
        String encodedAcceleration = acceleration.encodeUPER();
        assertTrue("000000000000".equals(encodedAcceleration));

        // test max
        acceleration = new Acceleration(Acceleration.MAX);
        encodedAcceleration = acceleration.encodeUPER();
        assertTrue("111110100001".equals(encodedAcceleration));

        int randomNumber = 0;
        acceleration = new Acceleration(randomNumber);
        encodedAcceleration = acceleration.encodeUPER();
        assertTrue("011111010000".equals(encodedAcceleration));
    }

    @Test
    public void testDecodeUPER() {

        Acceleration acceleration = new Acceleration();

        // test min
        String remainingBits = acceleration.decodeUPER("000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Acceleration.MIN == acceleration.getValue());

        // test max
        remainingBits = acceleration.decodeUPER("111110100001");
        assertTrue("".equals(remainingBits));
        assertTrue(Acceleration.MAX == acceleration.getValue());

        int expectedNumber = -1990;
        remainingBits = acceleration.decodeUPER("000000001010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == acceleration.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Acceleration acceleration = new Acceleration();
        thrown.expect(IllegalArgumentException.class);
        acceleration.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Acceleration acceleration = new Acceleration();
        String remainingBits = acceleration.decodeUPER("0000001010101");
        assertTrue("1".equals(remainingBits));
        assertTrue(acceleration.getValue() == -1958);
    }

    @Test
    public void testHashCode() {

        Acceleration acceleration = new Acceleration(5);
        Acceleration acceleration2 = new Acceleration(10);

        assertFalse(acceleration.hashCode() == acceleration2.hashCode());
        assertTrue(acceleration.hashCode() == acceleration.hashCode());
        assertTrue(acceleration2.hashCode() == acceleration2.hashCode());

        Acceleration acceleration3 = new Acceleration(acceleration.getValue());

        assertTrue(acceleration.hashCode() == acceleration3.hashCode());
        assertFalse(acceleration2.hashCode() == acceleration3.hashCode());
    }

    @Test
    public void testEquals() {

        Acceleration acceleration = new Acceleration(5);

        assertFalse(acceleration.equals(null));

        assertTrue(acceleration.equals(acceleration));

        Acceleration acceleration2 = new Acceleration(acceleration.getValue() + 5);

        assertFalse(acceleration.equals(new String()));
        assertFalse(acceleration.equals(acceleration2));

        acceleration2.setValue(acceleration.getValue());
        assertTrue(acceleration.equals(acceleration2));
    }
}
