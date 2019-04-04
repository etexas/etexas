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
 * Unit tests for the speed element.
 * 
 * @author ttevendale
 */
public class SpeedTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Speed speed = new Speed(Speed.MIN);
        assertTrue(Speed.MIN == speed.getValue());

        thrown.expect(IllegalArgumentException.class);
        speed = new Speed(Speed.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Speed speed = new Speed(Speed.MAX);
        assertTrue(Speed.MAX == speed.getValue());

        thrown.expect(IllegalArgumentException.class);
        speed = new Speed(Speed.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 8000;
        Speed speed = new Speed(randomNum);
        assertTrue(randomNum == speed.getValue());
    }

    @Test
    public void testSetValueMin() {

        Speed speed = new Speed();
        speed.setValue(Speed.MIN);
        assertTrue(Speed.MIN == speed.getValue());

        thrown.expect(IllegalArgumentException.class);
        speed.setValue(Speed.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Speed speed = new Speed();
        speed.setValue(Speed.MAX);
        assertTrue(Speed.MAX == speed.getValue());

        thrown.expect(IllegalArgumentException.class);
        speed.setValue(Speed.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Speed speed = new Speed();
        int randomNum = 184;
        speed.setValue(randomNum);
        assertTrue(randomNum == speed.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Speed speed = new Speed(Speed.MIN);
        String encodedSpeed = speed.encodeUPER();
        assertTrue("0000000000000".equals(encodedSpeed));

        // test max
        speed = new Speed(Speed.MAX);
        encodedSpeed = speed.encodeUPER();
        assertTrue("1111111111111".equals(encodedSpeed));

        int randomNumber = 541;
        speed = new Speed(randomNumber);
        encodedSpeed = speed.encodeUPER();
        assertTrue("0001000011101".equals(encodedSpeed));
    }

    @Test
    public void testDecodeUPER() {

        Speed speed = new Speed();

        // test min
        String remainingBits = speed.decodeUPER("0000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Speed.MIN == speed.getValue());

        // test max
        remainingBits = speed.decodeUPER("1111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(Speed.MAX == speed.getValue());

        int expectedNumber = 1789;
        remainingBits = speed.decodeUPER("0011011111101");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == speed.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Speed speed = new Speed();
        thrown.expect(IllegalArgumentException.class);
        speed.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Speed speed = new Speed();
        String remainingBits = speed.decodeUPER("00001000000000");
        assertTrue("0".equals(remainingBits));
        assertTrue(speed.getValue() == 256);
    }

    @Test
    public void testHashCode() {

        Speed speed = new Speed(8000);
        Speed speed2 = new Speed(45);

        assertFalse(speed.hashCode() == speed2.hashCode());
        assertTrue(speed.hashCode() == speed.hashCode());
        assertTrue(speed2.hashCode() == speed2.hashCode());

        Speed speed3 = new Speed(speed.getValue());

        assertTrue(speed.hashCode() == speed3.hashCode());
        assertFalse(speed2.hashCode() == speed3.hashCode());
    }

    @Test
    public void testEquals() {

        Speed speed = new Speed(8191);

        assertFalse(speed.equals(null));

        assertTrue(speed.equals(speed));

        Speed speed2 = new Speed(200);

        assertFalse(speed.equals(new String()));
        assertFalse(speed.equals(speed2));

        speed2.setValue(speed.getValue());
        assertTrue(speed.equals(speed2));
    }
}
