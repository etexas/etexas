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
 * Unit tests for the angle element.
 * 
 * @author ttevendale
 */
public class AngleTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Angle angle = new Angle(Angle.MIN);
        assertTrue(Angle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new Angle(Angle.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Angle angle = new Angle(Angle.MAX);
        assertTrue(Angle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new Angle(Angle.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 12980;
        Angle angle = new Angle(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testSetValueMin() {

        Angle angle = new Angle();
        angle.setValue(Angle.MIN);
        assertTrue(Angle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(Angle.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Angle angle = new Angle();
        angle.setValue(Angle.MAX);
        assertTrue(Angle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(Angle.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Angle angle = new Angle();
        int randomNum = 5330;
        angle.setValue(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Angle angle = new Angle(Angle.MIN);
        String encodedAngle = angle.encodeUPER();
        assertTrue("000000000000000".equals(encodedAngle));

        // test max
        angle = new Angle(Angle.MAX);
        encodedAngle = angle.encodeUPER();
        assertTrue("111000010000000".equals(encodedAngle));

        int randomNumber = 4415;
        angle = new Angle(randomNumber);
        encodedAngle = angle.encodeUPER();
        assertTrue("001000100111111".equals(encodedAngle));
    }

    @Test
    public void testDecodeUPER() {

        Angle angle = new Angle();

        // test min
        String remainingBits = angle.decodeUPER("000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Angle.MIN == angle.getValue());

        // test max
        remainingBits = angle.decodeUPER("111000010000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Angle.MAX == angle.getValue());

        int expectedNumber = 1234;
        remainingBits = angle.decodeUPER("000010011010010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == angle.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Angle angle = new Angle();
        thrown.expect(IllegalArgumentException.class);
        angle.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Angle angle = new Angle();
        String remainingBits = angle.decodeUPER("00000001101010101");
        assertTrue("01".equals(remainingBits));
        assertTrue(angle.getValue() == 213);
    }

    @Test
    public void testHashCode() {

        Angle angle = new Angle(5);
        Angle angle2 = new Angle(10);

        assertFalse(angle.hashCode() == angle2.hashCode());
        assertTrue(angle.hashCode() == angle.hashCode());
        assertTrue(angle2.hashCode() == angle2.hashCode());

        Angle angle3 = new Angle(angle.getValue());

        assertTrue(angle.hashCode() == angle3.hashCode());
        assertFalse(angle2.hashCode() == angle3.hashCode());
    }

    @Test
    public void testEquals() {

        Angle angle = new Angle(5);

        assertFalse(angle.equals(null));

        assertTrue(angle.equals(angle));

        Angle angle2 = new Angle(angle.getValue() + 5);

        assertFalse(angle.equals(new String()));
        assertFalse(angle.equals(angle2));

        angle2.setValue(angle.getValue());
        assertTrue(angle.equals(angle2));
    }
}
