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
 * Unit tests for the steering wheel angle element.
 * 
 * @author ttevendale
 */
public class SteeringWheelAngleTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SteeringWheelAngle angle = new SteeringWheelAngle(SteeringWheelAngle.MIN);
        assertTrue(SteeringWheelAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new SteeringWheelAngle(SteeringWheelAngle.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        SteeringWheelAngle angle = new SteeringWheelAngle(SteeringWheelAngle.MAX);
        assertTrue(SteeringWheelAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new SteeringWheelAngle(SteeringWheelAngle.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = -50;
        SteeringWheelAngle angle = new SteeringWheelAngle(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testSetValueMin() {

        SteeringWheelAngle angle = new SteeringWheelAngle();
        angle.setValue(SteeringWheelAngle.MIN);
        assertTrue(SteeringWheelAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(SteeringWheelAngle.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        SteeringWheelAngle angle = new SteeringWheelAngle();
        angle.setValue(SteeringWheelAngle.MAX);
        assertTrue(SteeringWheelAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(SteeringWheelAngle.MAX + 1);
    }

    @Test
    public void testSetValue() {

        SteeringWheelAngle angle = new SteeringWheelAngle();
        int randomNum = -120;
        angle.setValue(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SteeringWheelAngle angle = new SteeringWheelAngle(SteeringWheelAngle.MIN);
        String encodedAngle = angle.encodeUPER();
        assertTrue("00000000".equals(encodedAngle));

        // test max
        angle = new SteeringWheelAngle(SteeringWheelAngle.MAX);
        encodedAngle = angle.encodeUPER();
        assertTrue("11111101".equals(encodedAngle));

        int randomNumber = 20;
        angle = new SteeringWheelAngle(randomNumber);
        encodedAngle = angle.encodeUPER();
        assertTrue("10010010".equals(encodedAngle));
    }

    @Test
    public void testDecodeUPER() {

        SteeringWheelAngle angle = new SteeringWheelAngle();

        // test min
        String remainingBits = angle.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(SteeringWheelAngle.MIN == angle.getValue());

        // test max
        remainingBits = angle.decodeUPER("11111101");
        assertTrue("".equals(remainingBits));
        assertTrue(SteeringWheelAngle.MAX == angle.getValue());

        int expectedNumber = 0;
        remainingBits = angle.decodeUPER("01111110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == angle.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        SteeringWheelAngle angle = new SteeringWheelAngle();
        thrown.expect(IllegalArgumentException.class);
        angle.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SteeringWheelAngle angle = new SteeringWheelAngle();
        String remainingBits = angle.decodeUPER("000001000");
        assertTrue("0".equals(remainingBits));
        assertTrue(angle.getValue() == -122);
    }

    @Test
    public void testHashCode() {

        SteeringWheelAngle angle = new SteeringWheelAngle(-50);
        SteeringWheelAngle angle2 = new SteeringWheelAngle(50);

        assertFalse(angle.hashCode() == angle2.hashCode());
        assertTrue(angle.hashCode() == angle.hashCode());
        assertTrue(angle2.hashCode() == angle2.hashCode());

        SteeringWheelAngle angle3 = new SteeringWheelAngle(angle.getValue());

        assertTrue(angle.hashCode() == angle3.hashCode());
        assertFalse(angle2.hashCode() == angle3.hashCode());
    }

    @Test
    public void testEquals() {

        SteeringWheelAngle angle = new SteeringWheelAngle(-126);

        assertFalse(angle.equals(null));

        assertTrue(angle.equals(angle));

        SteeringWheelAngle angle2 = new SteeringWheelAngle(50);

        assertFalse(angle.equals(new String()));
        assertFalse(angle.equals(angle2));

        angle2.setValue(angle.getValue());
        assertTrue(angle.equals(angle2));
    }
}
