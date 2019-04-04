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
 * Unit tests for the roadway crown angle element.
 * 
 * @author ttevendale
 */
public class RoadwayCrownAngleTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(RoadwayCrownAngle.MIN);
        assertTrue(RoadwayCrownAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new RoadwayCrownAngle(RoadwayCrownAngle.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(RoadwayCrownAngle.MAX);
        assertTrue(RoadwayCrownAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new RoadwayCrownAngle(RoadwayCrownAngle.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = -123;
        RoadwayCrownAngle angle = new RoadwayCrownAngle(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testSetValueMin() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle();
        angle.setValue(RoadwayCrownAngle.MIN);
        assertTrue(RoadwayCrownAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(RoadwayCrownAngle.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle();
        angle.setValue(RoadwayCrownAngle.MAX);
        assertTrue(RoadwayCrownAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(RoadwayCrownAngle.MAX + 1);
    }

    @Test
    public void testSetValue() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle();
        int randomNum = 125;
        angle.setValue(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        RoadwayCrownAngle angle = new RoadwayCrownAngle(RoadwayCrownAngle.MIN);
        String encodedAngle = angle.encodeUPER();
        assertTrue("00000000".equals(encodedAngle));

        // test max
        angle = new RoadwayCrownAngle(RoadwayCrownAngle.MAX);
        encodedAngle = angle.encodeUPER();
        assertTrue("11111111".equals(encodedAngle));

        int randomNumber = 0;
        angle = new RoadwayCrownAngle(randomNumber);
        encodedAngle = angle.encodeUPER();
        assertTrue("10000000".equals(encodedAngle));
    }

    @Test
    public void testDecodeUPER() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle();

        // test min
        String remainingBits = angle.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(RoadwayCrownAngle.MIN == angle.getValue());

        // test max
        remainingBits = angle.decodeUPER("11111111");
        assertTrue("".equals(remainingBits));
        assertTrue(RoadwayCrownAngle.MAX == angle.getValue());

        int expectedNumber = -91;
        remainingBits = angle.decodeUPER("00100101");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == angle.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle();
        thrown.expect(IllegalArgumentException.class);
        angle.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle();
        String remainingBits = angle.decodeUPER("101000111101");
        assertTrue("1101".equals(remainingBits));
        assertTrue(angle.getValue() == 35);
    }

    @Test
    public void testHashCode() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(5);
        RoadwayCrownAngle angle2 = new RoadwayCrownAngle(10);

        assertFalse(angle.hashCode() == angle2.hashCode());
        assertTrue(angle.hashCode() == angle.hashCode());
        assertTrue(angle2.hashCode() == angle2.hashCode());

        RoadwayCrownAngle angle3 = new RoadwayCrownAngle(angle.getValue());

        assertTrue(angle.hashCode() == angle3.hashCode());
        assertFalse(angle2.hashCode() == angle3.hashCode());
    }

    @Test
    public void testEquals() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(5);

        assertFalse(angle.equals(null));

        assertTrue(angle.equals(angle));

        RoadwayCrownAngle angle2 = new RoadwayCrownAngle(angle.getValue() + 5);

        assertFalse(angle.equals(new String()));
        assertFalse(angle.equals(angle2));

        angle2.setValue(angle.getValue());
        assertTrue(angle.equals(angle2));
    }
}
