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
 * Unit tests for the delta angle element.
 * 
 * @author ttevendale
 */
public class DeltaAngleTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        DeltaAngle angle = new DeltaAngle(DeltaAngle.MIN);
        assertTrue(DeltaAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new DeltaAngle(DeltaAngle.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        DeltaAngle angle = new DeltaAngle(DeltaAngle.MAX);
        assertTrue(DeltaAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new DeltaAngle(DeltaAngle.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = -123;
        DeltaAngle angle = new DeltaAngle(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testSetValueMin() {

        DeltaAngle angle = new DeltaAngle();
        angle.setValue(DeltaAngle.MIN);
        assertTrue(DeltaAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(DeltaAngle.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        DeltaAngle angle = new DeltaAngle();
        angle.setValue(DeltaAngle.MAX);
        assertTrue(DeltaAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(DeltaAngle.MAX + 1);
    }

    @Test
    public void testSetValue() {

        DeltaAngle angle = new DeltaAngle();
        int randomNum = 55;
        angle.setValue(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        DeltaAngle angle = new DeltaAngle(DeltaAngle.MIN);
        String encodedAngle = angle.encodeUPER();
        assertTrue("000000000".equals(encodedAngle));

        // test max
        angle = new DeltaAngle(DeltaAngle.MAX);
        encodedAngle = angle.encodeUPER();
        assertTrue("100101100".equals(encodedAngle));

        int randomNumber = 105;
        angle = new DeltaAngle(randomNumber);
        encodedAngle = angle.encodeUPER();
        assertTrue("011111111".equals(encodedAngle));
    }

    @Test
    public void testDecodeUPER() {

        DeltaAngle angle = new DeltaAngle();

        // test min
        String remainingBits = angle.decodeUPER("000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(DeltaAngle.MIN == angle.getValue());

        // test max
        remainingBits = angle.decodeUPER("100101100");
        assertTrue("".equals(remainingBits));
        assertTrue(DeltaAngle.MAX == angle.getValue());

        int expectedNumber = 0;
        remainingBits = angle.decodeUPER("010010110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == angle.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        DeltaAngle angle = new DeltaAngle();
        thrown.expect(IllegalArgumentException.class);
        angle.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        DeltaAngle angle = new DeltaAngle();
        String remainingBits = angle.decodeUPER("0000001010101");
        assertTrue("0101".equals(remainingBits));
        assertTrue(angle.getValue() == -145);
    }

    @Test
    public void testHashCode() {

        DeltaAngle angle = new DeltaAngle(5);
        DeltaAngle angle2 = new DeltaAngle(10);

        assertFalse(angle.hashCode() == angle2.hashCode());
        assertTrue(angle.hashCode() == angle.hashCode());
        assertTrue(angle2.hashCode() == angle2.hashCode());

        DeltaAngle angle3 = new DeltaAngle(angle.getValue());

        assertTrue(angle.hashCode() == angle3.hashCode());
        assertFalse(angle2.hashCode() == angle3.hashCode());
    }

    @Test
    public void testEquals() {

        DeltaAngle angle = new DeltaAngle(5);

        assertFalse(angle.equals(null));

        assertTrue(angle.equals(angle));

        DeltaAngle angle2 = new DeltaAngle(angle.getValue() + 5);

        assertFalse(angle.equals(new String()));
        assertFalse(angle.equals(angle2));

        angle2.setValue(angle.getValue());
        assertTrue(angle.equals(angle2));
    }
}
