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
 * Unit tests for the merge diverge node angle element.
 * 
 * @author ttevendale
 */
public class MergeDivergeNodeAngleTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(MergeDivergeNodeAngle.MIN);
        assertTrue(MergeDivergeNodeAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new MergeDivergeNodeAngle(MergeDivergeNodeAngle.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(MergeDivergeNodeAngle.MAX);
        assertTrue(MergeDivergeNodeAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle = new MergeDivergeNodeAngle(MergeDivergeNodeAngle.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = -179;
        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testSetValueMin() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle();
        angle.setValue(MergeDivergeNodeAngle.MIN);
        assertTrue(MergeDivergeNodeAngle.MIN == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(MergeDivergeNodeAngle.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle();
        angle.setValue(MergeDivergeNodeAngle.MAX);
        assertTrue(MergeDivergeNodeAngle.MAX == angle.getValue());

        thrown.expect(IllegalArgumentException.class);
        angle.setValue(MergeDivergeNodeAngle.MAX + 1);
    }

    @Test
    public void testSetValue() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle();
        int randomNum = 88;
        angle.setValue(randomNum);
        assertTrue(randomNum == angle.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(MergeDivergeNodeAngle.MIN);
        String encodedAngle = angle.encodeUPER();
        assertTrue("000000000".equals(encodedAngle));

        // test max
        angle = new MergeDivergeNodeAngle(MergeDivergeNodeAngle.MAX);
        encodedAngle = angle.encodeUPER();
        assertTrue("101101000".equals(encodedAngle));

        int randomNumber = 0;
        angle = new MergeDivergeNodeAngle(randomNumber);
        encodedAngle = angle.encodeUPER();
        assertTrue("010110100".equals(encodedAngle));
    }

    @Test
    public void testDecodeUPER() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle();

        // test min
        String remainingBits = angle.decodeUPER("000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(MergeDivergeNodeAngle.MIN == angle.getValue());

        // test max
        remainingBits = angle.decodeUPER("101101000");
        assertTrue("".equals(remainingBits));
        assertTrue(MergeDivergeNodeAngle.MAX == angle.getValue());

        int expectedNumber = -154;
        remainingBits = angle.decodeUPER("000011010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == angle.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle();
        thrown.expect(IllegalArgumentException.class);
        angle.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle();
        String remainingBits = angle.decodeUPER("1010011010");
        assertTrue("0".equals(remainingBits));
        assertTrue(angle.getValue() == 153);
    }

    @Test
    public void testHashCode() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(5);
        MergeDivergeNodeAngle angle2 = new MergeDivergeNodeAngle(10);

        assertFalse(angle.hashCode() == angle2.hashCode());
        assertTrue(angle.hashCode() == angle.hashCode());
        assertTrue(angle2.hashCode() == angle2.hashCode());

        MergeDivergeNodeAngle angle3 = new MergeDivergeNodeAngle(angle.getValue());

        assertTrue(angle.hashCode() == angle3.hashCode());
        assertFalse(angle2.hashCode() == angle3.hashCode());
    }

    @Test
    public void testEquals() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(5);

        assertFalse(angle.equals(null));

        assertTrue(angle.equals(angle));

        MergeDivergeNodeAngle angle2 = new MergeDivergeNodeAngle(angle.getValue() + 5);

        assertFalse(angle.equals(new String()));
        assertFalse(angle.equals(angle2));

        angle2.setValue(angle.getValue());
        assertTrue(angle.equals(angle2));
    }
}
